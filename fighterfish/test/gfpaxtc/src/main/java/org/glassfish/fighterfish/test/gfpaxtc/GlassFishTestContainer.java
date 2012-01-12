/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2011-2012 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */


package org.glassfish.fighterfish.test.gfpaxtc;

import org.glassfish.embeddable.GlassFish;
import org.glassfish.embeddable.GlassFishException;
import org.glassfish.embeddable.GlassFishProperties;
import org.glassfish.embeddable.GlassFishRuntime;
import org.ops4j.pax.exam.Constants;
import org.ops4j.pax.exam.*;
import org.ops4j.pax.exam.TimeoutException;
import org.ops4j.pax.exam.options.FrameworkStartLevelOption;
import org.ops4j.pax.exam.options.ProvisionOption;
import org.osgi.framework.*;
import org.osgi.framework.launch.Framework;
import org.osgi.service.packageadmin.PackageAdmin;
import org.osgi.service.startlevel.StartLevel;
import org.osgi.util.tracker.ServiceTracker;

import java.io.InputStream;
import java.util.Stack;
import java.util.concurrent.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class GlassFishTestContainer implements TestContainer {
    private final ExamSystem m_system;
    private Framework framework;
    private GlassFishRuntime gfr;
    private GlassFish gf;
    private Stack<Long> m_installed = new Stack<Long>();
    private Logger logger = Logger.getLogger(getClass().getPackage().getName());
    final private static String PROBE_SIGNATURE_KEY = "Probe-Signature";
    private final GlassFishProperties gfProps;

    public GlassFishTestContainer(GlassFishRuntime gfr, GlassFishProperties gfProps, ExamSystem msystem) {
        this.gfProps = gfProps;
        logger.logp(Level.INFO, "GlassFishTestContainer", "GlassFishTestContainer",
                "gfr = {0} and is loaded by {1}", new Object[]{gfr, gfr.getClass().getClassLoader()});
        this.m_system = msystem;
        this.gfr = gfr;
    }

    public TestContainer start() throws TimeoutException {
        try {
            gf = gfr.newGlassFish(gfProps);
            gf.start();
            // Let's get hold of the framework
            PackageAdmin pa = gf.getService(PackageAdmin.class);
            framework = (Framework) pa.getBundle(Bundle.class);
            logger.logp(Level.INFO, "GlassFishTestContainer", "start", "framework = {0}", new Object[]{framework});
            installAndStartBundles();
        } catch (GlassFishException e) {
            throw new RuntimeException(e);
        } catch (BundleException e) {
            throw new RuntimeException(e);
        }
        return this;
    }

    public <T> T getService(Class<T> serviceType, String filter, long timeoutInMillis) throws TestContainerException {
        final String objectClassFilter = "(objectclass=" + serviceType.getName() + ")";
        filter = "(&" + objectClassFilter + filter + ")";
        Filter filter1 = null;
        try {
            filter1 = framework.getBundleContext().createFilter(filter);
        } catch (InvalidSyntaxException e) {
            throw new TestContainerException(e);
        }
        ServiceTracker st = new ServiceTracker(framework.getBundleContext(), filter1, null);
        st.open(false);
        try {
            return serviceType.cast(st.waitForService(timeoutInMillis));
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new TimeoutException(e);
        } finally {
            st.close();
        }
    }

    public void setBundleStartLevel(long bundleId, int startLevel) throws TestContainerException {
        getStartLevelService().setBundleStartLevel(framework.getBundleContext().getBundle(bundleId), startLevel);
    }

    private StartLevel getStartLevelService() {
        return (StartLevel) framework.getBundleContext().getService(
                framework.getBundleContext().getServiceReference(StartLevel.class.getName()));
    }

    public long install(InputStream stream) {
        return install("local", stream);
    }

    @Override
    public long install(String location, InputStream stream) {
        try {
            Bundle b = framework.getBundleContext().installBundle(location, stream);
            m_installed.push(b.getBundleId());
            setBundleStartLevel(b.getBundleId(), Constants.START_LEVEL_TEST_BUNDLE);
            b.start();
            logger.logp(Level.INFO, "GlassFishTestContainer", "install", "Installed pax exam probe: {0}", new Object[]{b});
            return b.getBundleId();
        } catch (BundleException e) {
            throw new RuntimeException(e);
        }
    }

    public synchronized void call(TestAddress address) {
        String filter = "(" + PROBE_SIGNATURE_KEY + "=" + address.root().identifier() + ")";
        ProbeInvoker service = getService(ProbeInvoker.class, filter, m_system.getTimeout().getValue());
        service.call(address.arguments());
    }


    public TestContainer stop() {
        ExecutorService executorService = Executors.newSingleThreadExecutor();
        Future<Boolean> result = executorService.submit(new Callable<Boolean>() {
            @Override
            public Boolean call() {
                try {
                    gf.stop();
                    uninstallAll();
                    gfr.shutdown();
                } catch (GlassFishException e) {
                    e.printStackTrace();
                    return false;
                } finally {
                    m_system.clear();
                }
                return true;
            }
        });
        try {
            executorService.shutdown();
            executorService.awaitTermination(
                    m_system.getTimeout().getUpperValue() + 1000, TimeUnit.MILLISECONDS);
            if (result.get()) {
                logger.logp(Level.INFO, "GlassFishTestContainer", "stop", "Test container stopped successfully");
            } else {
                logger.logp(Level.INFO, "GlassFishTestContainer", "stop", "Test container did not stop successfully");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException(e);
        } catch (ExecutionException e) {
            throw new RuntimeException(e);
        }
        return this;
    }

    private synchronized void uninstallAll() {
        while ((!m_installed.isEmpty())) {
            try {
                Long id = m_installed.pop();
                Bundle bundle = framework.getBundleContext().getBundle(id);
                bundle.uninstall();
                logger.logp(Level.INFO, "GlassFishTestContainer", "uninstallAll", "Uninstalled {0}", new Object[]{bundle});
            } catch (BundleException e) {
                // Sometimes bundles go mad when install + uninstall happens too
                // fast.
            }
        }
    }

    private void installAndStartBundles() throws BundleException {
        BundleContext context = framework.getBundleContext();
        for (ProvisionOption<?> bundle : m_system.getOptions(ProvisionOption.class)) {
            Bundle b = context.installBundle(bundle.getURL());
            m_installed.push(b.getBundleId());
            int startLevel = getStartLevel(bundle);
            setBundleStartLevel(b.getBundleId(), startLevel);
            if (bundle.shouldStart()) {
                b.start();
                logger.logp(Level.INFO, "GlassFishTestContainer", "installAndStartBundles", "Install (start@{0}) {1}", new Object[]{startLevel, bundle});
            } else {
                logger.logp(Level.INFO, "GlassFishTestContainer", "installAndStartBundles", "Install (no start) {0}", new Object[]{bundle});
            }
        }

        int startLevel = m_system.getSingleOption(FrameworkStartLevelOption.class).getStartLevel();
        logger.logp(Level.INFO, "GlassFishTestContainer", "installAndStartBundles", "Jump to startlevel: " + startLevel);
        getStartLevelService().setStartLevel(startLevel);
        // Work around for FELIX-2942
        final CountDownLatch latch = new CountDownLatch(1);
        context.addFrameworkListener(new FrameworkListener() {
            public void frameworkEvent(FrameworkEvent frameworkEvent) {
                switch (frameworkEvent.getType()) {
                    case FrameworkEvent.STARTLEVEL_CHANGED:
                        latch.countDown();
                }
            }
        });
        try {
            final long timeout = m_system.getTimeout().getLowerValue();
            if (!latch.await(timeout, TimeUnit.MILLISECONDS)) {
                // Framework start level has not reached yet, so report an error to cause the test process to abort
                final String message = "Framework is yet to reach target start level " + startLevel + " after " +
                        timeout + " ms. Current start level is " + getStartLevelService().getStartLevel();
                throw new TestContainerException(message);
            }
        } catch (InterruptedException e) {
            throw new TestContainerException(e);
        }
    }

    private int getStartLevel(ProvisionOption<?> bundle) {
        Integer start = bundle.getStartLevel();
        if (start == null) {
            start = Constants.START_LEVEL_DEFAULT_PROVISION;
        }
        return start;
    }


}
