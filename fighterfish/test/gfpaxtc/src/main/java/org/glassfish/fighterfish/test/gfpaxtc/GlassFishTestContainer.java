/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
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
import org.ops4j.pax.exam.*;
import org.ops4j.pax.exam.options.ProvisionOption;
import org.osgi.framework.*;
import org.osgi.service.packageadmin.PackageAdmin;
import org.osgi.service.startlevel.StartLevel;
import org.osgi.util.tracker.BundleTracker;
import org.osgi.util.tracker.ServiceTracker;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.ops4j.pax.exam.Constants.START_LEVEL_SYSTEM_BUNDLES;
import static org.ops4j.pax.exam.CoreOptions.bootDelegationPackage;
import static org.ops4j.pax.exam.CoreOptions.mavenBundle;
import static org.ops4j.pax.exam.LibraryOptions.junitBundles;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class GlassFishTestContainer implements TestContainer {
    BundleContext bctx;
    private GlassFish gf;
    private Bundle paxExamProbeBundle;
    private List<Bundle> paxBundles = new ArrayList<Bundle>();
    private Logger logger = Logger.getLogger(getClass().getPackage().getName());

    public GlassFishTestContainer(GlassFish gf) {
        logger.logp(Level.INFO, "GlassFishTestContainer", "GlassFishTestContainer", "gf = {0} and is loaded by {1}", new Object[]{gf, gf.getClass().getClassLoader()});
        this.gf = gf;

        // This returns null, so we have to retrieve BundleContext from getDeployer() in start()
        // bctx = BundleReference.class.cast(gf.getClass().getClassLoader()).getBundle().getBundleContext();
        // logger.logp(Level.INFO, "GlassFishTestContainer", "GlassFishTestContainer", "gf = {0}", new Object[]{gf});
    }

    public void setBundleStartLevel(long bundleId, int startLevel) throws TestContainerException {
        getStartLevelService().setBundleStartLevel(bctx.getBundle(bundleId), startLevel);
    }

    private StartLevel getStartLevelService() {
        return (StartLevel) bctx.getService(bctx.getServiceReference(StartLevel.class.getName()));
    }

    public TestContainer start() throws TimeoutException {
        try {
            gf.start();
            // Let's get hold of Framework BundleContext
            PackageAdmin pa = gf.getService(PackageAdmin.class);
            bctx = pa.getBundle(Bundle.class).getBundleContext();
            logger.logp(Level.INFO, "GlassFishTestContainer", "start", "bctx = {0}", new Object[]{bctx});
            provisionPaxExamBundles();
        } catch (GlassFishException e) {
            throw new RuntimeException(e);
        } catch (BundleException e) {
            throw new RuntimeException(e);
        }
        return this;
    }

    public TestContainer stop() throws TimeoutException {
        try {
            gf.stop();
        } catch (GlassFishException e) {
            throw new RuntimeException(e);
        }
        return this;
    }

    public void waitForState(final long bundleId, int state, long timeoutInMillis) throws TimeoutException {
        final CountDownLatch latch = new CountDownLatch(1);
        BundleTracker bt = new BundleTracker(bctx, state, null) {
            @Override
            public Object addingBundle(Bundle bundle, BundleEvent event) {
                if (bundle.getBundleId() == bundleId) {
                    latch.countDown();
                }
                return null;
            }
        };
        bt.open();
        try {
            latch.await(timeoutInMillis, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new TimeoutException(e);
        } finally {
            bt.close();
        }
    }

    public <T> T getService(Class<T> serviceType, String filter, long timeoutInMillis) throws TestContainerException {
        ServiceTracker st = new ServiceTracker(bctx, serviceType.getName(), null);
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

    public long install(InputStream stream) {
        try {
            paxExamProbeBundle = bctx.installBundle("pax-exam-probe", stream);
            paxExamProbeBundle.start(Bundle.START_TRANSIENT);
        } catch (BundleException e) {
            throw new RuntimeException(e);
        }
        logger.logp(Level.INFO, "GlassFishTestContainer", "install", "Installed pax exam probe: {0}", new Object[]{paxExamProbeBundle});
        return paxExamProbeBundle.getBundleId();
    }

    public void cleanup() {
        try {
            paxExamProbeBundle.uninstall();
            for (Bundle paxBundle : paxBundles) {
                paxBundle.uninstall();
            }
            logger.logp(Level.INFO, "GlassFishTestContainer", "cleanup", "Uninstalled probe bundle and pax bundles");
        } catch (BundleException e) {
            throw new RuntimeException(e);
        }
    }

    private void provisionPaxExamBundles() throws BundleException {
        ProvisionOption[] options = {
            mavenBundle()
                .groupId( "org.ops4j.pax.logging" )
                .artifactId( "pax-logging-api" )
                .version( "1.4" )
                .startLevel( START_LEVEL_SYSTEM_BUNDLES ),
            mavenBundle()
                .groupId( "org.ops4j.pax.exam" )
                .artifactId( "pax-exam-extender-service" )
                .version( Info.getPaxExamVersion() )
                .update( Info.isPaxExamSnapshotVersion() )
                .startLevel( START_LEVEL_SYSTEM_BUNDLES )
        };

        for (ProvisionOption option : options) {
            Bundle bundle = bctx.installBundle(option.getURL());
            paxBundles.add(bundle);
            logger.logp(Level.INFO, "GlassFishTestContainer", "provisionPaxExamBundles", "bundle = {0}", new Object[]{bundle});
        }

        for (Bundle b : paxBundles) {
            b.start(Bundle.START_TRANSIENT);
        }
    }

}
