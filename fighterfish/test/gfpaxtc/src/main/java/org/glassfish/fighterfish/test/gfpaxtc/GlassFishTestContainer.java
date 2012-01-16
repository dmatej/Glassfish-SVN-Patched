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

import org.glassfish.embeddable.*;
import org.ops4j.pax.exam.*;
import org.ops4j.pax.exam.TimeoutException;
import org.ops4j.pax.exam.options.FrameworkStartLevelOption;
import org.ops4j.pax.exam.options.ProvisionOption;
import org.ops4j.pax.exam.options.SystemPropertyOption;
import org.ops4j.pax.exam.options.UrlProvisionOption;
import org.osgi.framework.*;
import org.osgi.framework.launch.Framework;
import org.osgi.service.packageadmin.PackageAdmin;
import org.osgi.service.startlevel.StartLevel;
import org.osgi.util.tracker.ServiceTracker;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class GlassFishTestContainer implements TestContainer {
    private ConfigurationHelper configurationHelper;
    private BootstrapProperties bsProperties;
    private GlassFishProperties gfProps;

    private final ExamSystem system;
    private Framework framework;
    private GlassFishRuntime gfr;
    private GlassFish gf;
    private Stack<Long> m_installed = new Stack<Long>();
    private Logger logger = Logger.getLogger(getClass().getPackage().getName());
    final private static String PROBE_SIGNATURE_KEY = "Probe-Signature";

    public GlassFishTestContainer(ExamSystem system) {
        System.setProperty("java.protocol.handler.pkgs", "org.ops4j.pax.url");
        this.system = system;
        configurationHelper = new ConfigurationHelper(system);
    }

    public TestContainer start() throws TimeoutException {
        try {
            bootstrapGf();
            // Let's get hold of the framework
            PackageAdmin pa = gf.getService(PackageAdmin.class);
            framework = (Framework) pa.getBundle(Bundle.class);
            logger.logp(Level.INFO, "GlassFishTestContainer", "start", "framework = {0}", new Object[]{framework});
            installAndStartBundles();
        } catch (GlassFishException e) {
            throw new RuntimeException(e);
        } catch (BundleException e) {
            throw new RuntimeException(e);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e); // TODO(Sahoo): Proper Exception Handling
        }
        return this;
    }

    public long install(InputStream stream) {
        return install("local", stream);
    }

    @Override
    public long install(String location, InputStream stream) {
        try {
            Bundle b = framework.getBundleContext().installBundle(location, stream);
            addBundle(b);
            setBundleStartLevel(b.getBundleId(), org.ops4j.pax.exam.Constants.START_LEVEL_TEST_BUNDLE);
            b.start();
            logger.logp(Level.INFO, "GlassFishTestContainer", "install", "Installed pax exam probe: {0}", new Object[]{b});
            return b.getBundleId();
        } catch (BundleException e) {
            throw new RuntimeException(e);
        }
    }

    private void addBundle(Bundle b) {
        if(!m_installed.contains(b.getBundleId())) {
            m_installed.push(b.getBundleId());
        }
    }

    public synchronized void call(TestAddress address) {
        String filter = "(" + PROBE_SIGNATURE_KEY + "=" + address.root().identifier() + ")";
        ProbeInvoker service = getService(ProbeInvoker.class, filter, system.getTimeout().getValue());
        service.call(address.arguments());
    }


    public TestContainer stop() {
        ExecutorService executorService = Executors.newSingleThreadExecutor();
        Future<Boolean> result = executorService.submit(new Callable<Boolean>() {
            @Override
            public Boolean call() {
                try {
                    uninstallAll();
                    shutdownGf();
                } catch (GlassFishException e) {
                    e.printStackTrace();
                    return false;
                } finally {
                    system.clear();
                }
                return true;
            }
        });
        try {
            executorService.shutdown();
            executorService.awaitTermination(
                    system.getTimeout().getUpperValue() + 1000, TimeUnit.MILLISECONDS);
            if (result.get()!=null && result.get()) {
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

    private void bootstrapGf() throws GlassFishException, MalformedURLException {
        bootstrapGfr();
        gfProps = new GlassFishProperties(bsProperties.getProperties());
        gf = gfr.newGlassFish(gfProps);
        gf.start();
    }

    private void bootstrapGfr() throws MalformedURLException, GlassFishException {
        ClassLoader launcherCL = createGFLauncherCL();
        bsProperties = getBootstrapProperties();
        logger.logp(Level.FINE, "GFTC", "bootstrapGfr", "GlassFishRuntime BootstrapProperties = {0}", new Object[]{bsProperties.getProperties()});
        gfr = GlassFishRuntime.bootstrap(bsProperties, launcherCL);
        logger.logp(Level.INFO, "GFTC", "bootstrapGfr",
                "gfr = {0} and is loaded by {1}", new Object[]{gfr, gfr.getClass().getClassLoader()});
    }

    private void shutdownGf() throws GlassFishException {
        gf.dispose(); // dispose will remove it from service registry as well
        gf = null;
        gfr.shutdown();
        gfr = null;
    }

    private <T> T getService(Class<T> serviceType, String filter, long timeoutInMillis) throws TestContainerException {
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

    private int getStartLevel(ProvisionOption<?> bundle) {
        Integer start = bundle.getStartLevel();
        if (start == null) {
            start = org.ops4j.pax.exam.Constants.START_LEVEL_DEFAULT_PROVISION;
        }
        return start;
    }

    private void setBundleStartLevel(long bundleId, int startLevel) throws TestContainerException {
        getStartLevelService().setBundleStartLevel(framework.getBundleContext().getBundle(bundleId), startLevel);
    }

    private StartLevel getStartLevelService() {
        return (StartLevel) framework.getBundleContext().getService(
                framework.getBundleContext().getServiceReference(StartLevel.class.getName()));
    }

    private void installAndStartBundles() throws BundleException {
        BundleContext context = framework.getBundleContext();
        final ProvisionOption[] options = system.getOptions(ProvisionOption.class);
        for (ProvisionOption<?> option : options) {
            final String url = option.getURL();
            logger.logp(Level.INFO, "GlassFishTestContainer", "installAndStartBundles", "Installing bundle from {0}", new Object[]{url});
            Bundle b = context.installBundle(url);
            logger.logp(Level.INFO, "GlassFishTestContainer", "installAndStartBundles", "Installed bundle {0}", new Object[]{b});
            addBundle(b);
        }
        for (ProvisionOption<?> option : options) {
            int startLevel = getStartLevel(option);
            Bundle b = getBundle(option.getURL());
            setBundleStartLevel(b.getBundleId(), startLevel);
            if (option.shouldStart()) {
                b.start();
                logger.logp(Level.INFO, "GlassFishTestContainer", "installAndStartBundles", "Install (start@{0}) {1}", new Object[]{startLevel, option});
            } else {
                logger.logp(Level.INFO, "GlassFishTestContainer", "installAndStartBundles", "Install (no start) {0}", new Object[]{option});
            }
        }
        int startLevel = system.getSingleOption(FrameworkStartLevelOption.class).getStartLevel();
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
            final long timeout = system.getTimeout().getLowerValue();
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

    private Bundle getBundle(String url) {
        for (Bundle b : framework.getBundleContext().getBundles()) {
            if (url.equals(b.getLocation())) {
                return b;
            }
        }
        return null;
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

    private BootstrapProperties getBootstrapProperties() {
        Properties props = readOSGiFWConfiguration();
        // override by what's specified in system and options
        props.putAll(configurationHelper.getProperties());
        Util.substVars(props); // variable substitution
        augmentBootDelegation(props);
        return new BootstrapProperties(props);
    }

    private ClassLoader createGFLauncherCL() throws MalformedURLException {
        ClassLoader osgiFWLauncherCL = createOSGiFWLauncherCL();
        List<URL> cp = new ArrayList<URL>();
        cp.add(new File(new File(configurationHelper.getGFHome(), "modules"), "glassfish.jar").toURI().toURL());
        return new URLClassLoader(cp.toArray(new URL[cp.size()]), osgiFWLauncherCL);
    }

    private ClassLoader createOSGiFWLauncherCL() throws MalformedURLException {
        List<URL> cp = new ArrayList<URL>();
        for (File jar : getOSGiFWJars()) {
            cp.add(jar.toURI().toURL());
        }
        return new URLClassLoader(cp.toArray(new URL[cp.size()]), getClass().getClassLoader());
    }

    private File[] getOSGiFWJars() {
        String fwName = configurationHelper.getOSGiFW();
        if ("Felix".equals(fwName)) {
            return new File[]{new File(getOSGiFWDir(fwName), "bin/felix.jar")};
        } else if ("Equinox".equals(fwName)) {
            return getOSGiFWDir(fwName).listFiles(new FileFilter() {
                public boolean accept(File pathname) {
                    return (pathname.getName().endsWith(".jar") && pathname.isFile());
                }
            });
        }
        throw new IllegalArgumentException("Unknown platform " + fwName);
    }

    private File getOSGiFWDir(String fwName) {
        return new File(new File(configurationHelper.getGFHome(), "osgi/"), fwName.toLowerCase());
    }

    private void augmentBootDelegation(Properties props) {
        // Need to add this, else when JunitRunner calls getService (ProbeInvoker)
        // we can't cast the service registered by pax-exam-extender
        String bootdelegation = props.getProperty(org.osgi.framework.Constants.FRAMEWORK_BOOTDELEGATION);
        final String paxBootDelegation = "org.ops4j.pax.exam, org.ops4j.pax.exam.*, org.junit, org.junit.*";
        if (bootdelegation == null) {
            bootdelegation = paxBootDelegation;
        } else {
            bootdelegation = bootdelegation + ", " + paxBootDelegation;
        }
        props.setProperty(org.osgi.framework.Constants.FRAMEWORK_BOOTDELEGATION, bootdelegation);
    }

    private Properties readOSGiFWConfiguration() {
        File propertiesFile = new File(configurationHelper.getGFHome(), "config/osgi.properties");
        Properties props = new Properties();
        try {
            InputStream is = new FileInputStream(propertiesFile);
            try {
                props.load(is);
            } finally {
                is.close();
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return props;
    }

    static class ConfigurationHelper {
        private Properties props = new Properties();
        private ExamSystem examSystem;
        private String provisioningUrl;
        private File installDir;
        private boolean install;
        private File gfHome;

        private Logger logger = Logger.getLogger(ConfigurationHelper.class.getPackage().getName());
        private static final String PROVISIONER_URL = "fighterfish.provisioner.url";
        private static final String PROVISIONER_URL_DEFAULT_VALUE = "mvn:org.glassfish.distributions/glassfish/3.1.1/zip";
        private static final String INSTALL_ROOT = "com.sun.aas.installRoot";
        private static final String INSTALL_ROOT_URI = "com.sun.aas.installRootURI";
        private static final String INSTANCE_ROOT = "com.sun.aas.instanceRoot";
        private static final String DOMAIN_DIR = "domains/domain1/";
        private static final String INSTANCE_ROOT_URI = "com.sun.aas.instanceRootURI";
        private static final String PLATFORM = "GlassFish_Platform";
        private static final String FELIX = "Felix";

        public ConfigurationHelper(ExamSystem examSystem) {
            this.examSystem = examSystem;
            parseSystemPropertyOptions();
            determineGlassFishHome();
            install();
            logger.logp(Level.INFO, "GlassFishTestContainer$PropertyHelper", "PropertyHelper", "props = {0}", new Object[]{props});
        }

        private void parseSystemPropertyOptions() {
            for (SystemPropertyOption spo : examSystem.getOptions(SystemPropertyOption.class)) {
                props.setProperty(spo.getKey(), spo.getValue());
            }
        }

        Properties getProperties() {
            return props;
        }

        private void determineGlassFishHome() {
            String property = getProperty(INSTALL_ROOT);
            if (property != null && !property.isEmpty()) {
                gfHome = new File(property);
            }
            property = getProperty(PROVISIONER_URL);
            if (property != null && !property.isEmpty()) {
                provisioningUrl = property;
            }
            if (gfHome == null) {
                if (provisioningUrl == null) {
                    // both are unspecified
                    provisioningUrl = PROVISIONER_URL_DEFAULT_VALUE;
                }
                // We compute a hashcode so that if user changes provisioning url, we are less likely to
                // reuse the earlier created installation.
                installDir = new File(System.getProperty("java.io.tmpdir"), "fighterfish-" + provisioningUrl.hashCode());
                gfHome = new File(installDir, "glassfish3/glassfish/");
                install = !installDir.exists();
            } else {
                // gfHome is specified
                if (!gfHome.exists()) {
                    // explode only if provisioning url is explicitly specified
                    install = provisioningUrl != null;
                    try {
                        installDir = new File(gfHome, "../../").getCanonicalFile();
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
            props.setProperty(INSTALL_ROOT, gfHome.getAbsolutePath());
            property = gfHome.toURI().toString();
            if (!property.endsWith("/")) {
                property = property + "/"; // GlassFish osgi.properties expect a '/'
            }
            props.setProperty(INSTALL_ROOT_URI, property);

            property = getProperty(INSTANCE_ROOT);
            if (property == null) {
                property = new File(gfHome, DOMAIN_DIR).getAbsolutePath();
                props.setProperty(INSTANCE_ROOT, property);
            }
            props.setProperty(INSTANCE_ROOT_URI, new File(property).toURI().toString());
            property = getProperty(PLATFORM);
            if (property == null) {
                property = FELIX;
            }
            props.setProperty(PLATFORM, property);
        }

        private void install() {
            if (install) {
                if (installDir.exists()) {
                    logger.logp(Level.INFO, "TestsConfiguration", "setup",
                            "Reusing existing installation at {0}", new Object[]{installDir});
                } else {
                    logger.logp(Level.INFO, "ConfigurationHelper", "install",
                            "Will install {0} at {1}", new Object[]{provisioningUrl, installDir});
                    explode(provisioningUrl, installDir);
                }
            }
            verifyInstallation();
        }

        private void verifyInstallation() {
            final File file = new File(gfHome, "modules/glassfish.jar");
            if (!file.exists()) {
                throw new RuntimeException(file.getAbsolutePath() + " does not exist.");
            }
        }

        private void explode(String provisioningUrl, File out) {
            try {
                ZipUtil.explode(URI.create(provisioningUrl), out);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        private String getProperty(String s) {
            return props.getProperty(s, System.getProperty(s));
        }

        private File getGFHome() {
            return gfHome;
        }

        private String getOSGiFW() {
            return props.getProperty(PLATFORM);
        }
    }
}
