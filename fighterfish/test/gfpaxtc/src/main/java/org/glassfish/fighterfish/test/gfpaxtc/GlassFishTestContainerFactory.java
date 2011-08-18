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

import org.glassfish.embeddable.*;
import org.ops4j.pax.exam.*;
import org.ops4j.pax.exam.options.ProvisionOption;
import org.ops4j.pax.exam.options.SystemPropertyOption;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class GlassFishTestContainerFactory implements TestContainerFactory {

    private GlassFishRuntime gfr;
    private Logger logger = Logger.getLogger(getClass().getPackage().getName());
    private OptionsParser parser;

    public TestContainer[] parse(Option... options) throws TestContainerException {
        logger.logp(Level.INFO, "GFTCF", "parse", "options = {0}", new Object[]{options});
        System.setProperty("java.protocol.handler.pkgs", "org.ops4j.pax.url");
        parser = new OptionsParser(options);
        String[] osgiFWs = parser.getOSGiFWs();
        TestContainer[] tcs = new TestContainer[osgiFWs.length];
        for (int i = 0; i < osgiFWs.length; i++) {
            tcs[i] = createTestContainer(osgiFWs[i]);
        }
        return tcs;
    }

    private TestContainer createTestContainer(String fwName) {
        logger.logp(Level.INFO, "GFTCF", "createTestContainer", "fwName = {0}", new Object[]{fwName});
        try {
            ClassLoader launcherCL = getClass().getClassLoader();// createGFLauncherCL(fwName);
            BootstrapProperties bsProperties = getBootstrapProperties(fwName);
            logger.logp(Level.INFO, "GFTCF", "createTestContainer", "GlassFishRuntime BootstrapProperties = {0}", new Object[]{bsProperties.getProperties()});
            gfr = GlassFishRuntime.bootstrap(bsProperties, launcherCL);
            GlassFishProperties gfProps = new GlassFishProperties(bsProperties.getProperties());
            GlassFish gf = gfr.newGlassFish(gfProps);
            return new GlassFishTestContainer(gf);
        } catch (GlassFishException e) {
            throw new RuntimeException(e); // TODO(Sahoo): Proper Exception Handling
        }
    }

    private BootstrapProperties getBootstrapProperties(String fwName) {
        Properties props = parser.getSystemProperties();
        props.putAll(getOSGiFWConfiguration(fwName));
        Util.substVars(props);
//        props.setProperty("GlassFish_Platform", fwName);
        return new BootstrapProperties(props);
    }

    private ClassLoader createGFLauncherCL(String fwName) throws MalformedURLException {
        ClassLoader osgiFWLauncherCL = createOSGiFWLauncherCL(fwName);
        List<URL> cp = new ArrayList<URL>();
        cp.add(new File(new File(parser.getGFHome(), "modules"), "glassfish.jar").toURI().toURL());
        return new URLClassLoader(cp.toArray(new URL[0]), osgiFWLauncherCL);
    }

    private ClassLoader createOSGiFWLauncherCL(String fwName) throws MalformedURLException {
        List<URL> cp = new ArrayList<URL>();
        for (File jar : getOSGiFWJars(fwName)) {
            cp.add(jar.toURI().toURL());
        }
        URLClassLoader osgiFWLauncherCL = new URLClassLoader(cp.toArray(new URL[cp.size()]), getClass().getClassLoader());
        return osgiFWLauncherCL;
    }

    private File[] getOSGiFWJars(String fwName) {
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
        return new File(new File(parser.getGFHome(), "osgi/"), fwName.toLowerCase());
    }

    private Properties getOSGiFWConfiguration(String fwName) {
        File propertiesFile;
        if ("Felix".equals(fwName)) {
            propertiesFile = new File(getOSGiFWDir(fwName), "conf/config.properties");
        } else if ("Equinox".equals(fwName)) {
            propertiesFile = new File(getOSGiFWDir(fwName), "configuration/config.ini");
        } else {
            throw new IllegalArgumentException("Unknown platform " + fwName);
        }
        Properties props = new Properties();
        try {
            InputStream is = new FileInputStream(propertiesFile);
            try {
                props.load(is);
            } finally {
                is.close();
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e); // TODO(Sahoo): Proper Exception Handling
        } catch (IOException e) {
            throw new RuntimeException(e); // TODO(Sahoo): Proper Exception Handling
        }
        // Need to add this, else when JunitRunner calls getService (ProbeInvoker)
        // we can't cast the service registered by pax-exam-extender
        String bootdelegation = props.getProperty(org.osgi.framework.Constants.FRAMEWORK_BOOTDELEGATION);
        final String paxBootDelegation = "org.ops4j.pax.exam.*, org.junit, org.junit.*";
        if (bootdelegation == null) {
            bootdelegation = paxBootDelegation;
        } else {
            bootdelegation = bootdelegation + ", " + paxBootDelegation;
        }
        props.setProperty(org.osgi.framework.Constants.FRAMEWORK_BOOTDELEGATION, bootdelegation);
        return props;
    }

    class OptionsParser {
        Option[] options;
        private File gfHome;
        Properties props;
        private List<String> bundles = new ArrayList<String>();

        OptionsParser(Option[] options) {
            this.options = options;
            parseGFHome();
            props = new Properties();
            parseSystemProperties();
            parseProvisonOptions();
        }

        private void parseSystemProperties() {
            for (SystemPropertyOption spo : OptionUtils.filter(SystemPropertyOption.class, options)) {
                props.setProperty(spo.getKey(), spo.getValue());
            }
            if (props.getProperty("com.sun.aas.installRootURI") == null) {
                props.setProperty("com.sun.aas.installRootURI", getGFHome().toURI().toString());
            }
            String instanceRootStr = props.getProperty("com.sun.aas.instanceRoot");
            if (instanceRootStr == null) {
                instanceRootStr = new File(getGFHome(), "domains/domain1/").getAbsolutePath();
                props.setProperty("com.sun.aas.instanceRoot", instanceRootStr);
            }
            if (props.getProperty("com.sun.aas.instanceRootURI") == null) {
                props.setProperty("com.sun.aas.instanceRootURI", new File(instanceRootStr).toURI().toString());
            }
        }

        private void parseProvisonOptions() {
            for (ProvisionOption option : OptionUtils.filter(ProvisionOption.class, options)) {
                bundles.add(option.getURL());
            }
        }

        Properties getSystemProperties() {
            return props;
        }

        List<String> getBundles() {
            return bundles;
        }

        File getGFHome() {
            return gfHome;
        }

        private void parseGFHome() {
            for (SystemPropertyOption spo : OptionUtils.filter(SystemPropertyOption.class, options)) {
                if (spo.getKey().equals("com.sun.aas.installRoot")) {
                    gfHome = new File(spo.getValue());
                    return;
                }
            }
            throw new RuntimeException("System property called com.sun.aas.installRoot not set");
        }

        private String[] getOSGiFWs() {
            List<String> names = new ArrayList<String>();
            for (SystemPropertyOption spo : OptionUtils.filter(SystemPropertyOption.class, options)) {
                if (spo.getKey().equals("GlassFish_Platform")) {
                    names.add(spo.getValue());
                }
            }
            return names.isEmpty() ? new String[]{"Felix"} : names.toArray(new String[0]);
        }


    }
}
