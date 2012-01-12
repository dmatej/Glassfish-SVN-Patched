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
import org.ops4j.pax.exam.options.SystemPropertyOption;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
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
    private ExamSystem examSystem;

    @Override
    public TestContainer[] create(ExamSystem system) throws TestContainerException {
        this.examSystem = system;
        parser = new OptionsParser();
        return new TestContainer[]{createTestContainer()};
    }

    private TestContainer createTestContainer() {
        String fwName = parser.getOSGiFW();
        logger.logp(Level.INFO, "GFTCF", "createTestContainer", "fwName = {0}", new Object[]{fwName});
        try {
            System.setProperty("java.protocol.handler.pkgs", "org.ops4j.pax.url");
            ClassLoader launcherCL = createGFLauncherCL(parser.getOSGiFW());
            BootstrapProperties bsProperties = getBootstrapProperties(fwName);
            logger.logp(Level.INFO, "GFTCF", "createTestContainer", "GlassFishRuntime BootstrapProperties = {0}", new Object[]{bsProperties.getProperties()});
            gfr = GlassFishRuntime.bootstrap(bsProperties, launcherCL);
            GlassFishProperties gfProps = new GlassFishProperties(bsProperties.getProperties());
            return new GlassFishTestContainer(gfr, gfProps, examSystem);
        } catch (GlassFishException e) {
            throw new RuntimeException(e);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e); // TODO(Sahoo): Proper Exception Handling
        }
    }

    private BootstrapProperties getBootstrapProperties(String fwName) {
        Properties props = getOSGiFWConfiguration(fwName);
        props.putAll(parser.getSystemProperties()); // override by what's specified in options and system
        Util.substVars(props); // variable resolution
        for (String key : System.getProperties().stringPropertyNames()) {
            if (key.startsWith("felix.fileinstall.")) continue;
            props.put(key, System.getProperty(key));
        }
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
        File propertiesFile = new File(parser.getGFHome(), "config/osgi.properties");
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
        final String paxBootDelegation = "org.ops4j.pax.exam, org.ops4j.pax.exam.*, org.junit, org.junit.*";
        if (bootdelegation == null) {
            bootdelegation = paxBootDelegation;
        } else {
            bootdelegation = bootdelegation + ", " + paxBootDelegation;
        }
        props.setProperty(org.osgi.framework.Constants.FRAMEWORK_BOOTDELEGATION, bootdelegation);
        return props;
    }

    public ExamSystem getExamSystem() {
        return examSystem;
    }

    class OptionsParser {
        private File gfHome;
        Properties props;

        public OptionsParser() {
            props = new Properties();
            parseSystemProperties();
            parseGFHome();
        }

        private void parseSystemProperties() {
            for (SystemPropertyOption spo : getExamSystem().getOptions(SystemPropertyOption.class)) {
                String override = System.getProperty(spo.getKey());
                String value = override!=null ? override : spo.getValue(); // prefer system property
                props.setProperty(spo.getKey(), value);
            }
        }

        Properties getSystemProperties() {
            return props;
        }

        File getGFHome() {
            return gfHome;
        }

        private void parseGFHome() {
            String installRootStr = getProperty("com.sun.aas.installRoot");
            if (installRootStr != null) {
                gfHome = new File(installRootStr);
            } else {
                throw new RuntimeException("System property called com.sun.aas.installRoot not set");
            }
            props.setProperty("com.sun.aas.installRootURI", gfHome.toURI().toString());
            String instanceRootStr = getProperty("com.sun.aas.instanceRoot");
            if (instanceRootStr == null) {
                instanceRootStr = new File(gfHome, "domains/domain1/").getAbsolutePath();
                props.setProperty("com.sun.aas.instanceRoot", instanceRootStr);
            }
            props.setProperty("com.sun.aas.instanceRootURI", new File(instanceRootStr).toURI().toString());
        }

        private String getProperty(final String key) {
            return System.getProperty(key, props.getProperty(key)); // prefer system property
        }

        private String getOSGiFW() {
            List<String> names = new ArrayList<String>();
            for (SystemPropertyOption spo : getExamSystem().getOptions(SystemPropertyOption.class)) {
                if (spo.getKey().equals("GlassFish_Platform")) {
                    names.add(spo.getValue());
                }
            }
            if (names.size() > 1) {
                throw new RuntimeException("More than one platform specified: " + names);
            }
            return names.isEmpty() ? "Felix" : names.get(0);
        }
    }
}
