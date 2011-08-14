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


package org.glassfish.fighterfish.test.util;

import org.ops4j.pax.exam.Info;
import org.ops4j.pax.exam.Option;
import org.osgi.framework.Constants;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.glassfish.fighterfish.test.util.Constants.*;
import static org.ops4j.pax.exam.CoreOptions.*;
import static org.ops4j.pax.exam.OptionUtils.combine;

/**
 * Provides common PAX configuration for all tests.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class PaxExamConfigurator {
//    Not needed, as this is set by NativeTestContainer automatically.
//    static {
//        System.setProperty( "java.protocol.handler.pkgs", "org.ops4j.pax.url" );
//    }

    // TODO(Sahoo): Use mavenConfiguration to avoid having to encode version numbers while using maven urls.

    protected Logger logger = Logger.getLogger(getClass().getPackage().getName());
    private File gfHome;
    private String platform;
    private final long timeout;

    public PaxExamConfigurator(File gfHome, String platform, long timeout) {
        this.gfHome = gfHome;
        this.platform = platform;
        this.timeout = timeout;
    }

    public Option[] configure() throws IOException {
        return combine(combine(frameworkConfiguration(), provisioningBundles()), paxConfiguration());
    }

    private Option[] provisioningBundles() {
        return options(bundle(new File(gfHome, "modules/glassfish.jar").toURI().toString()),
                mavenBundle().groupId("org.junit").artifactId("com.springsource.org.junit").version("4.8.1"),
                mavenBundle().groupId("org.glassfish.fighterfish").artifactId("test.util").version("1.0.0-SNAPSHOT")
//                mavenBundle().groupId("org.ops4j.pax.exam").artifactId("pax-exam").version("2.1.0")
        );
    }

    private Option[] frameworkConfiguration() throws IOException {
        // We currently read framework options from a separate file, but we could
        // as well inline them here in code.
        List<Option> options = convertToOptions(readFrameworkConfiguration());

        // See: http://team.ops4j.org/browse/PAXEXAM-267
        // NativeTestContainer does not export the following package, but our test.util needs it
        options.add(systemPackages("org.ops4j.pax.exam.options.extra; version=" + Info.getPaxExamVersion()));
        return options.toArray(new Option[options.size()]);
    }

    private Option[] paxConfiguration() {
        // Switch to pax-exam API to set timeout when migrating to 2.2.0
        return options(systemProperty(EXAM_TIMEOUT_PROP).value(String.valueOf(timeout)));
    }

    /**
     * Adapts priperties to pax-exam options.
     *
     * @param properties
     * @return
     */
    private List<Option> convertToOptions(Properties properties) {
        List<Option> options = new ArrayList<Option>();
        // parse boot delegation property and set it as a bootdelegation option
        // as pax-exam's native test container implementation
        // does not use bootdelegation system property.
        for (String p : properties.getProperty(Constants.FRAMEWORK_BOOTDELEGATION, "").split(",")) {
            if (p.trim().isEmpty()) continue;
            System.out.println("Boot delegation pkg = " + p);
            options.add(bootDelegationPackage(p.trim()));
        }
        for (Map.Entry<Object, Object> entry : properties.entrySet()) {
            if (entry.getKey().equals(Constants.FRAMEWORK_BOOTDELEGATION)) continue; // already handled above
            if (entry.getKey().equals(Constants.FRAMEWORK_STORAGE)) {
                // Starting with pax-exam 2.1.0, we need to specify framework storage using workingDirectory option
                options.add(workingDirectory((String) entry.getValue()));
                continue;
            }
            // use frameworkProperty after migrating to new pax-exam (see http://team.ops4j.org/browse/PAXEXAM-261)
            options.add(systemProperty((String) entry.getKey()).value((String) entry.getValue()));
        }
        return options;
    }

    private Properties readFrameworkConfiguration() throws IOException {
        Properties properties = new Properties();
        logger.logp(Level.INFO, "DefaultPaxExamConfiguration", "readFrameworkConfiguration",
                "fwConfigFileName = {0}", new Object[]{FW_CONFIG_FILE_NAME});
        InputStream stream = getClass().getResourceAsStream(FW_CONFIG_FILE_NAME);
        if (stream != null) {
            try {
                properties.load(stream);
            } finally {
                stream.close();
            }
            PropertiesUtil.substVars(properties);
        } else {
            logger.logp(Level.WARNING, "DefaultPaxExamConfiguration", "readFrameworkConfiguration",
                    "{0} not found. Using default values", new Object[]{FW_CONFIG_FILE_NAME});
        }
        return properties;
    }

    private Properties readAndCustomizeFrameworkConfiguration() throws IOException {
        Properties properties = new Properties();
        URI uri;
        if (System.getProperty("glassfish.osgi-configuration") != null) {
            uri = URI.create(System.getProperty("glassfish.osgi-configuration"));
        } else if ("Felix".equals(platform)) {
            uri = new File(gfHome, "osgi/felix/conf/config.properties").toURI();
        } else if ("Equinox".equals(platform)) {
            uri = new File(gfHome, "osgi/equinox/configuration/config.ini").toURI();
        } else {
            throw new RuntimeException("GlassFish_Platform can only be Felix or Equinox");
        }
        logger.logp(Level.INFO, "TestConfiguration", "readAndCustomizeFrameworkConfiguration", "uri = {0}", new Object[]{uri});
        InputStream is = uri.toURL().openStream();
        try {
            properties.load(is);
        } finally {
            is.close();
        }

        // Because the provisioning bundles are not uninstalled and because they are marked persistently started,
        // we use a different cache location.
        final String defaultLocation = properties.getProperty(Constants.FRAMEWORK_STORAGE) + "/pax-exam/";
        final String cacheLocation = System.getProperty(Constants.FRAMEWORK_STORAGE, defaultLocation);
        System.out.println("OSGi persistence store location = " + cacheLocation);
        properties.put(Constants.FRAMEWORK_STORAGE, cacheLocation);

        // Because some of our tests use JPA in Java SE mode, we have to set this property to enable the same.
        properties.put("org.glassfish.osgjpa.extension.useHybridPersistenceProviderResolver", "true");

        // GlassFish is ocnfigured to get bootstrap APIs from system bundle so that the main program can control GlassFish's life cycle.
        // But the bootstrap bundle's activator, GlassFishMainActivator, does not shutdown GlassFishRuntime in its stop(). As a result,
        // when the bootstrap bundle is again started in the same VM, it gets an exception sayng "Already bootstrapped."
        // To avoid this, we don't configure system bundle to export bootstrap APIs. They are then obtained from bootstrap bundle
        // itself and test bundles also import from that bundle.
        String oldValue = (String) properties.get("extra-system-packages");
        final String newValue = "${jre-${java.specification.version}} ${internal-jdk-pkgs-for-gf}";
        System.out.println("Replacing extra-system-properties from [" + oldValue + "] to [" + newValue + "]");
        properties.put("extra-system-packages", newValue);

        // Now substitute properties
        PropertiesUtil.substVars(properties);
        return properties;
    }

}
