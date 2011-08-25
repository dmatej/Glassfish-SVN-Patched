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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.glassfish.fighterfish.test.util.Constants.EXAM_TIMEOUT_PROP;
import static org.glassfish.fighterfish.test.util.Constants.FW_CONFIG_FILE_NAME;
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
    private final File fwStorage;

    public PaxExamConfigurator(File gfHome, String platform, long timeout, File fwStorage) {
        this.gfHome = gfHome;
        this.platform = platform;
        this.timeout = timeout;
        this.fwStorage = fwStorage;
    }

    public Option[] configure() throws IOException {
        return combine(combine(frameworkConfiguration(), provisioningBundles()), paxConfiguration());
    }

    private Option[] provisioningBundles() {
        final String version = Version.getVersion();
        logger.logp(Level.INFO, "PaxExamConfigurator", "provisioningBundles", "FighterFish Test Util Version = {0}",
                new Object[]{version});
        return options(bundle(new File(gfHome, "modules/glassfish.jar").toURI().toString()),
                mavenBundle().groupId("org.junit").artifactId("com.springsource.org.junit").version("4.8.1"),
                mavenBundle().groupId("org.glassfish.fighterfish").artifactId("test.util").version(version)
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
        if (fwStorage != null) {
            // add this ahead of others so that it will override any value read from OSGiFramework.properties,
            options.add(0, workingDirectory(fwStorage.getAbsolutePath()));
        }
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

}
