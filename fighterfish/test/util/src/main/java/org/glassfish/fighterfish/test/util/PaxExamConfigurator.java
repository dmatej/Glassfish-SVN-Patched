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


package org.glassfish.fighterfish.test.util;

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

import static org.glassfish.fighterfish.test.util.Constants.FW_CONFIG_FILE_NAME;
import static org.ops4j.pax.exam.CoreOptions.*;
import static org.ops4j.pax.exam.OptionUtils.combine;

/**
 * Provides common PAX configuration for all tests.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class PaxExamConfigurator {
    // TODO(Sahoo): Use mavenConfiguration to avoid having to encode version numbers while using maven urls.

    protected Logger logger = Logger.getLogger(getClass().getPackage().getName());
    private File gfHome;
    private final long timeout;

    public PaxExamConfigurator(File gfHome, long timeout) {
        this.gfHome = gfHome;
        this.timeout = timeout;
    }

    public Option[] configure() throws IOException {
        return combine(combine(frameworkConfiguration(), provisioningBundles()), paxConfiguration());
    }

    private Option[] provisioningBundles() {
        final String version = Version.getVersion();
        logger.logp(Level.INFO, "PaxExamConfigurator", "provisioningBundles", "FighterFish Test Util Version = {0}",
                new Object[]{version});
        final Option gfBundle = bundle(new File(gfHome, "modules/glassfish.jar").toURI().toString());
        return options(gfBundle,
                junitBundles(),
                mavenBundle().groupId("org.apache.httpcomponents").artifactId("httpclient-osgi").version("4.2.4"),
                mavenBundle().groupId("org.apache.httpcomponents").artifactId("httpcore-osgi").version("4.2.4"),
                mavenBundle().groupId("org.glassfish.fighterfish").artifactId("test.util").version(version)
        );
    }

    private Option[] frameworkConfiguration() throws IOException {
        // We currently read framework options from a separate file, but we could
        // as well inline them here in code.
        final Properties properties = readFrameworkConfiguration();

        // override by system properties if set in system. We override everything except fileinstall properties
        // as GlassFish's domain.xml  is known to set them incorrectly.
        for(Map.Entry<Object, Object> entry : properties.entrySet()) {
            Object orig = properties.get(entry.getKey());
            Object override = System.getProperty((String) entry.getKey());
            if (override != null) {
                if (String.class.cast(entry.getKey()).startsWith("felix.fileinstall.")) {
                    logger.logp(Level.INFO, "PaxExamConfigurator", "frameworkConfiguration",
                            "Ignoring overriding of {0}", new Object[]{entry});
                    continue;
                }
                properties.put(entry.getKey(), override);
                logger.logp(Level.INFO, "PaxExamConfigurator", "frameworkConfiguration",
                        "entry = {0}, original = {1}, override = {2}",
                        new Object[]{entry.getKey(), orig, override});
            }
        }
        List<Option> options = convertToOptions(properties);

        // See: http://team.ops4j.org/browse/PAXEXAM-267
        // NativeTestContainer does not export the following package, but our test.util needs it
//        options.add(systemPackages("org.ops4j.pax.exam.options.extra; version=" + Info.getPaxExamVersion()));
        return options.toArray(new Option[options.size()]);
    }

    private Option[] paxConfiguration() {
        return options(systemTimeout(timeout));
    }

    /**
     * Adapts properties to pax-exam options.
     *
     * @param properties
     * @return
     */
    private List<Option> convertToOptions(Properties properties) {
        List<Option> options = new ArrayList<Option>();
        for (Map.Entry<Object, Object> entry : properties.entrySet()) {
            if (entry.getKey().equals(Constants.FRAMEWORK_STORAGE)) {
                // Starting with pax-exam 2.1.0, we need to specify framework storage using workingDirectory option
                options.add(workingDirectory((String) entry.getValue()));
                logger.logp(Level.INFO, "PaxExamConfigurator", "convertToOptions", "OSGi cache dir = {0}",
                        new Object[]{entry.getValue()});
            }
            options.add(frameworkProperty((String) entry.getKey()).value(entry.getValue()));
        }
        options.add(cleanCaches(false)); // default is to remove the cache
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
