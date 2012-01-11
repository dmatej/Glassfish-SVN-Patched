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

import org.ops4j.pax.exam.Option;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Represents configuration common to all tests.
 * It reads configuration information from System properties and configures various underlying objects.
 * Depending on configuration, this also installs GlassFish.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class TestsConfiguration {

    private File gfHome;
    private String provisioningUrl;
    private long testTimeout;
    private boolean install;
    private File installDir;

    protected Logger logger = Logger.getLogger(getClass().getPackage().getName());

    private static TestsConfiguration instance;

    public synchronized static TestsConfiguration getInstance() {
        if (instance == null) {
            instance = new TestsConfiguration(System.getProperties());
        }
        return instance;
    }

    private TestsConfiguration(Properties properties) {
        testTimeout = Long.parseLong(
                properties.getProperty(Constants.FIGHTERFISH_TEST_TIMEOUT_PROP,
                        Constants.FIGHTERFISH_TEST_TIMEOUT_DEFAULT_VALUE));
        String property = properties.getProperty(Constants.GLASSFISH_INSTALL_ROOT_PROP);
        if (property != null && !property.isEmpty()) {
            gfHome =  new File(property);
        }
        property = properties.getProperty(Constants.FIGHTERFISH_PROVISIONER_URL_PROP);
        if (property != null && !property.isEmpty()) {
            provisioningUrl = property;
        }
        if (gfHome == null) {
            if (provisioningUrl == null) {
                // both are unspecified
                provisioningUrl = Constants.FIGHTERFISH_PROVISIONER_URL_DEFAULT_VALUE;
            }
            // We compute a hashcode so that if user changes provisioning url, we are less likely to
            // reuse the earlier created installation.
            installDir = new File(System.getProperty("java.io.tmpdir"), "fighterfish-" + provisioningUrl.hashCode());
            gfHome = new File(installDir, "glassfish3/glassfish/");
            install = !installDir.exists();
            if (!install) {
                logger.logp(Level.INFO, "TestsConfiguration", "setup",
                        "Reusing existing installation at {0}", new Object[]{installDir});
            }
        } else {
            // gfHome is specified
            if(!gfHome.exists()) {
                // explode only if provisioning url is explicitly specified
                install = provisioningUrl!= null;
                installDir = new File(gfHome, "../..");
            }
        }
    }

    private void install() {
        if (install) {
            logger.logp(Level.INFO, "TestsConfiguration", "TestsConfiguration",
                "Will install {0} at {1}", new Object[]{provisioningUrl, installDir});
            explode(provisioningUrl, installDir);
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

    public long getTimeout() {
        return testTimeout;
    }

    private File getGfHome() {
        return gfHome;
    }

    public Option[] getPaxExamConfiguration() throws IOException {
        install();
        return new PaxExamConfigurator(getGfHome(), getTimeout()).configure();
    }

    static {
        // Work around for GLASSFISH-16510.
        // This code gets executes before any test methods get executed, which means this code
        // gets executed before any embedded glassfish gets provisioned. By eagely calling, getPlatformMBeanServer,
        // we ensure that all embedded glassfish will use this as opposed to what is created by
        // AppServerMBeanServerBuilder.
        java.lang.management.ManagementFactory.getPlatformMBeanServer();

        // This is needed as we allow user to specify glassfish zip installer using schemes like mvn
        System.setProperty( "java.protocol.handler.pkgs", "org.ops4j.pax.url" );

    }
}
