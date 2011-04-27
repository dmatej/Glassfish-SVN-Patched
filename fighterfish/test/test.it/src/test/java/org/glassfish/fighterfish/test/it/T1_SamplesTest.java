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


package org.glassfish.fighterfish.test.it;

import org.glassfish.embeddable.GlassFish;
import org.glassfish.embeddable.GlassFishException;
import org.glassfish.fighterfish.test.util.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ops4j.pax.exam.junit.JUnit4TestRunner;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

import java.io.File;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.osgi.framework.Bundle.START_TRANSIENT;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
@RunWith(JUnit4TestRunner.class)
public class T1_SamplesTest extends AbstractTestObject {

    /*
     * URIs of various sample.uas bundles that we are going to use.
     */
    private String uas_api = "mvn:org.glassfish.fighterfish/sample.uas.api/1.0.0-SNAPSHOT";
    private String uas_simpleservice = "mvn:org.glassfish.fighterfish/sample.uas.simpleservice/1.0.0-SNAPSHOT";
    private String uas_simplewab = "mvn:org.glassfish.fighterfish/sample.uas.simplewab/1.0.0-SNAPSHOT/war";
    private String uas_simplewabfragment = "mvn:org.glassfish.fighterfish/sample.uas.simplewabfragment/1.0.0-SNAPSHOT";
    private String uas_entities = "mvn:org.glassfish.fighterfish/sample.uas.entities/1.0.0-SNAPSHOT";
    private String uas_ejbservice = "mvn:org.glassfish.fighterfish/sample.uas.ejbservice/1.0.0-SNAPSHOT";
    private String uas_ejbservice2 = "mvn:org.glassfish.fighterfish/sample.uas.ejbservice2/1.0.0-SNAPSHOT";
    private String uas_advservice = "mvn:org.glassfish.fighterfish/sample.uas.advservice/1.0.0-SNAPSHOT";

    @Test
    public void uas_sample_test(BundleContext ctx)
            throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T1_SamplesTest", "uas_sample_test", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "uas_sample_test", new File(derbyRootDir, "uas_sample_test"));
        try {
            Bundle uas_api_b = installTestBundle(ctx, uas_api);
            Bundle uas_simpleservice_b = installTestBundle(ctx, uas_simpleservice);
            Bundle uas_simplewab_b = installTestBundle(ctx, uas_simplewab);
            WebAppBundle uas_simple_webapp = new WebAppBundle(ctx, uas_simplewab_b);
            uas_simple_webapp.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            String response = null;

            // Service type of the EJB registered by uas ejb service bundle 
            final String uas_service_type = "org.glassfish.fighterfish.sample.uas.api.UserAuthService";


            // Various request URIs - very tightly dependennt on servlets implementing the functionality
            final String loginRequest = "/LoginServlet?name=foo&password=bar";
            final String registrationRequest = "/RegistrationServlet?name=foo&password=bar";
            final String unregistrationRequest = "/UnregistrationServlet?name=foo";
            final String reportRequest = "/ReportServlet";

            // Expected Patterns for various kinds of output - very tightly coupled with what the implementation returns
            final String serviceUnavailable = "Service is not yet available";
            final String loginFailed = "Incorrect user name or password. Try again";
            final String successfulLogin = "Welcome ";
            final String successfulRegistration = "Registered ";
            final String successfulUnregistration = "Unregistered ";
            final String failedUnregistration = "Failed to unregister ";
            final String successfulReport = "Login Attempt Report:";

            {
                // Scenario 1: no service
                response = getResponse(uas_simple_webapp, loginRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(serviceUnavailable));
            }
            {
                // Scenario 2: dynamically adding a service bundle and retrying...
                uas_simpleservice_b.start(START_TRANSIENT);
                response = getResponse(uas_simple_webapp, loginRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(loginFailed));

                // now let's register a user and retry
                response = getResponse(uas_simple_webapp, registrationRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(successfulRegistration));
                response = getResponse(uas_simple_webapp, loginRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(successfulLogin));

                // unregister
                response = getResponse(uas_simple_webapp, unregistrationRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(successfulUnregistration));
            }

            {
                // Scenario #3: Dynamically switching the service by ejbservice
                uas_simpleservice_b.stop();
                response = getResponse(uas_simple_webapp, loginRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(serviceUnavailable));

                // let's install ejbservice bundle and retry
                Bundle uas_ejbservice_b = installTestBundle(ctx, uas_ejbservice);
                EjbBundle uas_ejbapp = new EjbBundle(ctx, uas_ejbservice_b, new String[] {uas_service_type});
                uas_ejbapp.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
                getResponse(uas_simple_webapp, unregistrationRequest); // unregister just in case there was a user by this name
                response = getResponse(uas_simple_webapp, loginRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(loginFailed));

                // now let's register a user and retry
                response = getResponse(uas_simple_webapp, registrationRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(successfulRegistration));

                response = getResponse(uas_simple_webapp, loginRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(successfulLogin));

                // unregister
                response = getResponse(uas_simple_webapp, unregistrationRequest);

                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(successfulUnregistration));

                // stop the service bundle and retry to make sure we are failing to get the service
                uas_ejbapp.undeploy();
                response = getResponse(uas_simple_webapp, unregistrationRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(serviceUnavailable));
            }

            {
                // Scenario #4: Let's replace the ejbservice by ejbservice2 which uses standalone entities jar.
                Bundle uas_entity_b = installTestBundle(ctx, uas_entities);
                EntityBundle uas_entityapp = new EntityBundle(ctx, uas_entity_b);
                uas_entityapp.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
                Bundle uas_ejbservice2_b = installTestBundle(ctx, uas_ejbservice2);
                EjbBundle uas_ejbapp2 = new EjbBundle(ctx, uas_ejbservice2_b, new String[] {uas_service_type});
                uas_ejbapp2.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
                response = getResponse(uas_simple_webapp, registrationRequest);
                assertThat(response, new StringPatternMatcher(successfulRegistration));

                // login
                response = getResponse(uas_simple_webapp, loginRequest);
                assertThat(response, new StringPatternMatcher(successfulLogin));

                // unregister
                response = getResponse(uas_simple_webapp, unregistrationRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher(successfulUnregistration));
            }

            {
                // WAB fragment test
            }
        } finally {
            uninstallAllTestBundles();
            rdc.restore();
        }
    }

}
