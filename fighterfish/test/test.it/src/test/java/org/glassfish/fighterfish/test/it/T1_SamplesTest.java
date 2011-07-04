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
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ops4j.pax.exam.junit.ExamReactorStrategy;
import org.ops4j.pax.exam.junit.JUnit4TestRunner;
import org.ops4j.pax.exam.spi.reactors.EagerSingleStagedReactorFactory;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;
import org.osgi.service.http.HttpService;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.util.Properties;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import static org.junit.Assert.*;
import static org.junit.Assert.assertTrue;
import static org.osgi.framework.Bundle.START_TRANSIENT;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
@RunWith(JUnit4TestRunner.class)
@ExamReactorStrategy( EagerSingleStagedReactorFactory.class )
public class T1_SamplesTest extends AbstractTestObject {


    @Test
    public void uas_sample_test(BundleContext ctx)
            throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T1_SamplesTest", "uas_sample_test", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "uas_sample_test", new File(derbyRootDir, "uas_sample_test"));
        try {
            /*
             * URIs of various sample.uas bundles that we are going to use.
             */
            String uas_api = "mvn:org.glassfish.fighterfish/sample.uas.api/1.0.0-SNAPSHOT";
            String uas_simpleservice = "mvn:org.glassfish.fighterfish/sample.uas.simpleservice/1.0.0-SNAPSHOT";
            String uas_simplewab = "mvn:org.glassfish.fighterfish/sample.uas.simplewab/1.0.0-SNAPSHOT/war";
            String uas_simplewabfragment = "mvn:org.glassfish.fighterfish/sample.uas.simplewabfragment/1.0.0-SNAPSHOT";
            String uas_entities = "mvn:org.glassfish.fighterfish/sample.uas.entities/1.0.0-SNAPSHOT";
            String uas_ejbservice = "mvn:org.glassfish.fighterfish/sample.uas.ejbservice/1.0.0-SNAPSHOT";
            String uas_ejbservice2 = "mvn:org.glassfish.fighterfish/sample.uas.ejbservice2/1.0.0-SNAPSHOT";
            String uas_advservice = "mvn:org.glassfish.fighterfish/sample.uas.advservice/1.0.0-SNAPSHOT";

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
            final String reportJspRequest = "/report.jsp";
            final String reportServletRequest = "/ReportServlet";

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
                try {
                    uas_simple_webapp.getResponse(reportJspRequest);
                    fail("Expected fragment to be not available");
                } catch (IOException e) {
                    Assert.assertTrue("Expected FileNotFoundException", e instanceof FileNotFoundException);
                }

                // now install the fragment and refresh the host
                installTestBundle(ctx, uas_simplewabfragment);
                uas_simplewab_b.stop(); // This is needed so that the web app does not get deployed upon update().
                uas_simplewab_b.update();
                uas_simple_webapp = new WebAppBundle(ctx, uas_simplewab_b);// TODO(Sahoo): because of some bug, we can't reuse earlier wab
                uas_simple_webapp.deploy(TIMEOUT, TimeUnit.MILLISECONDS); // deploy again
                response = getResponse(uas_simple_webapp, reportJspRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher("to see the report."));

                // now let's see if the servlet from the fragment can be used or not.
                response = getResponse(uas_simple_webapp, reportServletRequest);
                logger.logp(Level.INFO, "T1_SamplesTest", "uas_sample_test", "response = {0}", new Object[]{response});
                assertThat(response, new StringPatternMatcher("Login Attempt Report:"));
            }
        } finally {
            uninstallAllTestBundles();
            rdc.restore();
        }
    }

    @Test
    public void osgihttp_helloworld_sample_test(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T1_SamplesTest", "osgihttp_helloworld_sample_test", new Object[]{ctx});
        try {
            HttpService httpService = OSGiUtil.getService(ctx, HttpService.class);
            assertNull(httpService);
            for(Bundle b : ctx.getBundles()) {
                if ("org.glassfish.fighterfish.osgi-http".equals(b.getSymbolicName())) {
                    b.stop(Bundle.START_TRANSIENT);
                    b.start(Bundle.START_TRANSIENT);
                }
            }
            httpService = OSGiUtil.getService(ctx, HttpService.class, TIMEOUT);
            assertNotNull(httpService);
            final String location = "mvn:org.glassfish.fighterfish/sample.osgihttp.helloworld/1.0.0-SNAPSHOT";
            Bundle bundle = installTestBundle(ctx, location);
            final Semaphore eventRaised = new Semaphore(0);
            EventAdmin eventAdmin = OSGiUtil.getService(ctx, EventAdmin.class, TIMEOUT);
            Assert.assertNotNull("Event Admin Service not available", eventAdmin);
            Properties props = new Properties();
            String[] topics = {"org/glassfish/fighterfish/sample/osgihttp/helloworld"};
            props.put(EventConstants.EVENT_TOPIC, topics);
            ctx.registerService(EventHandler.class.getName(), new EventHandler(){
                @Override
                public void handleEvent(Event event) {
                    logger.logp(Level.INFO, "SingleTest", "handleEvent", "event = {0}", new Object[]{event});
                    eventRaised.release();
                }
            }, props);

            bundle.start(Bundle.START_TRANSIENT);
            assertTrue("Timedout waiting for event", eventRaised.tryAcquire(1, TIMEOUT, TimeUnit.MILLISECONDS));
            URL request1 = new URL("http://localhost:8080/osgi/hello1");
            URL request2 = new URL("http://localhost:8080/osgi/hello2");
            URL request3 = new URL("http://localhost:8080/osgi/hello3");
            String response = getResponse(request1);
            logger.logp(Level.INFO, "T1_SamplesTest", "osgihttp_helloworld_sample_test", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher("servlet context counter = 0"));
            response = getResponse(request1);
            logger.logp(Level.INFO, "T1_SamplesTest", "osgihttp_helloworld_sample_test", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher("servlet context counter = 1"));
            response = getResponse(request2);
            logger.logp(Level.INFO, "T1_SamplesTest", "osgihttp_helloworld_sample_test", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher("servlet context counter = 2"));
            response = getResponse(request1);
            logger.logp(Level.INFO, "T1_SamplesTest", "osgihttp_helloworld_sample_test", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher("servlet context counter = 3"));
            response = getResponse(request3);
            logger.logp(Level.INFO, "T1_SamplesTest", "osgihttp_helloworld_sample_test", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher("servlet context counter = null"));
        } finally {
            uninstallAllTestBundles();
        }
    }
}
