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

import org.glassfish.embeddable.GlassFishException;
import org.glassfish.fighterfish.test.util.*;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.Properties;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.glassfish.fighterfish.test.util.URLHelper.getResponse;
import static org.junit.Assert.*;

/**
 * Test scenarios for various integration tests applications of FighterFish project.
 * The test applications can be found in test/testapp directory in FighterFish svn root.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class T2_Test extends AbstractTestObject {

    private Logger logger = Logger.getLogger(getClass().getPackage().getName());

    /**
     * Tests test.app0
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp0() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp0", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {
            final String location = "mvn:org.glassfish.fighterfish/test.app0/1.0.0-SNAPSHOT/war";
            Bundle bundle = tc.installBundle(location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
            final String request = ""; // homepage
            final String expectedResponse = "Hello World";
            String response = wab.getResponse(request);
            logger.logp(Level.INFO, "T2_Test", "testapp0", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app1
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp1() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp1", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {
            final String location = "mvn:org.glassfish.fighterfish/test.app1/1.0.0-SNAPSHOT/war";
            Bundle bundle = tc.installBundle(location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
            final String registrationRequest = "/RegistrationServlet?name=foo&password=bar";
            final String loginRequest = "/LoginServlet?name=foo&password=bar";
            final String registrationSuccessful = "Registered foo";
            final String loginSuccessful = "Welcome foo";
            String response = wab.getResponse(registrationRequest);
            logger.logp(Level.INFO, "T2_Test", "testapp1", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(registrationSuccessful));
            response = wab.getResponse(loginRequest);
            logger.logp(Level.INFO, "T2_Test", "testapp1", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(loginSuccessful));
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app2
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp2() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp2", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {
            String location = "mvn:org.glassfish.fighterfish/test.app2/1.0.0-SNAPSHOT/war";
            Bundle bundle = tc.installBundle(location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
            final String registrationRequest = "/RegistrationServlet?name=foo&password=bar";
            final String loginRequest = "/LoginServlet?name=foo&password=bar";
            final String registrationSuccessful = "Registered foo";
            final String loginSuccessful = "Welcome foo";
            String response = wab.getResponse(registrationRequest);
            logger.logp(Level.INFO, "T2_Test", "testapp1", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(registrationSuccessful));
            response = wab.getResponse(loginRequest);
            logger.logp(Level.INFO, "T2_Test", "testapp1", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(loginSuccessful));
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app3
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp3() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp3", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {
            String location = "mvn:org.glassfish.fighterfish/test.app3/1.0.0-SNAPSHOT/war";
            Bundle bundle = tc.installBundle(location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
            final String request = "/";
            final String expectedResponse = "Hello from POJO!";
            String response = wab.getResponse(request);
            logger.logp(Level.INFO, "T2_Test", "testapp3", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app4
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp4() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp4", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {
            String location = "mvn:org.glassfish.fighterfish/test.app4/1.0.0-SNAPSHOT/war";
            Bundle bundle = tc.installBundle(location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
            final String request = "/?username=superman";
            final String expectedResponse = "Hello, superman";
            String response = wab.getResponse(request);
            logger.logp(Level.INFO, "T2_Test", "testapp4", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app5
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp5() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp5", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {
            String location = "mvn:org.glassfish.fighterfish/test.app5/1.0.0-SNAPSHOT/war";
            Bundle bundle = tc.installBundle(location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
            final String request = "/";
            final String expectedResponse = "My name is Duke.";
            String response = wab.getResponse(request);
            logger.logp(Level.INFO, "T2_Test", "testapp5", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app6
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    @Ignore // This is currently failing for EclipseLink's inability to handle URL with bundle scheme.
    public void testapp6() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp6", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app6/1.0.0-SNAPSHOT"; // this is a .jar file, so no classifier needed
            empDeptCrud(tc, location, "testapp6");
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app7
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    @Ignore // This is currently failing for EclipseLink's inability to handle URL with bundle scheme.
    public void testapp7() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp7", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app7/1.0.0-SNAPSHOT/war";
            empDeptCrud(tc, location, "testapp7");
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app8
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp8() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp8", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app8/1.0.0-SNAPSHOT"; // this is a jar
            empDeptCrud(tc, location, "testapp8");
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app9
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp9() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp9", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app9/1.0.0-SNAPSHOT";
            Bundle bundle = tc.installBundle(location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
            final String request = "/";
            final String expectedResponse = "Success";
            String response = wab.getResponse(request);
            logger.logp(Level.INFO, "T2_Test", "testapp9", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app10
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp10() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp10", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app10/1.0.0-SNAPSHOT"; // this has .jar extn
            Bundle bundle = tc.installBundle(location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
            final String request = "/";
            final String expectedResponse = "bean: bar";
            String response = wab.getResponse(request);
            logger.logp(Level.INFO, "T2_Test", "testapp10", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app11.ejb
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp11_ejb() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp11_ejb", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            // Tests only deploying an ejb bundle with remote and local ejb in it to make sure the bug reported in #11855 is fixed.
            String location_ejb = "mvn:org.glassfish.fighterfish/test.app11.ejb/1.0.0-SNAPSHOT";
            Bundle bundle_ejb = tc.installBundle(location_ejb);
            EjbBundle ejbBundle = new EjbBundle(ctx, bundle_ejb, new String[]{"org.glassfish.fighterfish.test.app11.ejb.TestLocal"});
            ejbBundle.deploy(getTimeout(), TimeUnit.MILLISECONDS);

        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app11 as a WAB
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    @Ignore
    // Currently this does not work because of remote ejb class loading issue yet to be understood and filed as a bug
    public void testapp11_wab() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp11_wab", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            String location_ejb = "mvn:org.glassfish.fighterfish/test.app11.ejb/1.0.0-SNAPSHOT";
            String location_war = "mvn:org.glassfish.fighterfish/test.app11/1.0.0-SNAPSHOT/war";
            Bundle bundle_ejb = tc.installBundle(location_ejb);
            EjbBundle ejbBundle = new EjbBundle(ctx, bundle_ejb, new String[]{"org.glassfish.fighterfish.test.app11.ejb.TestLocal"});
            ejbBundle.deploy(getTimeout(), TimeUnit.MILLISECONDS);

            // now let's deploy the war as a WAB
            Bundle bundle_web = tc.installBundle(location_war);
            WebAppBundle wab = new WebAppBundle(ctx, bundle_web);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
            String request = "/TestServlet";
            String expectedResponse = "HELLO WORLD";
            String response = wab.getResponse(request);
            logger.logp(Level.INFO, "T2_Test", "testapp11_wab", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            tc.destroy();
        }
    }

    /**
     * Tests test.app11 as a plain war
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    @Ignore
    // Currently this does not work because of remote ejb class loading issue yet to be understood and filed as a bug
    public void testapp11_war() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp11_war", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        String appName = null;
        try {

            String location_ejb = "mvn:org.glassfish.fighterfish/test.app11.ejb/1.0.0-SNAPSHOT";
            String location_war = "mvn:org.glassfish.fighterfish/test.app11/1.0.0-SNAPSHOT/war";
            Bundle bundle_ejb = tc.installBundle(location_ejb);
            EjbBundle ejbBundle = new EjbBundle(ctx, bundle_ejb, new String[]{"org.glassfish.fighterfish.test.app11.ejb.TestLocal"});
            ejbBundle.deploy(getTimeout(), TimeUnit.MILLISECONDS);

            // let's deploy a regular web app
            appName = tc.getGlassFish().getDeployer().deploy(URI.create(location_war), "--contextroot", "test.app11");
            final String request = "http://localhost:8080/test.app11/TestServlet";
            final String expectedResponse = "HELLO WORLD";
            String response = getResponse(new URL(request));
            logger.logp(Level.INFO, "T2_Test", "testapp11_war", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            if (appName != null) {
                tc.getGlassFish().getDeployer().undeploy(appName);
            }
            tc.destroy();
        }
    }

    /**
     * Tests test.app12
     *
     */
    @Test
    public void testapp12() throws BundleException, GlassFishException, InterruptedException, IOException {
        logger.entering("T2_Test", "testapp12", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {
            String location_host = "mvn:org.glassfish.fighterfish/test.app12/1.0.0-SNAPSHOT/war";
            String location_fragment = "mvn:org.glassfish.fighterfish/test.app12.fragment/1.0.0-SNAPSHOT";

            Bundle bundle_host = tc.installBundle(location_host);
            WebAppBundle wab = new WebAppBundle(ctx, bundle_host);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);

            String requestHost = "/";
            String requestFragment = "/fragment.html";
            String expectedResponseHost = "Hello Host";
            String expectedResponseFragment = "Hello Fragment";
            String response = wab.getResponse(requestHost);
            assertThat(response, new StringPatternMatcher(expectedResponseHost));

            // now request the fragment resource
            try {
                wab.getResponse(requestFragment);
                fail("Expected fragment to be not available");
            } catch (IOException e) {
                Assert.assertTrue("Expected FileNotFoundException", e instanceof FileNotFoundException);
            }

            // now install the fragment and refresh the host
            tc.installBundle(location_fragment);
            bundle_host.stop(); // This is needed so that the web app does not get deployed upon update().
            bundle_host.update();
            wab = new WebAppBundle(ctx, bundle_host);// TODO(Sahoo): because of some bug, we can't reuse earlier wab
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS); // deploy again
            response = wab.getResponse(requestFragment);
            assertThat(response, new StringPatternMatcher(expectedResponseFragment));

        } finally {
            tc.destroy();
        }
    }


    @Test
    public void testapp13() throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp13", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app13/1.0.0-SNAPSHOT";

            {
                Bundle bundle = tc.installBundle(location);
                EjbBundle ejbBundle = new EjbBundle(ctx, bundle,
                        new String[]{"org.glassfish.fighterfish.test.app13.DummySessionBeanLocal"});
                ejbBundle.deploy(getTimeout(), TimeUnit.MILLISECONDS);
                // if deployment has been successful, then the test has passed
            }
        } finally {
            tc.destroy();
        }
    }

    @Test
    public void testapp14() throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp14", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app14/1.0.0-SNAPSHOT";
            {
                Bundle bundle = tc.installBundle(location);
                bundle.start();
                Object service = OSGiUtil.waitForService(ctx, bundle,
                        "org.glassfish.fighterfish.test.app14.ConnectionFactory", getTimeout());
                Assert.assertNotNull(service);
            }
        } finally {
            tc.destroy();
        }
    }

    @Test
    public void testapp15() throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp15", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app15/1.0.0-SNAPSHOT";
            {
                Bundle bundle = tc.installBundle(location);
                bundle.start();
                Object service = OSGiUtil.waitForService(ctx, bundle,
                        "org.glassfish.fighterfish.test.app15.ConnectionFactory", getTimeout());
                Assert.assertNotNull(service);
            }
        } finally {
            tc.destroy();
        }
    }

    @Test
    public void testapp16() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp16", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        final String cfName = "jms/fighterfish.TestApp16ConnectionFactory";
        final String topicName = "jms/fighterfish.TestApp16Topic";
        tc.createJmsCF(cfName);
        tc.createJmsTopic(topicName);
        try {
            String location_entities = "mvn:org.glassfish.fighterfish/test.app16.entities/1.0.0-SNAPSHOT";
            String location_msgproducer = "mvn:org.glassfish.fighterfish/test.app16.msgproducer/1.0.0-SNAPSHOT";
            String location_mdb = "mvn:org.glassfish.fighterfish/test.app16.mdb/1.0.0-SNAPSHOT";
            String location_wab = "mvn:org.glassfish.fighterfish/test.app16/1.0.0-SNAPSHOT/war";
            String request = "/MessageReaderServlet";
            Bundle bundle_entities = tc.installBundle(location_entities);
            bundle_entities.start();
            Object service = OSGiUtil.waitForService(ctx, bundle_entities,
                    "javax.persistence.EntityManagerFactory", getTimeout());
            Assert.assertNotNull("Checking for EMF svc registered by entities bundle", service);
            Bundle bundle_mdb = tc.installBundle(location_mdb);
            bundle_mdb.start();
            Bundle bundle_msgproducer = tc.installBundle(location_msgproducer);
            bundle_msgproducer.start();
            Bundle bundle_wab = tc.installBundle(location_wab);
            WebAppBundle wab = new WebAppBundle(ctx, bundle_wab);
            // Note, bundle deployment happens in the same order as they are started
            // Since we are not waiting for mdb deployment, let's wait double time for this to deploy.
            wab.deploy(getTimeout() * 2, TimeUnit.MILLISECONDS);
            String response = wab.getResponse(request);
            assertThat(response, new StringPatternMatcher("Total number of messages: 0"));
            ConfigurationAdmin ca = OSGiUtil.getService(ctx, ConfigurationAdmin.class, getTimeout());
            final String pkgName = "org.glassfish.fighterfish.test.app16.msgproducer";
            Configuration config = ca.getConfiguration(pkgName, null);
            Properties props = new Properties();
            props.setProperty(pkgName + ".ConnectionFactory", cfName);
            props.setProperty(pkgName + ".Destination", topicName);
            final Integer noOfMsgs = 2;
            props.setProperty(pkgName + ".NoOfMsgs", noOfMsgs.toString());
            config.update(props);
            logger.logp(Level.INFO, "T2_Test", "testapp16",
                    "Sleeping for {0} ms for config changes to be propagated and msgs to be delivered",
                    new Object[]{getTimeout()});
            Thread.sleep(getTimeout()); // Allow the config changes to be propagated and msg to reach destination
            logger.logp(Level.INFO, "T2_Test", "testapp16",
                    "Waking up from sleep");
            response = wab.getResponse(request);
            final int expectedNoOfMsgs = (noOfMsgs) * 2; // we have 2 MDBs
            logger.logp(Level.INFO, "T2_Test", "testapp16", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher("Total number of messages: " + expectedNoOfMsgs));
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.toString());
        } finally {
            tc.destroy();
        }
    }

    @Test
    public void testapp17() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp17", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {
            String location_wab = "mvn:org.glassfish.fighterfish/test.app17/1.0.0-SNAPSHOT/war";
            Bundle bundle_wab = tc.installBundle(location_wab);
            WebAppBundle wab = new WebAppBundle(ctx, bundle_wab);
            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS); // deployment is sufficient to test this bundle
            String request = "/HelloWebServiceService?wsdl";
            String response = wab.getResponse(request);
            logger.logp(Level.INFO, "T2_Test", "testapp17", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher("HelloWebServicePort"));
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.toString());
        } finally {
            tc.destroy();
        }
    }

    @Test
    public void testapp18() throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp18", new Object[]{ctx});
        TestContext tc = TestContext.create(getClass());
        try {
            String location_wab = "mvn:org.glassfish.fighterfish/test.app18/1.0.0-SNAPSHOT/war";
            final Bundle bundle_wab = tc.installBundle(location_wab);
            WebAppBundle wab = new WebAppBundle(ctx, bundle_wab);

            final Semaphore eventRaised = new Semaphore(0);
            EventAdmin eventAdmin = OSGiUtil.getService(ctx, EventAdmin.class, getTimeout());
            Assert.assertNotNull("Event Admin Service not available", eventAdmin);
            Properties props = new Properties();
            String[] topics = {"org/glassfish/fighterfist/test/app18"};
            props.put(EventConstants.EVENT_TOPIC, topics);
            ctx.registerService(EventHandler.class.getName(), new EventHandler() {
                @Override
                public void handleEvent(Event event) {
                    logger.logp(Level.INFO, "T2_Test", "testapp18", "log message = {0}", new Object[]{event});
                    eventRaised.release();
                }
            }, props);

            wab.deploy(getTimeout(), TimeUnit.MILLISECONDS); // deployment is sufficient to test this bundle
            assertTrue("Incorrect no. of events", eventRaised.tryAcquire(1, getTimeout(), TimeUnit.MILLISECONDS));
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.toString());
        } finally {
            tc.destroy();
        }
    }


    //////////////////////////////////////////////////////////////////
    // Various utility methods used from test methods are found below.
    //////////////////////////////////////////////////////////////////

    private void empDeptCrud(TestContext tc, String location, String testMethodName) throws BundleException, InterruptedException, IOException {
        Bundle bundle = tc.installBundle(location);
        WebAppBundle wab = new WebAppBundle(ctx, bundle);
        wab.deploy(getTimeout(), TimeUnit.MILLISECONDS);
        final String request1 = "/crud?action=createDepartment&departmentName=hr";
        final String request2 = "/crud?action=createDepartment&departmentName=finance";
        final String request3 = "/crud?action=createEmployee&departmentName=finance";
        final String request4 = "/crud?action=createEmployee&departmentName=hr";
        final String request5 = "/crud?action=readEmployee&employeeId=1";
        final String request6 = "/crud?action=readDepartment&departmentName=hr";
        final String request7 = "/crud?action=deleteEmployee&employeeId=2";
        final String request8 = "/crud?action=deleteEmployee&employeeId=1";
        final String request9 = "/crud?action=deleteDepartment&departmentName=hr";
        final String request10 = "/crud?action=deleteDepartment&departmentName=finance";
        final String createdResponse = "Created ";
        final String readResponse = "Found ";
        final String deletedResponse = "Deleted ";
        String response = wab.getResponse(request1);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(createdResponse));

        response = wab.getResponse(request2);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(createdResponse));

        response = wab.getResponse(request3);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(createdResponse));

        response = wab.getResponse(request4);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(createdResponse));

        response = wab.getResponse(request5);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(readResponse));

        response = wab.getResponse(request6);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(readResponse));

        response = wab.getResponse(request6);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(readResponse));

        response = wab.getResponse(request7);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(deletedResponse));

        response = wab.getResponse(request8);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(deletedResponse));

        response = wab.getResponse(request9);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(deletedResponse));

        response = wab.getResponse(request10);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(deletedResponse));
    }


}
