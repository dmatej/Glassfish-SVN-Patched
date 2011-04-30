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
import org.glassfish.fighterfish.test.util.EjbBundle;
import org.glassfish.fighterfish.test.util.GlassFishTracker;
import org.glassfish.fighterfish.test.util.OSGiUtil;
import org.glassfish.fighterfish.test.util.WebAppBundle;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ops4j.pax.exam.junit.JUnit4TestRunner;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

import java.io.*;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
@RunWith(JUnit4TestRunner.class)
public class T2_Test extends AbstractTestObject {

    /**
     * Tests test.app0
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp0(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp0", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        try {
            final String location = "mvn:org.glassfish.fighterfish/test.app0/1.0.0-SNAPSHOT/war";
            Bundle bundle = installTestBundle(ctx, location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            final String request = ""; // homepage
            final String expectedResponse = "Hello World";
            String response = getResponse(wab, request);
            logger.logp(Level.INFO, "T2_Test", "testapp0", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            uninstallAllTestBundles();
        }
    }

    /**
     * Tests test.app1
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp1(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp1", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "testapp1", new File(derbyRootDir, "testapp1"));
        try {

            final String location = "mvn:org.glassfish.fighterfish/test.app1/1.0.0-SNAPSHOT/war";
            Bundle bundle = installTestBundle(ctx, location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            final String registrationRequest = "/RegistrationServlet?name=foo&password=bar";
            final String loginRequest = "/LoginServlet?name=foo&password=bar";
            final String registrationSuccessful = "Registered foo";
            final String loginSuccessful = "Welcome foo";
            String response = getResponse(wab, registrationRequest);
            logger.logp(Level.INFO, "T2_Test", "testapp1", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(registrationSuccessful));
            response = getResponse(wab, loginRequest);
            logger.logp(Level.INFO, "T2_Test", "testapp1", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(loginSuccessful));
        } finally {
            uninstallAllTestBundles();
            rdc.restore();
        }
    }

    /**
     * Tests test.app2
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp2(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp2", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "testapp2", new File(derbyRootDir, "testapp2"));
        try {
            String location = "mvn:org.glassfish.fighterfish/test.app2/1.0.0-SNAPSHOT/war";
            Bundle bundle = installTestBundle(ctx, location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            final String registrationRequest = "/RegistrationServlet?name=foo&password=bar";
            final String loginRequest = "/LoginServlet?name=foo&password=bar";
            final String registrationSuccessful = "Registered foo";
            final String loginSuccessful = "Welcome foo";
            String response = getResponse(wab, registrationRequest);
            logger.logp(Level.INFO, "T2_Test", "testapp1", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(registrationSuccessful));
            response = getResponse(wab, loginRequest);
            logger.logp(Level.INFO, "T2_Test", "testapp1", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(loginSuccessful));
        } finally {
            uninstallAllTestBundles();
            rdc.restore();
        }
    }

    /**
     * Tests test.app3
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp3(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp3", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        try {
            String location = "mvn:org.glassfish.fighterfish/test.app3/1.0.0-SNAPSHOT/war";
            Bundle bundle = installTestBundle(ctx, location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            final String request = "/";
            final String expectedResponse = "Hello from POJO!";
            String response = getResponse(wab, request);
            logger.logp(Level.INFO, "T2_Test", "testapp3", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            uninstallAllTestBundles();
        }
    }

    /**
     * Tests test.app4
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp4(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp4", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        try {
            String location = "mvn:org.glassfish.fighterfish/test.app4/1.0.0-SNAPSHOT/war";
            Bundle bundle = installTestBundle(ctx, location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            final String request = "/?username=superman";
            final String expectedResponse = "Hello, superman";
            String response = getResponse(wab, request);
            logger.logp(Level.INFO, "T2_Test", "testapp4", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            uninstallAllTestBundles();
        }
    }

    /**
     * Tests test.app5
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp5(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp5", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        try {
            String location = "mvn:org.glassfish.fighterfish/test.app5/1.0.0-SNAPSHOT/war";
            Bundle bundle = installTestBundle(ctx, location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            final String request = "/";
            final String expectedResponse = "My name is Duke.";
            String response = getResponse(wab, request);
            logger.logp(Level.INFO, "T2_Test", "testapp5", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            uninstallAllTestBundles();
        }
    }

    /**
     * Tests test.app6
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    @Ignore // This is currently failing for EclipseLink's inability to handle URL with bundle scheme.
    public void testapp6(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp6", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "testapp6", new File(derbyRootDir, "testapp6"));
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app6/1.0.0-SNAPSHOT"; // this is a .jar file, so no classifier needed
            empDeptCrud(ctx, location, "testapp6");
        } finally {
            uninstallAllTestBundles();
            rdc.restore();
        }
    }

    /**
     * Tests test.app7
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    @Ignore // This is currently failing for EclipseLink's inability to handle URL with bundle scheme.
    public void testapp7(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp7", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "testapp7", new File(derbyRootDir, "testapp7"));
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app7/1.0.0-SNAPSHOT/war";
            empDeptCrud(ctx, location, "testapp7");
        } finally {
            uninstallAllTestBundles();
            rdc.restore();
        }
    }

    /**
     * Tests test.app8
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp8(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp8", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "testapp8", new File(derbyRootDir, "testapp8"));
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app8/1.0.0-SNAPSHOT"; // this is a jar
            empDeptCrud(ctx, location, "testapp8");
        } finally {
            uninstallAllTestBundles();
            rdc.restore();
        }
    }

    /**
     * Tests test.app9
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp9(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp9", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "testapp9", new File(derbyRootDir, "testapp9"));
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app9/1.0.0-SNAPSHOT/war";
            Bundle bundle = installTestBundle(ctx, location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            final String request = "/";
            final String expectedResponse = "Success";
            String response = getResponse(wab, request);
            logger.logp(Level.INFO, "T2_Test", "testapp9", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            uninstallAllTestBundles();
            rdc.restore();
        }
    }

    /**
     * Tests test.app10
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp10(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp10", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "testapp10", new File(derbyRootDir, "testapp10"));
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app10/1.0.0-SNAPSHOT"; // this has .jar extn
            Bundle bundle = installTestBundle(ctx, location);
            WebAppBundle wab = new WebAppBundle(ctx, bundle);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            final String request = "/";
            final String expectedResponse = "bean: bar";
            String response = getResponse(wab, request);
            logger.logp(Level.INFO, "T2_Test", "testapp10", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            uninstallAllTestBundles();
            rdc.restore();
        }
    }

    /**
     * Tests test.app11.ejb
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    public void testapp11_ejb(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp11_ejb", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        try {

            // Tests only deploying an ejb bundle with remote and local ejb in it to make sure the bug reported in #11855 is fixed.
            String location_ejb = "mvn:org.glassfish.fighterfish/test.app11.ejb/1.0.0-SNAPSHOT";
            Bundle bundle_ejb = installTestBundle(ctx, location_ejb);
            EjbBundle ejbBundle = new EjbBundle(ctx, bundle_ejb, new String[]{"org.glassfish.fighterfish.test.app11.ejb.TestLocal"});
            ejbBundle.deploy(TIMEOUT, TimeUnit.MILLISECONDS);

        } finally {
            uninstallAllTestBundles();
        }
    }

    /**
     * Tests test.app11 as a WAB
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    @Ignore // Currently this does not work because of remote ejb class loading issue yet to be understood and filed as a bug
    public void testapp11_wab(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp11_wab", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        try {

            String location_ejb = "mvn:org.glassfish.fighterfish/test.app11.ejb/1.0.0-SNAPSHOT";
            String location_war = "mvn:org.glassfish.fighterfish/test.app11/1.0.0-SNAPSHOT/war";
            Bundle bundle_ejb = installTestBundle(ctx, location_ejb);
            EjbBundle ejbBundle = new EjbBundle(ctx, bundle_ejb, new String[]{"org.glassfish.fighterfish.test.app11.ejb.TestLocal"});
            ejbBundle.deploy(TIMEOUT, TimeUnit.MILLISECONDS);

            // now let's deploy the war as a WAB
            Bundle bundle_web = installTestBundle(ctx, location_war);
            WebAppBundle wab = new WebAppBundle(ctx, bundle_web);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
            String request = "/TestServlet";
            String expectedResponse = "HELLO WORLD";
            String response = wab.getResponse(request);
            logger.logp(Level.INFO, "T2_Test", "testapp11_wab", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            uninstallAllTestBundles();
        }
    }

    /**
     * Tests test.app11 as a plain war
     * @param ctx        .
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     */
    @Test
    @Ignore // Currently this does not work because of remote ejb class loading issue yet to be understood and filed as a bug
    public void testapp11_war(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException, IOException {
        logger.entering("T2_Test", "testapp11_war", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        String appName = null;
        try {

            String location_ejb = "mvn:org.glassfish.fighterfish/test.app11.ejb/1.0.0-SNAPSHOT";
            String location_war = "mvn:org.glassfish.fighterfish/test.app11/1.0.0-SNAPSHOT/war";
            Bundle bundle_ejb = installTestBundle(ctx, location_ejb);
            EjbBundle ejbBundle = new EjbBundle(ctx, bundle_ejb, new String[]{"org.glassfish.fighterfish.test.app11.ejb.TestLocal"});
            ejbBundle.deploy(TIMEOUT, TimeUnit.MILLISECONDS);

            // let's deploy a regular web app
            appName =gf.getDeployer().deploy(URI.create(location_war), "--contextroot", "test.app11");
            final String request = "http://localhost:8080/test.app11/TestServlet";
            final String expectedResponse = "HELLO WORLD";
            String response = getResponse(new URL(request));
            logger.logp(Level.INFO, "T2_Test", "testapp11_war", "response = {0}", new Object[]{response});
            assertThat(response, new StringPatternMatcher(expectedResponse));
        } finally {
            if (appName != null) {
                gf.getDeployer().undeploy(appName);
            }
            uninstallAllTestBundles();
        }
    }

    /**
     * Tests test.app12
     * @param ctx
     */
    @Test
    public void testapp12(BundleContext ctx) throws BundleException, GlassFishException, InterruptedException, IOException {
        logger.entering("T2_Test", "testapp12", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        try {
            String location_host = "mvn:org.glassfish.fighterfish/test.app12/1.0.0-SNAPSHOT/war";
            String location_fragment = "mvn:org.glassfish.fighterfish/test.app12.fragment/1.0.0-SNAPSHOT";

            Bundle bundle_host = installTestBundle(ctx, location_host);
            WebAppBundle wab = new WebAppBundle(ctx, bundle_host);
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);

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
            installTestBundle(ctx, location_fragment);
            bundle_host.stop(); // This is needed so that the web app does not get deployed upon update().
            bundle_host.update();
            wab = new WebAppBundle(ctx, bundle_host);// TODO(Sahoo): because of some bug, we can't reuse earlier wab
            wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS); // deploy again
            response = wab.getResponse(requestFragment);
            assertThat(response, new StringPatternMatcher(expectedResponseFragment));
            
        }finally {
            uninstallAllTestBundles();
        }
    }


    @Test
    public void testapp13(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp13", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app13/1.0.0-SNAPSHOT";

            {
                Bundle bundle = installTestBundle(ctx, location);
                EjbBundle ejbBundle = new EjbBundle(ctx, bundle,
                        new String[]{"org.glassfish.fighterfish.test.app13.DummySessionBeanLocal"});
                ejbBundle.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
                // if deployment has been successful, then the test has passed
            }
        }finally {
            uninstallAllTestBundles();
        }
    }

    @Test
    public void testapp14(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp14", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "testapp14", new File(derbyRootDir, "testapp14"));
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app14/1.0.0-SNAPSHOT";
            {
                Bundle bundle = installTestBundle(ctx, location);
                bundle.start();
                Object service = OSGiUtil.waitForService(ctx, bundle,
                        "org.glassfish.fighterfish.test.app14.ConnectionFactory", TIMEOUT);
                Assert.assertNotNull(service);
            }
        }finally {
            rdc.restore();
            uninstallAllTestBundles();
        }
    }
    
    @Test
    public void testapp15(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException {
        logger.entering("T2_Test", "testapp15", new Object[]{ctx});
        GlassFish gf = GlassFishTracker.waitForService(ctx, TIMEOUT);
        RestorableDomainConfiguration rdc = configureEmbeddedDerby(gf, "testapp15", new File(derbyRootDir, "testapp15"));
        try {

            String location = "mvn:org.glassfish.fighterfish/test.app15/1.0.0-SNAPSHOT";
            {
                Bundle bundle = installTestBundle(ctx, location);
                bundle.start();
                Object service = OSGiUtil.waitForService(ctx, bundle,
                        "org.glassfish.fighterfish.test.app15.ConnectionFactory", TIMEOUT);
                Assert.assertNotNull(service);
            }
        }finally {
            rdc.restore();
            uninstallAllTestBundles();
        }
    }

    //////////////////////////////////////////////////////////////////
    // Various utility methods used from test methods are found below.
    //////////////////////////////////////////////////////////////////

    
    private String getResponse(URL request) throws IOException {
        URLConnection yc = request.openConnection();
        BufferedReader in = new BufferedReader(
                new InputStreamReader(
                        yc.getInputStream()));

        StringBuilder sb = new StringBuilder();
        String inputLine;
        while ((inputLine = in.readLine()) != null) {
            sb.append(inputLine);
        }
        in.close();
        return sb.toString();
    }

    private void empDeptCrud(BundleContext ctx, String location, String testMethodName) throws BundleException, InterruptedException {
        Bundle bundle = installTestBundle(ctx, location);
        WebAppBundle wab = new WebAppBundle(ctx, bundle);
        wab.deploy(TIMEOUT, TimeUnit.MILLISECONDS);
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
        String response = getResponse(wab, request1);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(createdResponse));

        response = getResponse(wab, request2);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(createdResponse));

        response = getResponse(wab, request3);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(createdResponse));

        response = getResponse(wab, request4);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(createdResponse));

        response = getResponse(wab, request5);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(readResponse));

        response = getResponse(wab, request6);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(readResponse));

        response = getResponse(wab, request6);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(readResponse));

        response = getResponse(wab, request7);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(deletedResponse));

        response = getResponse(wab, request8);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(deletedResponse));

        response = getResponse(wab, request9);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(deletedResponse));

        response = getResponse(wab, request10);
        logger.logp(Level.INFO, "T2_Test", testMethodName, "response = {0}", new Object[]{response});
        assertThat(response, new StringPatternMatcher(deletedResponse));
    }


}
