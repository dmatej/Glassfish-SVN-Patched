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

import org.glassfish.embeddable.GlassFish;
import org.glassfish.embeddable.GlassFishException;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

import java.io.File;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

/**
 * A TestContext is a facade through which a test interacts with underlying OSGi or Java EE platform.
 * It provides functionality like installing/uninstalling bundles, configuring Java EE resources like
 * JDBC data sources, JMS destinations, etc. Each test method is accompanied by a single TestContext object.
 * A TestContext object's life cycle is scoped to a test method for this reason. Each test method must create a
 * TestContext by calling the factory method {@link #create(BundleContext)} at the beginning of the test method and
 * destroy it by calling {@link #destroy} at the end of the test method. When a test context is destroyed, all changes
 * done so far will be rolled back. This includes any bundles deployed. any domain configuration made, etc.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class TestContext {

    // TODO(Sahoo): Group related methods into separate interfaces.
    // TODO(Sahoo): Add methods from OSGiUtil here.
    // TODO(Sahoo): Use fluent API
    // TODO(Sahoo): Explore possibility of automatically controlling life cycle of a TestContext

    private final String testClassName;
    private final String testMethodName;

    /**
     * BundleContext associated with the test
     */
    private BundleContext ctx;

    private BundleProvisioner bundleProvisioner;
    private EnterpriseResourceProvisioner resourceProvisioner;

    private Logger logger = Logger.getLogger(getClass().getPackage().getName());

    private TestContext(String testClassName, String testMethodName, BundleContext ctx) {
        logger.info("Beginning of test " + testClassName + "." + testMethodName);
        this.ctx = ctx;
        this.testClassName = testClassName;
        this.testMethodName = testMethodName;
        bundleProvisioner = new BundleProvisioner(ctx);
        resourceProvisioner = new EnterpriseResourceProvisioner();
    }

    public static TestContext create(final BundleContext ctx) throws GlassFishException, InterruptedException {
        TestContext tc = new TestContext(getCallingClassName(), getCallingMethodName(), ctx);
        tc.getGlassFish();
        tc.configureEmbeddedDerby();
        return tc;
    }

    public void destroy() throws BundleException, GlassFishException {
        bundleProvisioner.uninstallAllTestBundles();
        resourceProvisioner.restoreDomainConfiguration();
        logger.info("End of test " + testClassName + "." + testMethodName);
    }

    /**
     * Deploy the given OSGi Web Application Bundle.
     * WAB deployment happens asynchronously when a WAB is activated. It waits for a configured amount time
     * for deployment to take place successfully. If deployment fails or does not happen within the configured
     * times, it throws TimeoutException.
     * @param bundle
     * @return ServletContext associated with the deployed web application
     * @throws BundleException
     * @throws InterruptedException
     */
    public WebAppBundle deployWebAppBundle(Bundle bundle) throws BundleException, InterruptedException {
        WebAppBundle webAppBundle = new WebAppBundle(getBundleContext(), bundle);
        webAppBundle.deploy(TestsConfiguration.getInstance().getTimeout(), TimeUnit.MILLISECONDS);
        return webAppBundle;
    }

    /**
     * Deploy the given JPA Entities bundle. If a service of type EntityManagerFactory does not get registered in
     * the specified time, assume the deployment has failed and throw a TimeoutException.
     * @param bundle Entity bundle to be deployed
     * @return a handle to the deployed application
     * @throws BundleException
     * @throws InterruptedException
     * @throws TimeoutException
     */
    public EntityBundle deployEntityBundle(Bundle bundle) throws BundleException, InterruptedException {
        EntityBundle entityBundle = new EntityBundle(getBundleContext(), bundle);
        entityBundle.deploy(TestsConfiguration.getInstance().getTimeout(), TimeUnit.MILLISECONDS);
        return entityBundle;
    }

    /**
     * Deploy the given EJB OSGi bundle. Deployment is triggered asynchronously by starting the bundle. If none of the
     * user specified services show up in service registry in the specified amount of time, it assumes the operation
     * has failed and throws TimeoutOperation.
     * @param bundle EJB Bundle to be deployed
     * @param services Services that are expected to be made available by this EJB bundle if deployment is successful.
     * @return a handle to the deployed application
     * @throws BundleException
     * @throws InterruptedException
     * @throws TimeoutException
     */
    public EjbBundle deployEjbBundle(Bundle bundle, String[] services) throws BundleException, InterruptedException {
        EjbBundle ejbBundle = new EjbBundle(getBundleContext(), bundle, services);
        ejbBundle.deploy(TestsConfiguration.getInstance().getTimeout(), TimeUnit.MILLISECONDS);
        return ejbBundle;
    }

    public GlassFish getGlassFish() throws GlassFishException, InterruptedException {
        return GlassFishTracker.waitForGfToStart(ctx, TestsConfiguration.getInstance().getTimeout());
    }

    public void configureEmbeddedDerby() throws GlassFishException, InterruptedException {
        resourceProvisioner.configureEmbeddedDerby(getGlassFish(),
                testMethodName,
                new File(EnterpriseResourceProvisioner.getDerbyDBRootDir(), testMethodName));
    }

    public BundleContext getBundleContext() {
        return ctx;
    }

    public Bundle installTestBundle(String location) throws BundleException {
        return bundleProvisioner.installTestBundle(location);
    }

    public void createJmsCF(String cfName) throws GlassFishException, InterruptedException {
        resourceProvisioner.createJmsCF(getGlassFish(), cfName);
    }

    public void createJmsTopic(String topicName) throws GlassFishException, InterruptedException {
        resourceProvisioner.createJmsTopic(getGlassFish(), topicName);
    }

    private static String getCallingMethodName() {
        return new Exception().getStackTrace()[2].getMethodName();
    }

    private static String getCallingClassName() {
        return new Exception().getStackTrace()[2].getClassName();
    }

}
