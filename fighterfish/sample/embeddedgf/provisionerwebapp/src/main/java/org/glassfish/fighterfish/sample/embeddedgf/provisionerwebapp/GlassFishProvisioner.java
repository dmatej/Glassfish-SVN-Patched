/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012 Oracle and/or its affiliates. All rights reserved.
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


package org.glassfish.fighterfish.sample.embeddedgf.provisionerwebapp;

import org.glassfish.embeddable.GlassFish;
import org.glassfish.embeddable.GlassFishException;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;
import org.osgi.framework.launch.Framework;
import org.osgi.util.tracker.ServiceTracker;

import javax.annotation.Resource;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;
import java.io.File;
import java.net.URL;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author sanjeeb.sahoo@oracle.com
 */

@WebListener
public class GlassFishProvisioner implements ServletContextListener {

    private ServletContext servletContext;
    private volatile Framework framework;
    private volatile GlassFish glassfish;
    private final ExecutorService executorService = Executors.newSingleThreadExecutor();

    private String fwJndiName = FW_JNDI_NAME_DEFAULT;
    private String gfJndiName = GF_JNDI_NAME_DEFAULT;
    private String gfHome;

    private static final String FW_JNDI_NAME_DEFAULT = "java:global/glassfish-osgi-framework";
    private static final String GF_JNDI_NAME_DEFAULT = "java:global/glassfish-instance";
    private static final String GLASSFISH_INSTALL_ROOT_PROP = "com.sun.aas.installRoot";
    private Bundle gfMainBundle;

    @Resource
    public void setGfJndiName(String gfJndiName) {
        this.gfJndiName = gfJndiName;
    }

    @Resource
    public void setFwJndiName(String fwJndiName) {
        this.fwJndiName = fwJndiName;
    }

    @Resource
    public void setGfHome(String gfHome) {
        this.gfHome = gfHome;
    }

    @Override
    public void contextInitialized(ServletContextEvent servletContextEvent) {
        this.servletContext = servletContextEvent.getServletContext();
        executorService.submit(new Runnable() {
            @Override
            public void run() {
                try {
                    waitForFramework();
                    provisionGlassFish();
                    waitForGlassFish();
                    new InitialContext().rebind(gfJndiName, glassfish);
                    log("bound " + glassfish + " in JNDI location: " + gfJndiName);
                } catch (InterruptedException e) {
                    log("got interrupted: ", e);
                    Thread.currentThread().interrupt();
                    return;
                } catch (Exception e) {
                    log("Something has gone wrong while provisioning GlassFish.", e);
                }
            }
        });
        log("Submitted task");
    }

    private void waitForFramework() throws ExecutionException, InterruptedException {
        log("waiting for OSGi framework");
        framework = new WaitForOSGiFrameworkTask().call();
    }


    private void waitForGlassFish() throws InterruptedException, ExecutionException {
        log("waiting for GlassFish");
        ServiceTracker st = new ServiceTracker(framework.getBundleContext(), GlassFish.class.getName(), null);
        st.open();
        try {
            glassfish = (GlassFish) st.waitForService(0);
        } finally {
            st.close();
        }
        new WaitForGlassFishToStart(glassfish).call();
    }

    private void provisionGlassFish() throws Exception {
        BundleContext bctx = framework.getBundleContext();
        if (gfHome == null) {
            gfHome = bctx.getProperty("com.sun.aas.installRoot");
        }
        if (gfHome == null) {
            throw new RuntimeException("Please set GlassFish home either by setting a property called " +
                    GLASSFISH_INSTALL_ROOT_PROP + " either in the system or in OSGi properties file.\n" +
                    "Alternatively, you can set it using runtime deployment descriptor or " +
                    "deployment plan while deploying this war file.");
        }
        log("Going to provision GlassFish bundles from " + gfHome);
        File jar = new File(gfHome, "modules" + File.separator + "glassfish.jar");
        if (!jar.exists()) {
            throw new Exception(jar.getAbsolutePath() + " does not exist. Check what you have set as " + GLASSFISH_INSTALL_ROOT_PROP);
        }
        URL url = jar.toURI().toURL();
        this.log("Installing bundle [" + url + "]");
        gfMainBundle = bctx.installBundle(url.toExternalForm());
        try {
            gfMainBundle.start(Bundle.START_TRANSIENT); // start transiently to have more control over the lifecycle
        } catch (BundleException e) {
            log("Failed to start " + gfMainBundle, e);
        }
    }

    @Override
    public void contextDestroyed(ServletContextEvent servletContextEvent) {
        log("Shutting down the executor so that it can cancel any pending tasks");
        executorService.shutdownNow();
        if (gfMainBundle != null) {
            try {
                gfMainBundle.stop();
            } catch (BundleException e) {
                log("Error while stopping glassfish main bundle " + gfMainBundle, e);
            }
        }
        glassfish = null;
    }

    /**
     * Waits for OSGi framework to be available in JNDI
     */
    private class WaitForOSGiFrameworkTask implements Callable<Framework> {
        @Override
        public Framework call() throws InterruptedException {
            while (true) {
                try {
                    Framework framework = (Framework) new InitialContext().lookup(fwJndiName);
                    log("obtained " + framework);
                    return framework;
                } catch (NamingException e) {
                    log("This is sometimes expected: ", e);
                }
                Thread.sleep(1000);
            }
        }

    }

    /**
     * Waits for GlassFish to start.
     */
    private class WaitForGlassFishToStart implements Callable<Void> {

        GlassFish gf;

        private WaitForGlassFishToStart(GlassFish gf) {
            this.gf = gf;
        }

        @Override
        public Void call() throws InterruptedException {
            try {
                // Poll for GlassFish to start. GlassFish service might have been registered by
                // GlassFishRuntime.newGlassFish() and hence might not be ready to use.
                GlassFish.Status status = gf.getStatus();
                while (status != GlassFish.Status.STARTED && status != GlassFish.Status.DISPOSED) {
                    Thread.sleep(1000);
                }
                if (status != GlassFish.Status.STARTED) {
                    log("status = " + status);
                    throw new RuntimeException("GlassFish didn't start properly");
                }

            } catch (GlassFishException e) {
                throw new RuntimeException(e); // TODO(Sahoo): Proper Exception Handling
            }
            return null;
        }
    }

    private void log(String s) {
        servletContext.log("provisionerwebapp: " + s);
    }

    private void log(String s, Throwable e) {
        servletContext.log("provisionerwebapp: " + s, e);
    }

}
