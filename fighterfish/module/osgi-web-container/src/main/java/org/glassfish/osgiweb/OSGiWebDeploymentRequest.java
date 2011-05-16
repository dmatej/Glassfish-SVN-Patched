/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2009-2011 Oracle and/or its affiliates. All rights reserved.
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

package org.glassfish.osgiweb;

import com.sun.enterprise.config.serverbeans.VirtualServer;
import com.sun.enterprise.web.*;
import org.glassfish.internal.data.ApplicationInfo;
import org.glassfish.internal.data.EngineRef;
import org.glassfish.internal.data.ModuleInfo;
import org.glassfish.osgijavaeebase.DeploymentException;
import org.glassfish.osgijavaeebase.OSGiApplicationInfo;
import org.glassfish.osgijavaeebase.OSGiDeploymentRequest;
import org.glassfish.osgijavaeebase.OSGiDeploymentContext;
import org.glassfish.api.ActionReport;
import org.glassfish.api.deployment.archive.ReadableArchive;
import org.glassfish.api.deployment.OpsParams;
import org.glassfish.api.deployment.DeployCommandParameters;
import org.glassfish.server.ServerEnvironmentImpl;
import org.glassfish.internal.deployment.Deployment;
import org.glassfish.internal.api.Globals;
import org.osgi.framework.*;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.io.IOException;

import com.sun.enterprise.deploy.shared.ArchiveFactory;
import com.sun.enterprise.config.serverbeans.*;
import org.osgi.service.packageadmin.PackageAdmin;

import javax.servlet.ServletContext;

import static org.glassfish.osgiweb.Constants.*;
import static org.osgi.framework.Constants.BUNDLE_VERSION;


/**
 * This is the class responsible for deploying a WAB in the Java EE container.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class OSGiWebDeploymentRequest extends OSGiDeploymentRequest {

    private static final Logger logger =
            Logger.getLogger(OSGiWebDeployer.class.getPackage().getName());

    // Set the current bundle context in a thread local for use during web module decoration
    private static ThreadLocal<BundleContext> currentBundleContext = new ThreadLocal<BundleContext>();

    public OSGiWebDeploymentRequest(Deployment deployer, ArchiveFactory archiveFactory, ServerEnvironmentImpl env, ActionReport reporter, Bundle b) {
        super(deployer, archiveFactory, env, reporter, b);
    }

    protected OSGiDeploymentContext getDeploymentContextImpl(ActionReport reporter, Logger logger, ReadableArchive archive, OpsParams opsParams, ServerEnvironmentImpl env, Bundle b) throws Exception {
        return new OSGiWebDeploymentContext(reporter, logger, archive, opsParams, env, b);
    }

    @Override
    protected WAB makeArchive() {
        Bundle host = getBundle();
        Bundle[] fragments = getPackageAdmin().getFragments(host);
        return new WAB(host, fragments);
    }

    private PackageAdmin getPackageAdmin() {
        BundleContext ctx = BundleReference.class.cast(getClass().getClassLoader()).getBundle().getBundleContext();
        return PackageAdmin.class.cast(ctx.getService(ctx.getServiceReference(PackageAdmin.class.getName())));
    }

    @Override
    protected DeployCommandParameters getDeployParams() throws Exception {
        DeployCommandParameters parameters = super.getDeployParams();
        // Set the contextroot explicitly, else it defaults to name.
        try
        {
            // We expect WEB_CONTEXT_PATH to be always present.
            // This is mandated in the spec.
            parameters.contextroot = getArchive().getManifest().
                    getMainAttributes().getValue(WEB_CONTEXT_PATH);
        }
        catch (IOException e)
        {
            // ignore and continue
        }
        if (parameters.contextroot == null || parameters.contextroot.length() == 0)
        {
            throw new Exception(WEB_CONTEXT_PATH +
                    " manifest header is mandatory");
        }
        if (!parameters.contextroot.startsWith("/")) {
            // We prefix '/' for reasons mentioned in Uitil.getContextRoot(). 
            parameters.contextroot = "/".concat(parameters.contextroot);
        }
        parameters.virtualservers = getVirtualServers();
        return parameters;
    }

    private String getVirtualServers() {
        String virtualServers = null;
        try {
            virtualServers = getArchive().getManifest().getMainAttributes().getValue(
                    VIRTUAL_SERVERS);
        } catch (Exception e) {
            // ignore
        }
        if (virtualServers == null) virtualServers = getDefaultVirtualServer();
        StringTokenizer st = new StringTokenizer(virtualServers);
        if (st.countTokens() > 1) {
            throw new IllegalArgumentException("Currently, we only support deployment to one virtual server.");
        }
        return virtualServers;
    }

    /**
     * @return comma-separated list of all defined virtual servers (exclusive
     * of __asadmin)
     */
    private String getAllVirtualServers() {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        Domain domain = Globals.get(Domain.class);
        String target = "server"; // Need to understand how to dynamically obtains this
        Server server = domain.getServerNamed(target);
        if (server != null) {
            Config config = domain.getConfigs().getConfigByName(
                server.getConfigRef());
            if (config != null) {
                HttpService httpService = config.getHttpService();
                if (httpService != null) {
                    List<VirtualServer> hosts = httpService.getVirtualServer();
                    if (hosts != null) {
                        for (VirtualServer host : hosts) {
                            if (("__asadmin").equals(host.getId())) {
                                continue;
                            }
                            if (first) {
                                sb.append(host.getId());
                                first = false;
                            } else {
                                sb.append(",");
                                sb.append(host.getId());
                            }
                        }
                    }
                }
            }
        }
        return sb.toString();
    }

    /**
     * @return the dafault virtual server
     */
    private String getDefaultVirtualServer() {
        // Grizzly renamed its package name from com.sun.grizzly to org.glassfish.grizzly in Grizzly 2.1. Since Grizzly 2.1 is only
        // integrated into GF3.2 only and we expect our module to work with GF 3.1.1 as well, we are not relying on Grizzly classes statically.
        // So, the code below does what the following line would have done.
        // return Globals.get(com.sun.grizzly.config.dom.NetworkListener.class).findHttpProtocol().getHttp().getDefaultVirtualServer();
        Class netWorkListenerClass;
        try {
            netWorkListenerClass = Class.forName("com.sun.grizzly.config.dom.NetworkListener");
        } catch (ClassNotFoundException cnfe) {
            try {
                netWorkListenerClass = Class.forName("org.glassfish.grizzly.config.dom.NetworkListener");
            } catch (ClassNotFoundException e) {
                throw new RuntimeException(e);
            }
        }
        Object networkListenerObj = Globals.get(netWorkListenerClass);
        try {
            Method findHttpProtocolMethod = netWorkListenerClass.getMethod("findHttpProtocol");
            Object httpProtocolObj = findHttpProtocolMethod.invoke(networkListenerObj);
            final Object httpObj = httpProtocolObj.getClass().getMethod("getHttp").invoke(httpProtocolObj);
            final String defaultVirtualServer = (String) httpObj.getClass().getMethod("getDefaultVirtualServer").invoke(httpObj);
            logger.logp(Level.FINE, "OSGiWebDeploymentRequest", "getDefaultVirtualServer", "defaultVirtualServer = {0}", new Object[]{defaultVirtualServer});
            return defaultVirtualServer;
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void preDeploy() throws DeploymentException {
        detectCollisions();
        currentBundleContext.set(getBundle().getBundleContext());
    }

    private void detectCollisions() throws ContextPathCollisionException {
        ContextPathCollisionDetector cd = ContextPathCollisionDetector.get();
        cd.preDeploy(getBundle());
    }

    @Override
    public void postDeploy() {
        currentBundleContext.set(null);
        OSGiApplicationInfo osgiAppInfo = getResult();
        if (osgiAppInfo == null) {
            ContextPathCollisionDetector cd = ContextPathCollisionDetector.get();
            cd.cleanUp(getBundle());
            return;
        }
        ServletContext sc = getServletContext(osgiAppInfo.getAppInfo());
        assert(sc.getAttribute(BUNDLE_CONTEXT_ATTR) == osgiAppInfo.getBundle().getBundleContext());

        try {
            ServiceRegistration scReg = registerService(osgiAppInfo.getBundle(), sc);
            // TODO(Sahoo): Unregister scReg when we go down
        } catch (IllegalStateException e) {
            // See issue #15398 as to why this can happen
            logger.logp(Level.WARNING, "OSGiWebDeploymentRequest", "postDeploy",
                    "Failed to register ServletContext for bundle " + osgiAppInfo.getBundle().getBundleId() +
                            " because of following exception:", e);
        }
    }

    private ServletContext getServletContext(ApplicationInfo appInfo)
    {
        if (appInfo.getModuleInfos().size() == 1)
        {
            ModuleInfo m = appInfo.getModuleInfos().iterator().next();
            EngineRef e = m.getEngineRefForContainer(com.sun.enterprise.web.WebContainer.class);
            assert (e != null);
            WebApplication a = (WebApplication) e.getApplicationContainer();
            Set<com.sun.enterprise.web.WebModule> wms = a.getWebModules();
            assert (wms.size() == 1); // we only deploy to default virtual server
            if (wms.size() == 1)
            {
                return wms.iterator().next().getServletContext();
            }
        }
        return null;
    }

    private ServiceRegistration registerService(Bundle b, ServletContext sc)
    {
        Properties props = new Properties();
        props.setProperty(OSGI_WEB_SYMBOLIC_NAME, b.getSymbolicName());
        String cpath = Util.getContextPath(b);
        props.setProperty(OSGI_WEB_CONTEXTPATH, cpath);
        String version = (String) b.getHeaders().get(BUNDLE_VERSION);
        if (version != null)
        {
            props.setProperty(OSGI_WEB_VERSION, version);
        }
        BundleContext bctx = b.getBundleContext();
        if (bctx != null) {
            // This null check is required until we upgrade to Felix 1.8.1.
            // Felix 1.8.0 returns null when bundle is in starting state.
            ServiceRegistration scReg = bctx.registerService(
                    ServletContext.class.getName(),
                    sc, props);
            logger.logp(Level.INFO, "OSGiWebContainer", "registerService",
                    "Registered ServletContext as a service with properties: {0} ",
                    new Object[]{props});
            return scReg;
        } else {
            logger.logp(Level.WARNING, "OSGiWebContainer", "registerService",
                    "Not able to register ServletContext as a service as bctx is null");
        }
        return null;
    }

    /* package */ static BundleContext getCurrentBundleContext() {
        return currentBundleContext.get();
    }

}
