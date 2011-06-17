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

package org.glassfish.osgihttp;

import com.sun.enterprise.config.serverbeans.Config;
import com.sun.enterprise.config.serverbeans.Domain;
import com.sun.enterprise.config.serverbeans.Server;
import com.sun.enterprise.config.serverbeans.VirtualServer;
import com.sun.enterprise.web.WebContainer;
import com.sun.enterprise.web.WebModule;
import com.sun.enterprise.web.WebModuleConfig;
import org.apache.catalina.Container;
import org.apache.catalina.Engine;
import org.apache.catalina.Host;
import org.apache.catalina.Manager;
import org.apache.catalina.Realm;
import org.apache.catalina.core.StandardContext;
import org.apache.catalina.session.StandardManager;
import org.apache.catalina.startup.ContextConfig;
import org.glassfish.api.admin.ServerEnvironment;
import org.glassfish.api.event.EventListener;
import org.glassfish.api.event.EventTypes;
import org.glassfish.api.event.Events;
import org.glassfish.internal.api.ClassLoaderHierarchy;
import org.glassfish.internal.api.Globals;
import org.glassfish.web.valve.GlassFishValve;
import org.jvnet.hk2.component.Habitat;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.http.HttpService;
import org.osgi.util.tracker.ServiceTracker;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * This is the entry point to our implementation of OSGi/HTTP service.
 * For every virtual server in the configuration, it creates HTTPService.
 * Every service has same context path. The context path can be defined
 * by user using configuration property org.glassfish.web.osgihttp.ContextPath.
 * If it is absent, we use a default value of "/osgi." After initializing
 * the HttpService factory with necessary details, we register the factory
 * OSGi service registry.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class Activator implements BundleActivator {

    // TODO(Sahoo): Use config admin to configure context path, virtual server, etc.

    private BundleContext bctx;
    private Map<String, Host> vss = new HashMap<String, Host>();
    private String contextPath;
    private List<ServiceRegistration> registrations = new ArrayList<ServiceRegistration>();

    // configuration property used to select context root under which
    // this service is deployed.
    private static final String CONTEXT_PATH_PROP =
            Activator.class.getPackage().getName() + ".ContextPath";
    private ServiceTracker serverTracker;

    private Logger logger = Logger.getLogger(getClass().getPackage().getName());

    public void start(BundleContext context) throws Exception {
        bctx = context;
        serverTracker = new GlassFishServerTracker(bctx);
        serverTracker.open();
    }

    /**
     * This method is responsible for registering a HTTPService for every virtual server.
     * Each service is registered with a service property called "VirtualServer," which can be used by clients
     * to select a service. e.g., web console can use this to select __asadmin virtual server.
     * While registering the service for the default virtual server, it sets the service.ranking
     * to the maximum value so that any client just looking for an HTTPService gets to see the
     * HTTPService bound to default virtual server.
     *
     * @param webContainer
     */
    private void doActualWork(WebContainer webContainer) {
        String defaultVsId = getDefaultVirtualServer();
        final StringTokenizer vsIds = new StringTokenizer(getAllVirtualServers(), ",");
        while (vsIds.hasMoreTokens()) {
            String vsId = vsIds.nextToken().trim();
            try {
                WebModule standardContext = createRootWebModule(webContainer, vsId);
                if (standardContext == null) {
                    logger.logp(Level.WARNING, "Activator", "doActualWork",
                            "GlassFishHttpService will not be available for for virtual server = {0}, " +
                                    "because we are not able to create root web app.", new Object[]{vsId});
                    continue;
                }
                GlassFishHttpService httpService = new GlassFishHttpService(standardContext);
                Properties props = new Properties();
                props.put("VirtualServer", vsId);
                if (vsId.equals(defaultVsId)) {
                    props.put(Constants.SERVICE_RANKING, Integer.MAX_VALUE);
                }
                ServiceRegistration registration = bctx.registerService(HttpService.class.getName(),
                        new HttpServiceWrapper.HttpServiceFactory(httpService),
                        props);
                registrations.add(registration);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private WebModule createRootWebModule(WebContainer webContainer, String vsId) throws Exception {
        Engine engine = webContainer.getEngine();
        Host vs = (Host) engine.findChild(vsId);
        if (vs == null) return null; // this can happen if some one deleted a virtual server after we read domain.xml
        vss.put(vsId, vs);
        contextPath = bctx.getProperty(CONTEXT_PATH_PROP);
        if (contextPath == null) {
            contextPath = "/osgi"; // default value
        }
        // create a new context under which all OSGi HTTP wrappers
        // will be registered.
        final WebModule standardContext = new WebModule();
        standardContext.setWebContainer(webContainer);
        standardContext.setName(contextPath);
        standardContext.setPath(contextPath);
        // TODO(Sahoo): Need to set proper values for these directories
        standardContext.setDocBase(System.getProperty("java.io.tmpdir"));
        standardContext.setWorkDir(System.getProperty("java.io.tmpdir"));
        // standardContext.setJ2EEServer(System.getProperty("com.sun.aas.instanceName"));
        standardContext.setJ2EEServer(getInstanceName());
        standardContext.addLifecycleListener(new ContextConfig());
        Realm realm = Globals.getDefaultHabitat().getByContract(Realm.class);
        standardContext.setRealm(realm);
        WebModuleConfig wmConfig = new WebModuleConfig();
        wmConfig.setWorkDirBase(System.getProperty("java.io.tmpdir"));
        wmConfig.setVirtualServers(vsId);

        // Setting it in WebModuleConfig does not work, Ceck with Jan.
//        wmConfig.setAppClassLoader(getCommonClassLoader());
        standardContext.setParentClassLoader(getCommonClassLoader());
        standardContext.setWebModuleConfig(wmConfig);

        // See  See GLASSFISH-16764 for more details about this valve
        standardContext.addValve((GlassFishValve) new OSGiHttpContextValve());
        // Since there is issue about locating user classes that are part
        // of some OSGi bundle while deserializing, we switch off session
        // persistence.
        switchOffSessionPersistence(standardContext);
        vs.addChild(standardContext);
        logger.logp(Level.INFO, "Activator", "createRootWebModule", "standardContext = {0}",
                new Object[]{standardContext});
        return standardContext;
    }

    private ClassLoader getCommonClassLoader() {
        ClassLoaderHierarchy clh =
                Globals.getDefaultHabitat().getComponent(ClassLoaderHierarchy.class);
        return clh.getAPIClassLoader();
    }

    public void stop(BundleContext context) throws Exception {
        if (serverTracker != null) serverTracker.close();
        for (ServiceRegistration registration : registrations) registration.unregister();
        for (Host vs : vss.values()) {
            StandardContext standardContext =
                    StandardContext.class.cast(vs.findChild(contextPath));
            if (standardContext == null) {
                continue;
            }
            for (Container child : standardContext.findChildren()) {
                standardContext.removeChild(child);
            }
            vs.removeChild(standardContext);
        }
        // TODO(Sahoo): Need to call stop on all wrappers if they are not
        // automatically stopped when removed from context.
    }

    private void switchOffSessionPersistence(StandardContext ctx) {
        // See Jan's blog about how to switch off
        // Session persistence:
        // http://blogs.sun.com/jluehe/entry/how_to_disable_persisting_of
        Manager mgr = ctx.getManager();
        if (mgr == null) {
            mgr = new StandardManager();
            StandardManager.class.cast(mgr).setPathname(null);
            ctx.setManager(mgr);
        } else {
            try {
                StandardManager.class.cast(mgr).setPathname(null);
            } catch (ClassCastException cce) {
                logger.logp(Level.INFO, "Activator", "switchOffSessionPersistence",
                        "SessionManager {0} does not allow path name of session store to be configured.",
                        new Object[]{mgr});
            }
        }
    }

    /**
     * Tracks Habitat and obtains EVents service from it and registers a listener
     * that takes care of doing the actual work.
     */
    private class GlassFishServerTracker extends ServiceTracker {
        public GlassFishServerTracker(BundleContext context) {
            super(context, Habitat.class.getName(), null);
        }

        @Override
        public Object addingService(ServiceReference reference) {
            ServiceReference habitatServiceRef = context.getServiceReference(Habitat.class.getName());
            final Habitat habitat = Habitat.class.cast(context.getService(habitatServiceRef));
            Events events = habitat.getComponent(Events.class);
            EventListener listener = new org.glassfish.api.event.EventListener() {
                public void event(Event event) {
                    if (EventTypes.SERVER_READY.equals(event.type())) {
                        WebContainer wc = habitat.getComponent(WebContainer.class);
                        doActualWork(wc);
                    }
                }
            };
            events.register(listener);
            // We can get into infinite waiting loop if server is already started. So check the status once.
            if (habitat.getComponent(ServerEnvironment.class).getStatus() == ServerEnvironment.Status.started) {
                WebContainer wc = habitat.getComponent(WebContainer.class);
                doActualWork(wc);
            }
            close(); // no need to track any more
            return super.addingService(reference);
        }
    }


    /**
     * @return comma-separated list of all defined virtual servers (including __asadmin)
     */
    private String getAllVirtualServers() {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        Domain domain = Globals.get(Domain.class);
        String target = getInstanceName();
        Server server = domain.getServerNamed(target);
        if (server != null) {
            Config config = server.getConfig();
            if (config != null) {
                com.sun.enterprise.config.serverbeans.HttpService httpService = config.getHttpService();
                if (httpService != null) {
                    List<VirtualServer> hosts = httpService.getVirtualServer();
                    if (hosts != null) {
                        for (VirtualServer host : hosts) {
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

    private String getInstanceName() {
        ServerEnvironment se = Globals.get(ServerEnvironment.class);
        String target = se.getInstanceName();
        return target;
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
            logger.logp(Level.INFO, "Activator", "getDefaultVirtualServer", "defaultVirtualServer = {0}",
                    new Object[]{defaultVirtualServer});
            return defaultVirtualServer;
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

}
