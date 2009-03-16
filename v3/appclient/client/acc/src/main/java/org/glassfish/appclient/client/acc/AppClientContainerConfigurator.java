/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
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
package org.glassfish.appclient.client.acc;

import com.sun.enterprise.module.bootstrap.BootException;
import com.sun.enterprise.util.LocalStringManager;
import com.sun.enterprise.util.LocalStringManagerImpl;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;
import javax.security.auth.callback.CallbackHandler;
import org.glassfish.appclient.client.acc.config.AuthRealm;
import org.glassfish.appclient.client.acc.config.ClientCredential;
import org.glassfish.appclient.client.acc.config.MessageSecurityConfig;
import org.glassfish.appclient.client.acc.config.Property;
import org.glassfish.appclient.client.acc.config.TargetServer;
import org.glassfish.appclient.client.acc.config.util.XML;
import org.jvnet.hk2.component.Habitat;

/**
 * Implements a builder for accumulating configuration information for the
 * app client container and then starting the ACC.
 * <p>
 * The interface for the  ACC builder is defined as AppClientContainer.Configurator so the
 * relevant JavaDoc is concentrated in that one class.
 *<p>
 * The AppClientContainerConfigurator class records the
 * information the container itself needs in order to operate.
 *
 * @author tjquinn
 */
public class AppClientContainerConfigurator implements AppClientContainer.Configurator {

    private static final LocalStringManager localStrings = new LocalStringManagerImpl(AppClientContainerConfigurator.class);
    /** caller-specified target servers */
    private TargetServer[] targetServers;
//    private String[] clientArgs = null;

    /** caller-optional logger  - initialized to logger name from the class; caller can override with logger method */
    private Logger logger = Logger.getLogger(getClass().getName());

    private AuthRealm authRealm = null;

    private Habitat habitat = null;
    
    private ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

//    private String mainClassName = null;
//
//    private Class mainClass = null;

    /**
     * records whether the mainClass was set by the caller or inferred from an archive
     */
//    private boolean isMainClassFromCaller;
//
//    private URI clientURI = null;


    /**
     * The caller can pre-set the client credentials using the
     * <code>clientCredentials</code> method.  The ACC will use the
     * username and realm values in intializing a callback handler if one is
     * needed.
     */
    private ClientCredential clientCredential = null;

    private boolean sendPassword = true;

    /** caller-provided message security configurations */
    private final List<MessageSecurityConfig> messageSecurityConfigs = new ArrayList<MessageSecurityConfig>();

//    private Class<? extends CallbackHandler> callbackHandlerClass = null;

    /**
     * optional caller-specified properties governing the ACC's behavior.
     * Correspond to the property elements available in the client-container
     * element from sun-application-client-containerxxx.dtd.
     */
    private Properties containerProperties = null;

    AppClientContainerConfigurator() {

    }
    
    /**
     * Creates a new configurator with the specified target servers and client URI.
     *
     * @param targetServers the <code>TargetServer</code>s to use
     * @param clientURI the URI of the client archive to launch
     */
    AppClientContainerConfigurator(final TargetServer[] targetServers) {
        this.targetServers = targetServers;
    }

//    private Habitat getHabitat() throws BootException, URISyntaxException {
//        if (habitat == null) {
//            habitat = ACCModulesManager.getHabitat(classLoader, logger);
//        }
//        return habitat;
//    }

    public AppClientContainer newContainer(final Class mainClass,
            final CallbackHandler callerSpecifiedCallbackHandler) throws Exception {
        Launchable client = Launchable.Util.newLaunchable(mainClass);
        AppClientContainer container = createContainer(client, callerSpecifiedCallbackHandler);
        return container;
    }
    
    public AppClientContainer newContainer(final Class mainClass) throws Exception {
        return newContainer(mainClass, null);
    }

    public AppClientContainer newContainer(final URI clientURI,
            final CallbackHandler callerSpecifiedCallbackHandler,
            final String callerSpecifiedMainClassName,
            final String callerSpecifiedAppClientName) throws Exception {

        Launchable client = Launchable.Util.newLaunchable(
                clientURI,
                callerSpecifiedMainClassName,
                callerSpecifiedAppClientName);

        AppClientContainer container = createContainer(client, callerSpecifiedCallbackHandler);
        return container;
    }

    public AppClientContainer newContainer(final URI clientURI) throws Exception {
        return newContainer(clientURI, null, null, null);
    }

    private AppClientContainer createContainer(final Launchable client,
            final CallbackHandler callerSuppliedCallbackHandler) throws BootException, BootException, URISyntaxException, ClassNotFoundException {
        AppClientContainer container = ACCModulesManager.getComponent(AppClientContainer.class);
        container.setClient(client);
        container.setCallbackHandler(callerSuppliedCallbackHandler);
        container.setConfigurator(this);
        return container;
    }


//    /**
//     * Returns an AppClientContainer prepared to execute the app client implied
//     * by the launch info and the app client args.
//     * @param launchInfo info about the launch (type, name)
//     * @param appclientArgs appclient command line arguments
//     * @return
//     */
//    AppClientContainer newContainer(final CommandLaunchInfo launchInfo, final AppclientCommandArguments appclientArgs) {
//        AppClientContainer acc = null;
////        switch (launchInfo.getClientLaunchType()) {
////            case JAR:
////                /*
////                 * The user will have used local file path syntax, so create a
////                 * file first and then get its URI.
////                 */
////                File f = new File(launchInfo.getClientName());
////                acc = newContainer(f.toURI());
////
////                StandAloneAppClientInfo acInfo = new StandAloneAppClientInfo(
////                        false /* isJWS */,
////                        logger,
////                        f, archivist, mainClassFromCommandLine)
////        }
//        return acc;
//    }

    public AppClientContainerConfigurator addMessageSecurityConfig(final MessageSecurityConfig msConfig) {
        messageSecurityConfigs.add(msConfig);
        return this;
    }

    public List<MessageSecurityConfig> getMessageSecurityConfig() {
        return this.messageSecurityConfigs;
    }

    public AppClientContainerConfigurator logger(final Logger logger) {
        this.logger = logger;
        return this;
    }

    public Logger getLogger() {
        return logger;
    }


    public AppClientContainerConfigurator authRealm(final String className) {
        authRealm = new AuthRealm(className);
        return this;
    }

    public AuthRealm getAuthRealm() {
        return authRealm;
    }

    public AppClientContainerConfigurator clientCredentials(final String user, final char[] password) {
        return clientCredentials(user, password, null);
    }

    public AppClientContainerConfigurator clientCredentials(final String user, final char[] password, final String realm) {
//        this.clientCredential = new ClientCredential()
//        this.user = user;
//        this.password = password;
//        this.realmName = realm;
        ClientCredential cc = new ClientCredential(user, new XML.Password(password), realm);
        return clientCredentials(cc);
    }

    public AppClientContainerConfigurator clientCredentials(final ClientCredential cc) {
        clientCredential = cc;
        return this;
    }

    public ClientCredential getClientCredential() {
        return clientCredential;
    }

    public AppClientContainerConfigurator containerProperties(final Properties props) {
        this.containerProperties = props;
        return this;
    }

    public AppClientContainerConfigurator containerProperties(final List<Property> props) {
        containerProperties = XML.toProperties(props);
        return this;
    }

    public Properties getContainerProperties() {
        return containerProperties;
    }

    public AppClientContainerConfigurator sendPassword(final boolean sendPassword){
        this.sendPassword = sendPassword;
        return this;
    }

    public boolean getSendPassword() {
        return sendPassword;
    }

//    public AppClientContainerConfigurator callbackHandler(final Class<? extends CallbackHandler> callbackHandlerClass) {
//        this.callbackHandlerClass = callbackHandlerClass;
//        return this;
//    }

//    public Class<? extends CallbackHandler> getCallbackHandler() {
//        return callbackHandlerClass;
//    }

    public TargetServer[] getTargetServers() {
        return targetServers;
    }

//    public AppClientContainerConfigurator mainClass(Class mainClass) {
//        this.mainClass = mainClass;
//        return this;
//    }

//    public Class getMainClass() {
//        return mainClass;
//    }
//
//    public AppClientContainerConfigurator mainClassName(String mainClassName) {
//        if (isMainClassFromCaller) {
//            throw new IllegalStateException();
//        }
//        this.mainClassName = mainClassName;
//        return this;
//    }
//
//    public String getMainClassName() {
//        return mainClassName;
//    }
//
//    public AppClientContainerConfigurator mainClass(final Class mainClass) {
//        this.mainClass = mainClass;
//        mainClassName = mainClass.getName();
//        return this;
//    }
//
//    public Method getMainMethod() {
//        return mainMethod;
//    }
//
//    private void completeConfig() throws NoSuchMethodException {
//        mainMethod = initMainMethod();
//    }
//

}
