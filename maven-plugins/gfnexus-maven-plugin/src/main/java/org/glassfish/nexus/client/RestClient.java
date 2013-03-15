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
 * https://glassfish.java.net/public/CDDL+GPL_1_1.html
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

package org.glassfish.nexus.client;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Logger;
import javax.net.ssl.SSLContext;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientFactory;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.client.HttpUrlConnector;
import org.glassfish.jersey.client.HttpUrlConnector.ConnectionFactory;
import org.glassfish.jersey.client.SslConfig;
import org.glassfish.jersey.client.filter.HttpBasicAuthFilter;
import org.glassfish.jersey.filter.LoggingFilter;
import org.glassfish.jersey.moxy.json.MoxyJsonFeature;
import org.glassfish.nexus.client.logging.CustomHandler;
import org.glassfish.nexus.client.logging.CustomPrinter;

/**
 *
 * @author Romain Grecourt
 */
public class RestClient {

    private Client client;
    private boolean useSsl = false;
    private String proxyHost;
    private int proxyPort;
    private char[] username;
    private char[] password;
    private CustomHandler handler = null;

    public String getProxyHost() {
        return proxyHost;
    }

    public int getProxyPort() {
        return proxyPort;
    }

    public String getUsername() {
        return new String(username);
    }

    public Client getClient(){
        return client;
    }

    private void initClient(){
        ClientConfig cc = new ClientConfig();

        if(proxyHost!=null && !proxyHost.isEmpty()){
            cc.connector(new HttpUrlConnector(new ConnectionFactory() {
                public HttpURLConnection getConnection(URL url) throws IOException {
                    final InetSocketAddress inetAddr =
                            new InetSocketAddress(InetAddress.getByName(proxyHost), proxyPort);
                    return (HttpURLConnection) url.openConnection(new Proxy(Proxy.Type.HTTP, inetAddr));
                }
            }));
        }

        this.client = ClientFactory.newClient(cc);

        if(useSsl){
            SSLContext context = null;
            try {
                context = SSLContext.getInstance("SSL");
                context.init(null, null, null);
            } catch (NoSuchAlgorithmException ex) {
            } catch (KeyManagementException ex) {
            }
            this.client.configuration().setProperty(ClientProperties.SSL_CONFIG, new SslConfig(context));
        }

        if(username != null
                && username.length > 0
                && password!=null
                && password.length > 0){
            HttpBasicAuthFilter authFilter =
                    new HttpBasicAuthFilter(new String(username), new String(password));
            this.client.configuration().register(authFilter);
        }
        this.client.configuration().register(new MoxyJsonFeature()).register(new JsonMoxyConfigurationContextResolver());

        Logger logger = Logger.getLogger(RestClient.class.getSimpleName());
        if(handler != null){
            logger.setUseParentHandlers(false);
            logger.addHandler(handler);
        }
        this.client.configuration().register(new LoggingFilter(logger, true));
    }

    public RestClient() {
        initClient();
    }

    public RestClient(CustomHandler handler) {
        this.handler = handler;
        initClient();
    }

    public RestClient(boolean useSsl) {
        this.useSsl = useSsl;
        initClient();
    }

    public RestClient(boolean useSsl, CustomHandler handler) {
        this.useSsl = useSsl;
        this.handler = handler;
        initClient();
    }

    public RestClient(final String proxyHost, final int proxyPort){
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        initClient();
    }

    public RestClient(final String proxyHost, final int proxyPort, CustomHandler handler){
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        initClient();
    }

    public RestClient(final String proxyHost, final int proxyPort, boolean  useSsl){
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        this.useSsl = true;
        initClient();
    }
    public RestClient(final String proxyHost, final int proxyPort, boolean  useSsl, CustomHandler handler){
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        this.handler = handler;
        this.useSsl = true;
        initClient();
    }

    public RestClient(String proxyHost, int proxyPort, String username, String password, boolean useSsl) {
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        this.username = username.toCharArray();
        this.password = password.toCharArray();
        this.useSsl = useSsl;
        initClient();
    }

    public RestClient(String proxyHost, int proxyPort, String username, String password, boolean useSsl, CustomPrinter logger) {
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        this.username = username.toCharArray();
        this.password = password.toCharArray();
        this.useSsl = useSsl;
        this.handler = new CustomHandler(logger);
        initClient();
    }

    public RestClient(String proxyHost, int proxyPort, String username, String password) {
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        this.username = username.toCharArray();
        this.password = password.toCharArray();
        initClient();
    }

    public RestClient(String proxyHost, int proxyPort, String username, String password, CustomPrinter logger) {
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        this.username = username.toCharArray();
        this.password = password.toCharArray();
        this.handler = new CustomHandler(logger);
        initClient();
    }
}