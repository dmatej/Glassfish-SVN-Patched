/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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

package com.sun.scn.client.comm;

import com.sun.scn.client.util.SCRKClientHelper;
import com.sun.scn.dao.Domain;
import com.sun.scn.util.XMLErrorSuppressor;

import com.sun.scn.servicetags.SvcTag;
import com.sun.scn.servicetags.SunOnlineAccount;
import com.sun.scn.servicetags.util.XMLUtil;

import org.apache.commons.codec.EncoderException;
import org.apache.commons.codec.binary.Base64;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringReader;

import java.net.Authenticator;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;
import java.net.UnknownHostException;

import java.security.AccessControlException;
import java.security.KeyPair;

import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
import java.util.Locale;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * Sends a registration-related XML message to a catcher.
 */
public class RegSender {
    private static Logger log = Logger.getLogger(RegSender.class.getName());

    /**
     * The name of the System property which allows setting of the product
     * registration URL.
     */
    public static final String URL_PROPERTY_KEY = RegSender.class.getName() +
        ".prsUrl";

    public static final String TARGET_PROPERTY_KEY = RegSender.class.getName() +
        ".target";

    private static final String DEFAULT_URL = "https://inv-cs.sun.com";
    private static final String DEFAULT_URI = "ProductRegistrationService/";
    private static ResourceBundle resources;

    static {
        // Otherwise, we sometimes get a "Pasword Required -- Networking" popup
        Authenticator.setDefault(null);

        Locale locale = Locale.getDefault();
        resources = ResourceBundle.getBundle
            ("com.sun.scn.client.resources.bundle.RegistrationWrapper", locale);
    }

    public static String getBaseUrl() {
        String baseUrl;

        String altTarget = System.getProperty(TARGET_PROPERTY_KEY);

        try {
            ResourceBundle connResources =
                ResourceBundle.getBundle("com.sun.scn.client.resources.Connection");
            if (altTarget != null) {
                baseUrl = connResources.getString(altTarget + ".prs.url");
            } else {
                String target = connResources.getString("target");
                baseUrl = connResources.getString(target + ".prs.url");
            }
        } catch (Exception e) {
            baseUrl = DEFAULT_URL;
        }

        String tmp = System.getProperty(URL_PROPERTY_KEY);

        if (tmp != null) {
            baseUrl = tmp;
        }

        if (baseUrl.endsWith("/")) {
            baseUrl += DEFAULT_URI;
        } else {
            baseUrl += "/" + DEFAULT_URI;
        }

        if (log.isLoggable(Level.FINE))
            log.log(Level.FINE, "Using base url: " + baseUrl);

        return baseUrl;
    }

    private static Document readErrorDocument(InputStream in) {
        // Putting everything into a string is a little wasteful, but it
        // allows a more meaningful error msg if something goes wrong.
        try {
            StringBuilder sb = new StringBuilder();
            String line;
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            boolean bodyFlag = false;
            boolean responseFlag = false;

            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.equals("")) {
                    continue;
                }
                if (line.toUpperCase().indexOf("<RESPONSE>") >= 0 && bodyFlag) {
                    responseFlag = true;
                }
                if (line.toUpperCase().indexOf("<BODY>") >= 0) {
                    bodyFlag = true;
                }
                if (bodyFlag && responseFlag) {
                    sb.append(line);
                    sb.append("\n");
                }
                if (line.toUpperCase().indexOf("</BODY>") >= 0) {
                    bodyFlag = false;
                }

                if (line.toUpperCase().indexOf("</RESPONSE>") >= 0) {
                    responseFlag = false;
                }

            }
            //System.out.println(sb.toString());
            log.fine("ERROR RESPONSE: " + sb.toString());

            try {
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                DocumentBuilder builder = factory.newDocumentBuilder();

                return builder.parse(new InputSource(
                        new StringReader(sb.toString())));
            } catch (ParserConfigurationException pce) {
                //throw new RuntimeException(pce);
                log.log(Level.FINE, "Encountered error: " + pce.getMessage(), pce);
                return null;
            } catch (SAXException se) {
                //throw new RuntimeException(se);
                log.log(Level.FINE, "Encountered error: " + se.getMessage(), se);
                return null;
            }
        } catch (IOException ioe) {
            log.log(Level.FINE, "Encountered error: " + ioe.getMessage(), ioe);
            return null;
        }
    }

    private static Document readDocument(InputStream in)
        throws IOException {
        // Putting everything into a string is a little wasteful, but it
        // allows a more meaningful error msg if something goes wrong.
        StringBuilder sb = new StringBuilder();
        String line;
        BufferedReader bin = new BufferedReader(new InputStreamReader(in));

        while ((line = bin.readLine()) != null) {
            sb.append(line);
            sb.append("\n");
        }

        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();

            return builder.parse(new InputSource(
                    new StringReader(sb.toString())));
        } catch (ParserConfigurationException e) {
            throw new RuntimeException(e);
        } catch (SAXException e) {
            //System.err.println("offending xml: " + sb.toString());
            if (log.isLoggable(Level.FINEST))
                log.log(Level.FINEST, "exception", e);
            throw new RuntimeException(e);
        }
    }

    public static boolean isSvcTagRegistered(String serviceTagURN,
            String proxyHost, int proxyPort)
            throws SvcTagException, ConnectException, UnknownHostException {
        try {
            URL url = new URL(
                getBaseUrl()
                + "status/"
                + serviceTagURN);
            HttpURLConnection con = null;
            if ((proxyHost != null) && !proxyHost.equals("") &&
                    (proxyPort != -1)) {
                Proxy proxy = new Proxy(Proxy.Type.HTTP,
                        new InetSocketAddress(proxyHost, proxyPort));
                con = (HttpURLConnection) (url.openConnection(proxy));
            } else {
                con = (HttpURLConnection) (url.openConnection(Proxy.NO_PROXY));
            }

            con.setRequestMethod("GET");
            con.setDoOutput(true);
            con.setDoInput(true);
            con.setAllowUserInteraction(false);
            con.setUseCaches(false);

            con.connect();
            int responseCode = con.getResponseCode();

            if (responseCode == 200) {
                BufferedReader reader = new BufferedReader(
                    new InputStreamReader(con.getInputStream()));
                StringBuffer sb = new StringBuffer();

                while (true) {
                    String line = reader.readLine();

                    if (line == null) {
                        break;
                    }

                    sb.append(line);
                }

                reader.close();

                String response = sb.toString();

                if (response.equals("REGISTERED")) {
                    return true;
                } else if (response.equals("NOT REGISTERED")) {
                    return false;
                }
            }
        } catch (IOException ioe) {
            if (log.isLoggable(Level.FINEST))
                log.log(Level.FINEST, "exception", ioe);
            ioe.printStackTrace();
        }
        return false;
    }

    public static boolean isSvcTagRegistered(String serviceTagURN)
            throws SvcTagException, ConnectException, UnknownHostException {
        return isSvcTagRegistered(serviceTagURN, null, -1);
    }

    public static void createSunOnlineAccount(SunOnlineAccount soa)
            throws SvcTagException {
        createSunOnlineAccount(soa, null, -1);
    }

    public static void createSunOnlineAccount(SunOnlineAccount soa, Proxy proxy)
            throws SvcTagException {
        try {
            if (proxy != null && proxy.type() == Proxy.Type.HTTP) {
                InetSocketAddress addr = (InetSocketAddress) proxy.address();
                createSunOnlineAccount(soa, addr.getHostName(), addr.getPort());
                return;
            }
        } catch (Exception e) {
        }
        createSunOnlineAccount(soa, null, -1);
    }

    public static void createSunOnlineAccount(SunOnlineAccount soa,
            String proxyHost, int proxyPort) throws SvcTagException {

        if (log.isLoggable(Level.FINER))
            log.log(Level.FINER, "Creating sun online account");
        // not sure that this really belongs here, but for now, we'll hard
        // code this in
        soa.setTouVersion("SMI_TOU_1.2");
        soa.setTouResponse("Accepted");

        Document response =
            doHttpOp("POST", getBaseUrl(), "soa", soa.toXMLRequestString(), null, null, proxyHost, proxyPort);

        if (response == null) {
            if (log.isLoggable(Level.FINEST))
                log.log(Level.FINEST, "Null response from post");
            throw new SvcTagException(SvcTagException.GENERAL_ERROR);
        }

        NodeList responses = response.getElementsByTagName(
                "response");

        if (responses != null && responses.getLength() > 0) {
            Element e = (Element) (responses.item(0));
            String status = XMLUtil.getOptionalTextValue(e, "status");
            String detail = XMLUtil.getOptionalTextValue(e, "detail");
            if (status != null && status.equals("success")) {
                if (log.isLoggable(Level.FINEST))
                    log.log(Level.FINEST, "Successfully created account");
                // this is the only good state out of this method
                return;
            }
            if (status == null || detail == null) {
                throw new SvcTagException(SvcTagException.GENERAL_ERROR);
            }

            // this is a brittle way to handle errors, but at
            // this time, we don't get individual exceptions when creating
            // a user, so the SOAServlet sends along the message and we're
            // left to decode it
            if (detail.contains("VALIDATION_ERROR")) {
                if (detail.contains("Email Address must be in a valid format")) {
                    throw new SvcTagException(SvcTagException.INVALID_EMAIL_FORMAT,
                        resources.getString("create.soa.invalid.email"));
                } else if (detail.contains("Password must be at least 6 characters")) {
                    throw new SvcTagException(SvcTagException.PASSWORD_TOO_SHORT,
                        resources.getString("create.soa.invalid.password"));
                }
            } else if (detail.contains("DRPL_MATCH")) {
                throw new SvcTagException(SvcTagException.ERROR_CREATING_ACCOUNT,
                    resources.getString("create.soa.error"));
            } else if (detail.contains("InvalidUsernameException")) {
                throw new SvcTagException(SvcTagException.ERROR_CREATING_ACCOUNT,
                    resources.getString("create.soa.invalid.userid"));
            } else if (detail.contains("DUPLICATE_USERNAME")) {
                throw new SvcTagException(SvcTagException.USERNAME_ALREADY_EXISTS,
                    resources.getString("create.soa.username.exists.general"));
            } else if (detail.contains("Invalid Country Name")) {
                throw new SvcTagException(SvcTagException.INVALID_COUNTRY,
                    resources.getString("create.soa.invalid.country"));
            } else if (detail.contains("Duplicate Screen Name")) {
                throw new SvcTagException(SvcTagException.SCREENNAME_ALREADY_EXISTS,
                    resources.getString("create.soa.screenname.exists.general"));
            }
        }

        // default fall through behavior is the exception
        throw new SvcTagException(SvcTagException.GENERAL_ERROR);
    }

    public static List<Domain> getUserDomains(String soaId, KeyPair keyPair,
        String clientId) throws SvcTagException {
        return getUserDomains(soaId, null, -1, keyPair, clientId);
    }

    public static List<Domain> getUserDomains(String soaId,
            KeyPair keyPair, String clientId, Proxy proxy)
            throws SvcTagException {
        try {
            if (proxy != null && proxy.type() == Proxy.Type.HTTP) {
                InetSocketAddress addr = (InetSocketAddress) proxy.address();
                return getUserDomains(soaId, addr.getHostName(), addr.getPort(), keyPair, clientId);
            }
        } catch (Exception e) {
        }
        return getUserDomains(soaId, null, -1, keyPair, clientId);
    }

    public static List<Domain> getUserDomains(String soaId, String proxyHost,
            int proxyPort, KeyPair keyPair, String clientId) throws SvcTagException {
        String destBaseUrl = getBaseUrl();

        List<Domain> domains = new ArrayList<Domain>();
        if (log.isLoggable(Level.FINER))
            log.log(Level.FINER, "Getting domains for: " + soaId);

        Document doc = doHttpOp("GET", destBaseUrl, "domain/" + soaId,
            soaId, keyPair, clientId, proxyHost, proxyPort);

        if (doc != null) {
            NodeList list = doc.getElementsByTagName("domain");

            for (int i = 0; i < list.getLength(); i++) {
                try {
                    Domain d = new Domain((Element) list.item(i));
                    domains.add(d);
                    if (log.isLoggable(Level.FINER))
                        log.log(Level.FINER, "found role/domain: " + d.getDomainName());
                } catch (Exception e) {
                    if (log.isLoggable(Level.FINE))
                        log.log(Level.FINE, "Error: " + e.getMessage(), e);
                }
            }
        } else {
            if (log.isLoggable(Level.FINER))
                log.log(Level.FINER, "No domains found for: " + soaId);
        }

        return domains;
    }

    public static Domain getExplorerDomain(String soaId, KeyPair keyPair,
            String clientId) throws SvcTagException {
        return getExplorerDomain(soaId, null, -1, keyPair, clientId);
    }

    public static Domain getExplorerDomain(String soaId, String proxyHost,
            int proxyPort, KeyPair keyPair, String clientId) throws SvcTagException {
        String destBaseUrl = getBaseUrl();

        Domain domain = null;
        if (log.isLoggable(Level.FINER))
            log.log(Level.FINER, "Getting explorer domain for: " + soaId);

        Document doc = doHttpOp("GET", destBaseUrl, "explorer_domain/" + soaId,
            soaId, keyPair, clientId, proxyHost, proxyPort);

        if (doc != null) {
            NodeList list = doc.getElementsByTagName("domain");

            for (int i = 0; i < list.getLength(); i++) {
                try {
                    Domain d = new Domain((Element) list.item(i));
                    if (d.getDomainName().toLowerCase().startsWith("$#explorer_")
                            || d.getDomainName().toLowerCase().startsWith("$explorer_")) {
                        domain = d;
                        if (log.isLoggable(Level.FINER))
                            log.log(Level.FINER, "found explorer_domain: "
                                + domain.getDomainName());
                        break;
                    }
                } catch (Exception e) {
                    if (log.isLoggable(Level.FINE))
                        log.log(Level.FINE, "Error: " + e.getMessage(), e);
                }
            }
        } else {
            if (log.isLoggable(Level.FINER))
                log.log(Level.FINER, "No explorer domain found for: " + soaId);
        }

        return domain;
    }

    public static SvcTag getSvcTag(String destUrlPath, KeyPair keyPair,
        String clientId, int domainId) throws SvcTagException {
        return getSvcTag(destUrlPath, null, -1, keyPair, clientId, domainId);
    }

    public static SvcTag getSvcTag(String destUrlPath, String proxyHost,
            int proxyPort, KeyPair keyPair, String clientId, int domainId)
            throws SvcTagException {
        String destBaseUrl = getBaseUrl();
        SvcTagException ste = null;

        SvcTag st = null;
        HttpURLConnection con = null;

        if (log.isLoggable(Level.FINER))
            log.log(Level.FINER, "Getting svctag information: " + destUrlPath);

        try {
            URL url = new URL(destBaseUrl + destUrlPath);

            if ((proxyHost != null) && !proxyHost.equals("") &&
                    (proxyPort != -1)) {
                if (log.isLoggable(Level.FINE))
                    log.log(Level.FINE, "Using proxy: " + proxyHost + ":" + proxyPort);

                Proxy proxy = new Proxy(Proxy.Type.HTTP,
                        new InetSocketAddress(proxyHost, proxyPort));
                con = (HttpURLConnection) (url.openConnection(proxy));
            } else {
                con = (HttpURLConnection) (url.openConnection(Proxy.NO_PROXY));
            }

            con.setDoInput(true);
            con.setDoOutput(true);
            con.setUseCaches(false);
            con.setAllowUserInteraction(false);
            con.setRequestMethod("GET");
            con.setRequestProperty("Content-Type", "text/xml; charset=\"utf-8\"");
            con.setRequestProperty("User-Agent", "ServiceTag Collector");

            String payload = "" + domainId;
            String signature = new String(SCRKClientHelper.signPayload(
                        payload, keyPair.getPrivate()));

            // SCRK params to pass
            con.setRequestProperty("client_reg_id", clientId);
            con.setRequestProperty("payload_sig", signature);
            con.setRequestProperty("payload", payload);

            con.connect();

            if (con.getResponseCode() == HttpURLConnection.HTTP_UNAUTHORIZED) {
                if (log.isLoggable(Level.FINE))
                    log.log(Level.FINE, "unauthorized response code: " + con.getResponseCode());
                ste = new SvcTagException
                               (SvcTagException.INVALID_USERNAME_PASSWORD);
            } else if (con.getResponseCode() >= 400) {
                if (log.isLoggable(Level.FINE)) {
                    if (con.getResponseCode() == 400) {
                        log.log(Level.FINE, "Service tag not found or registered");
                    } else {
                        log.log(Level.FINE, "error response code: " + con.getResponseCode());
                    }
                }
                ste = new SvcTagException(SvcTagException.HTTP_ERROR,
                                    readErrorDocument(con.getErrorStream()));
            } else {
                Document response = readDocument(con.getInputStream());
                NodeList svcTagNodes = response.getElementsByTagName(
                        "service_tag");

                for (int j = 0; j < svcTagNodes.getLength(); j++) {
                    Element svcTagElement = (Element) (svcTagNodes.item(j));
                    st = new SvcTag(XMLUtil.getRequiredTextValue(
                                svcTagElement, "instance_urn"));
                    st.setState(svcTagElement);
                    if (log.isLoggable(Level.FINEST))
                        log.log(Level.FINEST, "found svctag: " + st.toXMLString());

                    break;
                }
            }
        } catch (ConnectException ce) {
            if (log.isLoggable(Level.FINE))
                log.log(Level.FINE, "connect exception: " + ce.getMessage(), ce);
            ste = new SvcTagException(SvcTagException.CONNECTION_ERROR);
        } catch (IOException ioe) {
            if (log.isLoggable(Level.FINE))
                log.log(Level.FINE, "io exception: " + ioe.getMessage(), ioe);
            ste = new SvcTagException
                      (SvcTagException.IO_ERROR, ioe.getMessage());
        } finally {
            try {
                //con.disconnect();
            } catch (Exception e) {
            }
        }

        if (ste != null) {
            throw ste;
        }
 
        return st;
    }

    public static Document sendPut(Element e, String destUrlPath,
        String proxyHost, int proxyPort, KeyPair keyPair, String clientId)
            throws SvcTagException {
        return sendElement(e, destUrlPath, proxyHost, proxyPort, keyPair,
            clientId, "PUT");
    }

    public static Document sendPut(Element e, String destUrlPath,
        KeyPair keyPair, String clientId) throws SvcTagException {
        return sendElement(e, destUrlPath, null, -1, keyPair, clientId, "PUT");
    }
    public static Document sendPut(Element e, String destUrlPath,
            KeyPair keyPair, String clientId, Proxy proxy)
            throws SvcTagException {
        try {
            if (proxy != null && proxy.type() == Proxy.Type.HTTP) {
                InetSocketAddress addr = (InetSocketAddress) proxy.address();
                return sendElement(e, destUrlPath, addr.getHostName(), addr.getPort(),
                    keyPair, clientId, "PUT");
            }
        } catch (Exception ex) {
        }
        return sendElement(e, destUrlPath, null, -1, keyPair, clientId, "PUT");
    }

    public static Document sendPost(Element e, String destUrlPath,
        String proxyHost, int proxyPort, KeyPair keyPair, String clientId)
            throws SvcTagException {
        return sendElement(e, destUrlPath, proxyHost, proxyPort, keyPair,
            clientId, "POST");
    }

    public static Document sendPost(Element e, String destUrlPath,
        KeyPair keyPair, String clientId) throws SvcTagException {
        return sendElement(e, destUrlPath, null, -1, keyPair, clientId, "POST");
    }

    private static Document sendElement(Element e, String destUrlPath,
        String proxyHost, int proxyPort, KeyPair keyPair, String clientId,
        String requestMethod) throws SvcTagException {
        String destBaseUrl = getBaseUrl();
        HttpURLConnection con = null;
        SvcTagException ste = null;
        Document response = null;

        try {
            URL url = new URL(destBaseUrl + destUrlPath);
            if ((proxyHost != null) && !proxyHost.equals("") && 
                                                  (proxyPort != -1)) {
                if (log.isLoggable(Level.FINE))
                    log.log(Level.FINE, "Using proxy: " + proxyHost + ":" + proxyPort);

                Proxy proxy = new Proxy(Proxy.Type.HTTP,
                    new InetSocketAddress(proxyHost, proxyPort));
                con = (HttpURLConnection) (url.openConnection(proxy));
            } else {
                con = (HttpURLConnection) (url.openConnection(Proxy.NO_PROXY));
            }

            if (log.isLoggable(Level.FINE))
                log.log(Level.FINE, "Sending " + requestMethod + "to: " + url);

            con.setDoInput(true);
            con.setDoOutput(true);
            con.setUseCaches(false);
            con.setAllowUserInteraction(false);
            //con.setRequestMethod("PUT");
            con.setRequestMethod(requestMethod);
            con.setRequestProperty("Content-Type", "text/xml; charset=\"utf-8\"");
            con.setRequestProperty("User-Agent", "ServiceTag Collector");

            ByteArrayOutputStream payloadWriter = new ByteArrayOutputStream();
            XMLUtil.writeElement(e, payloadWriter);

            String payload = new String(payloadWriter.toByteArray());
            // hack for window's based systems -- the conversion above inserts
            // extra characters that aren't transmitted and we get signature
            // exceptions
            payload = payload.replace("\r", "");
    
            String signature = new String(SCRKClientHelper.signPayload(payload,
                        keyPair.getPrivate()));

            if (log.isLoggable(Level.FINER))
                log.log(Level.FINER, "Sending payload: " + payload);

    
            // SCRK params to pass
            con.setRequestProperty("client_reg_id", clientId);
            con.setRequestProperty("payload_sig", signature);

            //System.out.print(payload);
            con.connect();

            OutputStream urlOut = con.getOutputStream();
            urlOut.write(payload.getBytes());
            urlOut.flush();
            urlOut.close();
    
            if (log.isLoggable(Level.FINER)) {
                log.log(Level.FINER, "Response code: " + con.getResponseCode());
                log.log(Level.FINER, "Response message: " + con.getResponseMessage());
            }
            if (con.getResponseCode() == HttpURLConnection.HTTP_UNAUTHORIZED) {
                ste = new SvcTagException
                               (SvcTagException.INVALID_USERNAME_PASSWORD);
                log.log(Level.FINE, "Invalid username/pwd");
            
                throw ste;
                //throw new SvcTagException(SvcTagException.INVALID_USERNAME_PASSWORD);
                //throw new AccessControlException("Invalid SOA username/password");
            } else if (con.getResponseCode() >= 400) {
                log.log(Level.FINE, "Invalid response code: " + con.getResponseCode());
                //System.err.println(con.getResponseCode());
                ste = new SvcTagException(SvcTagException.HTTP_ERROR,
                                    readErrorDocument(con.getErrorStream()));
                throw ste;
            } else {
                response = readDocument(con.getInputStream());
            }
        } catch (UnknownHostException uhe) {
            log.log(Level.FINE, "unknown host exception", uhe);
            ste = new SvcTagException(SvcTagException.CONNECTION_ERROR);
        } catch (ConnectException ce) {
            log.log(Level.FINE, "connect exception", ce);
            ste = new SvcTagException(SvcTagException.CONNECTION_ERROR);
        } catch (IOException ioe) {
            log.log(Level.FINE, "io exception", ioe);
            ste = new SvcTagException
                      (SvcTagException.IO_ERROR, ioe.getMessage());
        } finally {
            try {
                //con.disconnect();
            } catch (Exception ex) {
            }
        }

        if (ste != null) {
            throw ste;
        }

        return response;
    }

    private static Document doHttpOp(String operation, String baseUrl,
        String uri, String payload, KeyPair keyPair, String clientId,
        String proxyHost, int proxyPort) throws SvcTagException {
        try {
            String targetURL = baseUrl + uri;
            URL url = new URL(targetURL);
            HttpURLConnection con = null;
            if ((proxyHost != null) && !proxyHost.equals("") &&
                    (proxyPort != -1)) {
                if (log.isLoggable(Level.FINE))
                    log.log(Level.FINE, "Using proxy: " + proxyHost + ":" + proxyPort);

                Proxy proxy = new Proxy(Proxy.Type.HTTP,
                        new InetSocketAddress(proxyHost, proxyPort));
                con = (HttpURLConnection) (url.openConnection(proxy));
            } else {
                con = (HttpURLConnection) (url.openConnection(Proxy.NO_PROXY));
            }

            con.setRequestMethod(operation);
            con.setDoOutput(true);
            con.setDoInput(true);
            con.setAllowUserInteraction(false);
            con.setUseCaches(false);

            if (log.isLoggable(Level.FINE))
                log.log(Level.FINE, "Sending " + operation + "to: " + url);

            con.setRequestProperty("User-Agent", "User Interface");
            con.setRequestProperty("Content-Type", "text/xml;charset=\"utf-8\"");

            if (keyPair != null && clientId != null) {
                String signature = new String(SCRKClientHelper.signPayload(
                        payload, keyPair.getPrivate()));

                // SCRK params to pass
                con.setRequestProperty("client_reg_id", clientId);
                con.setRequestProperty("payload_sig", signature);
                con.setRequestProperty("payload", payload);
            }

            //System.out.println("Attempting to connect to: " + targetURL);
            con.connect();

            if ((payload != null) &&
                    (operation.equals("POST") || operation.equals("PUT"))) {
                if (log.isLoggable(Level.FINER))
                    log.log(Level.FINER, "Payload: " + payload);
                // write to connection
                OutputStream urlOut = con.getOutputStream();
                urlOut.write(payload.getBytes());
                urlOut.flush();
                urlOut.close();
            }

            int responseCode = con.getResponseCode();

            if (log.isLoggable(Level.FINER)) {
                log.log(Level.FINER, "Response code: " + con.getResponseCode());
                log.log(Level.FINER, "Response message: " + con.getResponseMessage());
            }

            //System.out.println("Response code = " + responseCode);
            if (responseCode < 400) {
                BufferedReader reader = new BufferedReader(new InputStreamReader(
                            con.getInputStream()));
                StringBuffer sb = new StringBuffer();

                while (true) {
                    String line = reader.readLine();

                    if (line == null) {
                        break;
                    }

                    sb.append(line);
                }

                reader.close();

                String response = sb.toString();
                if (log.isLoggable(Level.FINEST))
                    log.log(Level.FINEST, "Response: " + response);

                //System.out.println("response: " + response);
                try {
                    DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
                    DocumentBuilder dbuilder = dbfactory.newDocumentBuilder();

                    return dbuilder.parse(new InputSource(
                            new StringReader(response)));
                } catch (ParserConfigurationException pce) {
                    if (log.isLoggable(Level.FINEST))
                        log.log(Level.FINEST, "exception while configuring parser", pce);
                } catch (SAXException ex) {
                    if (log.isLoggable(Level.FINEST))
                        log.log(Level.FINEST, "sax exception", ex);
                }
            } else {
                BufferedReader br = new BufferedReader(new InputStreamReader(
                            con.getErrorStream()));
                StringBuffer sb = new StringBuffer();
                String line = null;
                boolean flag = false;

                while ((line = br.readLine()) != null) {
                    if (line.indexOf("<HTML>") >= 0) {
                        flag = true;
                        line = line.substring(line.indexOf("<HTML>"));
                    }
                    if (line.indexOf("<HEAD>") >= 0) {
                        flag = false;
                        line = line.substring(line.indexOf("<HEAD>"));
                    }
                    if (line.indexOf("</HEAD>") >= 0) {
                        flag = true;
                        line = line.substring(line.indexOf("</HEAD>")+7);
                    }

                    if (flag) {
                        sb.append(line);
                        sb.append("\n");

                        continue;
                    }
                }

                br.close();

                String response = sb.toString();

                //System.out.println("error response: " + response);
                if (log.isLoggable(Level.FINEST))
                    log.log(Level.FINEST, "Error Response: " + response);
                try {
                    DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
                    DocumentBuilder dbuilder = dbfactory.newDocumentBuilder();
                    dbuilder.setErrorHandler(new XMLErrorSuppressor());

                    return dbuilder.parse(new InputSource(
                            new StringReader(response)));
                } catch (ParserConfigurationException pce) {
                    if (log.isLoggable(Level.FINEST))
                        log.log(Level.FINEST, "parser configuration exception", pce);
                } catch (SAXException ex) {
                    if (log.isLoggable(Level.FINEST))
                        log.log(Level.FINEST, "sax exception", ex);
                }
            }
        } catch (ConnectException ce) {
            if (log.isLoggable(Level.FINE))
                log.log(Level.FINE, "connect exception: " + ce.getMessage(), ce);
            throw new SvcTagException(SvcTagException.CONNECTION_ERROR, ce.getMessage());
        } catch (UnknownHostException uhe) {
            if (log.isLoggable(Level.FINEST))
                log.log(Level.FINEST, "exception", uhe);
            throw new SvcTagException(SvcTagException.UNKNOWN_HOST, uhe.getMessage());
        } catch (MalformedURLException mue) {
            if (log.isLoggable(Level.FINEST))
                log.log(Level.FINEST, "exception", mue);
            throw new SvcTagException(SvcTagException.MALFORMED_URL, mue.getMessage());
        } catch (IOException ex) {
            if (log.isLoggable(Level.FINEST))
                log.log(Level.FINEST, "exception", ex);
        }

        return null;
    }

    /*
     * Gets the HTTP header string which should be used to authenticate to
     * the Catcher.
     */
    private static String getAuthHeader(String user, String pass) {
        Base64 base64 = new Base64();
        String s = user + ":" + pass;
        String header = "Basic " + new String(base64.encode(s.getBytes()));

        return header;
    }
}
