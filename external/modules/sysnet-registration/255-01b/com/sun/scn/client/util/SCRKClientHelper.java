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

package com.sun.scn.client.util;

import org.apache.commons.codec.binary.Base64;

import java.net.HttpURLConnection;
import java.net.URL;
import java.net.Proxy;
import java.net.InetSocketAddress;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;

import java.security.InvalidKeyException;
import java.security.KeyPair;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;

import java.util.ResourceBundle;
import java.util.UUID;

import java.util.logging.Logger;
import java.util.logging.Level;

public class SCRKClientHelper {
    private static Logger log = Logger.getLogger(SCRKClientHelper.class.getName());
    /**
     * The name of the System property which allows setting of CRS URL.
     */
    public static final String URL_PROPERTY_KEY = SCRKClientHelper.class.getName() +
        ".crsUrl";

    public static final String TARGET_PROPERTY_KEY = SCRKClientHelper.class.getName() +
        ".target";

    // Signature algorithm used by SCRK
    private static final String SCRK_SIG_ALG = "SHA1withRSA";
    private static final String DEFAULT_URL = "https://cns-services.sun.com/";
    private static final String DEFAULT_URI = "SCRK/ClientRegistrationV1_1_0";
    private static final String DEFAULT_DEREG_URI = "SCRK/ClientUnregisterV1_2_0";

    // HTTP POST parameter names for the ClientRegistraton request.
    private static final String NAME_CLIENT_REG_ID = "SC_CLIENT_REG_ID";
    private static final String NAME_ASSET_ID = "ASSET_ID";
    private static final String NAME_PUB_KEY = "PUBLIC_KEY";
    private static final String NAME_SOA_ID = "SOA_ID";
    private static final String NAME_SOA_PW = "SOA_PW";

    // HTTP POST parameter names for the ClientUnRegistraton request.
    private static final String NAME_MSGSIG_ID = "MSG_SIGNATURE";

    /**
     * Gets a client registration id for the SCRK services.  If one hasn't already
     * been created, this method will register the user (and associated public key)
     * with the SCRK service and return the generated client registration id.
     */
    public String getClientRegId(KeyPair keyPair, String soaUsername,
            String soaPassword, String proxyHost, int proxyPort) throws Exception {
        return registerClient(keyPair, soaUsername, soaPassword, proxyHost, proxyPort);
    }

    public String getClientRegId(KeyPair keyPair, String soaUsername,
            String soaPassword) throws Exception {
        return registerClient(keyPair, soaUsername, soaPassword, null, -1);
    }

    public String getClientRegId(KeyPair keyPair, String soaUsername,
            String soaPassword, Proxy proxy) throws Exception {
        try {
            if (proxy != null && proxy.type() == Proxy.Type.HTTP) {
                InetSocketAddress addr = (InetSocketAddress) proxy.address();
                return registerClient(keyPair, soaUsername, soaPassword,
                    addr.getHostName(), addr.getPort());
            }
        } catch (Exception ex) {
        }
        return registerClient(keyPair, soaUsername, soaPassword, null, -1);
    }

    public void deleteClientRegId(KeyPair keyPair, String clientRegId,
            String proxyHost, int proxyPort) {
        try {
            unregisterClient(keyPair, clientRegId, proxyHost, proxyPort);
        } catch (Exception e) {
        }
    }

    public void deleteClientRegId(KeyPair keyPair, String clientRegId) {
        deleteClientRegId(keyPair, clientRegId, null, -1);
    }

    public void deleteClientRegId(KeyPair keyPair, String clientRegId,
            Proxy proxy) {
        try {
            if (proxy != null && proxy.type() == Proxy.Type.HTTP) {
                InetSocketAddress addr = (InetSocketAddress) proxy.address();
                deleteClientRegId(keyPair, clientRegId,
                    addr.getHostName(), addr.getPort());
                return;
            }
        } catch (Exception ex) {
        }
        deleteClientRegId(keyPair, clientRegId, null, -1);
    }

    /**
     * Get a key pair generator that uses SCRK key formats.
     */
    public KeyPair getKeyPair() {
        try {
            return KeyGenerator.generateKeyPair();
        } catch (NoSuchAlgorithmException nsae) {
            throw new RuntimeException("Error Generating KeyPair for communication with Sun Inventory: ",
                nsae);
        }
    }

    /**
     * Signs the given payload using the given private key.
     */
    public static byte[] signPayload(String payload, PrivateKey privateKey) {
        try {
            Signature sig = Signature.getInstance(SCRK_SIG_ALG);
            sig.initSign(privateKey);

            byte[] bytes = payload.getBytes();
            sig.update(bytes, 0, bytes.length);

            return Base64.encodeBase64(sig.sign());
        } catch (NoSuchAlgorithmException nsax) {
            // exception should be handled appropriately
            throw new RuntimeException("Failed to construct Signature", nsax);
        } catch (InvalidKeyException ikx) {
            // exception should be handled appropriately
            throw new RuntimeException("Failed to init Signature with PrivateKey",
                ikx);
        } catch (SignatureException sx) {
            // exception should be handled appropriately
            throw new RuntimeException("Failed to generate Signature", sx);
        }
    }

    public static String getBaseUrl() {
        String baseUrl;

        String altTarget = System.getProperty(TARGET_PROPERTY_KEY);

        try {
            ResourceBundle resources =
                ResourceBundle.getBundle("com.sun.scn.client.resources.Connection");
            if (altTarget != null) {
                baseUrl = resources.getString(altTarget + ".scrk.url");
            } else {
                String target = resources.getString("target");
                baseUrl = resources.getString(target + ".scrk.url");
            }
        } catch (Exception e) {
            //e.printStackTrace();
            baseUrl = DEFAULT_URL;
        }

        // for now, we'll still support the old model of updating the
        // registration service
        String tmp = System.getProperty(URL_PROPERTY_KEY);

        if (tmp != null) {
            baseUrl = tmp;
        }

        if (!baseUrl.endsWith(DEFAULT_URI)) {
            if (baseUrl.endsWith("/")) {
                baseUrl += DEFAULT_URI;
            } else {
                baseUrl += "/" + DEFAULT_URI;
            }
        }

        return baseUrl;
    }

    public static String getBaseDeregisterUrl() {
        String baseUrl;

        String altTarget = System.getProperty(TARGET_PROPERTY_KEY);

        try {
            ResourceBundle resources =
                ResourceBundle.getBundle("com.sun.scn.client.resources.Connection");
            if (altTarget != null) {
                baseUrl = resources.getString(altTarget + ".scrk.url");
            } else {
                String target = resources.getString("target");
                baseUrl = resources.getString(target + ".scrk.url");
            }
        } catch (Exception e) {
            //e.printStackTrace();
            baseUrl = DEFAULT_URL;
        }

        // for now, we'll still support the old model of updating the
        // registration service
        String tmp = System.getProperty(URL_PROPERTY_KEY);

        if (tmp != null) {
            baseUrl = tmp;
        }

        if (!baseUrl.endsWith(DEFAULT_DEREG_URI)) {
            if (baseUrl.endsWith("/")) {
                baseUrl += DEFAULT_DEREG_URI;
            } else {
                baseUrl += "/" + DEFAULT_DEREG_URI;
            }
        }

        return baseUrl;
    }

    private void unregisterClient(KeyPair keyPair, String clientRegId,
            String proxyHost, int proxyPort) throws Exception {
        log.log(Level.FINER, "Deregistering client with scrk");
        HttpURLConnection crsClient = null;
        URL url = new URL(getBaseDeregisterUrl());

        if ((proxyHost != null) && !proxyHost.equals("") &&
                    (proxyPort != -1)) {
            log.log(Level.FINER, "Setting proxy to " + proxyHost + ":" + proxyPort);
            Proxy proxy = new Proxy(Proxy.Type.HTTP,
                new InetSocketAddress(proxyHost, proxyPort));
            crsClient = (HttpURLConnection) (url.openConnection(proxy));
        } else {
            crsClient = (HttpURLConnection) (url.openConnection(Proxy.NO_PROXY));
        }

        String boundary = "---------------------------84028462148935";

        crsClient.setDoInput(true);
        crsClient.setDoOutput(true);
        crsClient.setUseCaches(false);
        crsClient.setAllowUserInteraction(false);
        crsClient.setRequestMethod("POST");
        crsClient.setRequestProperty("Content-Type",
            "multipart/form-data; boundary=" + boundary);
        // Compose the parts which make up the 'unregistration'
        // message that will be POSTed to the SCRK unregistration
        // service:
        //
        // 1. clientRegId
        // 2. signature for clientRegId

        String payload = getContent(boundary, NAME_CLIENT_REG_ID, clientRegId)
            + getContent(boundary, NAME_MSGSIG_ID, new String(signPayload(clientRegId, keyPair.getPrivate())))
            + "--" + boundary + "--\r\n";

        // POST the request
        String body = null;
        crsClient.connect();

        OutputStream urlOut = crsClient.getOutputStream();
        urlOut.write(payload.getBytes());
        urlOut.flush();
        urlOut.close();

        int statusCode = crsClient.getResponseCode();
        log.log(Level.FINER, "crs response code: " + statusCode);

        if (statusCode == 200) {
            // the POST made it to the servlet and was
            // processed, extract the response body
            body = getResponseBodyAsString(crsClient);
        } else {
            body = getErrorResponseBodyAsString(crsClient);
            throw new Exception(
                "Sun Inventory Client Deregistration Error: Fatal server error encountered, status code: "
                + statusCode);
        }
        crsClient.disconnect();

        // cleanup
        //clientRegId = null;
        //keyPair = null;
    }

    /*
     * Register with the SCRK ClientRegistrationService.
     *
     * Sends this client's PublicKey to the SCRK backend.
     *
     * The SC_CLIENT_REG_ID will be returned in the body of
     * the response message.
     */
    private String registerClient(KeyPair keyPair, String soaUser,
            String soaPass, String proxyHost, int proxyPort) throws Exception {
        log.log(Level.FINER, "Registering client with scrk");
        HttpURLConnection crsClient = null;
        URL url = new URL(getBaseUrl());

        if ((proxyHost != null) && !proxyHost.equals("") &&
                    (proxyPort != -1)) {
            log.log(Level.FINER, "Setting proxy to " + proxyHost + ":" + proxyPort);
            Proxy proxy = new Proxy(Proxy.Type.HTTP,
                new InetSocketAddress(proxyHost, proxyPort));
            crsClient = (HttpURLConnection) (url.openConnection(proxy));
        } else {
            crsClient = (HttpURLConnection) (url.openConnection(Proxy.NO_PROXY));
        }

        String boundary = "---------------------------84028462148935";

        crsClient.setDoInput(true);
        crsClient.setDoOutput(true);
        crsClient.setUseCaches(false);
        crsClient.setAllowUserInteraction(false);
        crsClient.setRequestMethod("POST");
        crsClient.setRequestProperty("Content-Type",
            "multipart/form-data; boundary=" + boundary);

        // generate a random uuid
        String uuid = UUID.randomUUID().toString();

        // Compose the parts which make up the 'registration'
        // message that will be POSTed to the SCRK registration
        // service:
        // 1. Sun Online Account
        // 2. Sun Online Account password
        // 3. arbitrary identifier for the object being
        //    registered, the identifier should be something
        //    that uniquely identifies the object.  UUIDs
        //    are sufficient if the object has no fixed unique
        //    identifier.
        // 4. Public key associated with the object being
        //    registered.
        //
        // For the purposes of this example use the test
        // Sun Online Account (guest80864/guest) a random
        // indentifier for this client and the Public key
        // what was generated for this client.
        String payload = getContent(boundary, NAME_SOA_ID, soaUser)
            + getContent(boundary, NAME_SOA_PW, soaPass)
            + getContent(boundary, NAME_ASSET_ID, uuid)
            + getContent(boundary, "DESCRIPTION", "SvcTag Registration")
            + getContent(boundary, NAME_PUB_KEY,
                getPublicKeyAsX509String(keyPair.getPublic()))
            + "--" + boundary + "--\r\n";

        //System.out.println("payload: \n" + payload);
        crsClient.connect();

        OutputStream urlOut = crsClient.getOutputStream();
        urlOut.write(payload.getBytes());
        urlOut.flush();
        urlOut.close();

        // POST the request
        //System.out.println("\nRegistering...");
        String body = null;
        int statusCode = crsClient.getResponseCode();
        log.log(Level.FINER, "crs response code: " + statusCode);

        if (statusCode == 200) {
            // the POST made it to the servlet and was
            // processed, extract the response body
            body = getResponseBodyAsString(crsClient);
        } else {
            //System.out.println("bad status: " + statusCode);
            //body = getErrorResponseBodyAsString(crsClient);
            //System.out.println("body: \n" + body);
            throw new Exception(
                "Sun Inventory Authentication Error: Fatal server error encountered, status code: "
                + statusCode);
        }
        //System.out.println("body: \n" + body);

        crsClient.disconnect();

        // retrieve the SCRK identifier assigned to the registered
        // object from the response body.
        //
        // This identifier should be used in subsequent SCRK webservice
        // calls to identify 'who' is making the webservice calls.
        // Those webservices expect to be able to find the publickey
        // associated with the identifier to verify message signatures.
        String clientRegId = null;
        String[] crsResponseBody = body.split("\n");
        log.log(Level.FINEST, "CRS Body: " + body);
        //System.out.println("CRS Body: " + body);
        String crsCodeStr = null;

        for (int i = 0; i < crsResponseBody.length; i++) {
            if (crsResponseBody[i].startsWith(NAME_CLIENT_REG_ID + "=")) {
                String[] clientReg = crsResponseBody[i].split("=", 2);
                clientRegId = clientReg[1];
                break;
            } else if (crsResponseBody[i].startsWith("CODE=")) {
                String[] crsCodes = crsResponseBody[i].split("=", 2);
                crsCodeStr = crsCodes[1];
            }
        }

        log.log(Level.FINE, "client reg id: " + clientRegId);
        if (clientRegId == null) {
            // If no identifier was returned, there was an error of some sort.
            //System.err.println("Failure!  No CLIENT_REG_ID returned");
            if (crsCodeStr != null) {
                int crsCode = -1;
                try {
                    crsCode = Integer.parseInt(crsCodeStr);
                } catch (Exception e) {
                }
                switch (crsCode) {
                    case 1:
                        throw new Exception(
                            "Sun Inventory Authentication Error: Missing requred element");
                    case 2:
                        throw new Exception(
                            "Sun Inventory Authentication Error: Argument too large");
                    case 3:
                        throw new Exception(
                            "Sun Inventory Authentication Error: Invalid XML");
                    case 4:
                        throw new Exception(
                            "Sun Inventory Authentication Error: Invalid Sun Online Account user/password");
                    case 5:
                        throw new Exception(
                            "Sun Inventory Authentication Error: Invalid autoregistration token");
                    case 6:
                        throw new Exception(
                            "Sun Inventory Authentication Error: Fatal server error");
                }
            }
            throw new Exception(
                "Sun Inventory Authentication Error: Fatal server error encountered");
        }

        return clientRegId;
    }

    /**
     * When a publicKey is submitted to the SCRK ClientRegistration
     * service, it is expected to be in X509/PEM format:
     *
     *   -----BEGIN PUBLIC KEY-----\n
     *   MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDur6pijLbYpIuCU7oeE0hhC1R8\n
     *   j1Kc4cIaqCRlanQ5+XWBrje0eKZeSe/xnFoGs17zFuqfqwvrrGeG7HHMU3IEQFDw\n
     *   4DbPekdtBCs5ED/8ofW8hK9EzC4EBni4UkrXcVkmfk82SQUJ66xyPt5n4qa/tmX0\n
     *   OEGQ2fUJRs5ptff8zwIDAQAB\n
     *   -----END PUBLIC KEY-----
     *
     * This method converts the Java PublicKey into that format
     * and returns it as a String.
     */
    private String getPublicKeyAsX509String(PublicKey publicKey) {
        final String PEM_PUBKEY_HEADER = "-----BEGIN PUBLIC KEY-----\n";
        final String PEM_PUBKEY_FOOTER = "\n-----END PUBLIC KEY-----";

        StringBuilder sb = new StringBuilder(PEM_PUBKEY_HEADER);
        // the bytes representing the key need to be Base64
        // encoded for X509/PEM:
        sb.append(new String(Base64.encodeBase64(publicKey.getEncoded())));
        sb.append(PEM_PUBKEY_FOOTER);

        return sb.toString();
    }

    private String getContent(String boundary, String key, String value) {
        return "--" + boundary + "\r\n"
            + "Content-Disposition: form-data; name=\"" + key + "\"\r\n\r\n"
            + value + "\r\n";
    }

    private String getErrorResponseBodyAsString(HttpURLConnection crsClient) {
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                            crsClient.getErrorStream()));
            StringBuilder sb = new StringBuilder();

            while (true) {
                String line = reader.readLine();

                if (line == null) {
                    break;
                }

                sb.append(line).append("\n");
                //System.out.println(line);
            }

            reader.close();
            return sb.toString();
        } catch (Exception e) {
            //e.printStackTrace();
            return null;
        }
    }

    private String getResponseBodyAsString(HttpURLConnection crsClient) {
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                            crsClient.getInputStream()));
            StringBuilder sb = new StringBuilder();

            while (true) {
                String line = reader.readLine();

                if (line == null) {
                    break;
                }

                sb.append(line).append("\n");
                //System.out.println(line);
            }

            reader.close();
            return sb.toString();
        } catch (Exception e) {
            //e.printStackTrace();
            return null;
        }
    }
}
