/*
 * @(#)file      GenericHttpClientHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.12
 * @(#)date      07/04/04
 *
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright (c) 2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU General
 * Public License Version 2 only ("GPL") or the Common Development and
 * Distribution License("CDDL")(collectively, the "License"). You may not use
 * this file except in compliance with the License. You can obtain a copy of the
 * License at http://opendmk.dev.java.net/legal_notices/licenses.txt or in the 
 * LEGAL_NOTICES folder that accompanied this code. See the License for the 
 * specific language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file found at
 *     http://opendmk.dev.java.net/legal_notices/licenses.txt
 * or in the LEGAL_NOTICES folder that accompanied this code.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.
 * 
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * 
 *       "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding
 * 
 *       "[Contributor] elects to include this software in this distribution
 *        under the [CDDL or GPL Version 2] license."
 * 
 * If you don't indicate a single choice of license, a recipient has the option
 * to distribute your version of this file under either the CDDL or the GPL
 * Version 2, or to extend the choice of license to its licensees as provided
 * above. However, if you add GPL Version 2 code and therefore, elected the
 * GPL Version 2 license, then the option applies only if the new code is made
 * subject to such option by the copyright holder.
 * 
 *
 */


package com.sun.jdmk.comm;



// java import
//
import java.io.*;
import java.net.*;
import java.util.Set;
import java.util.Date;

// jmx import
//
import javax.management.ObjectName;
import javax.management.MBeanServer;

// jdmk import
import com.sun.jdmk.ThreadContext;
import com.sun.jdmk.internal.ClassLogger;


abstract class GenericHttpClientHandler extends ClientHandler {

    // CONSTRUCTOR
    //------------

    public GenericHttpClientHandler(CommunicatorServer server, int id,
                                    GenericHttpSocket s, MBeanServer mbs,
                                    ObjectName name) {
        super(server, id, mbs, name);

        sockClient = s;

        // Now we can start the thread.
        // Note: it is done here (i.e. at the end of the constructor)
        // because the object initialization is now complete.
        //
        thread.start();
    }

    // ABSTRACT METHODS
    //

    protected abstract AuthInfo authenticateRequest(HttpRequest request)
        throws IOException;
    protected abstract HttpResponse processPostRequest(HttpRequest request)
        throws IOException;
  
    /**
     * Treat the incoming requests and send the results back to the client.
     * This method ends after the last request is processed or after
     * an interruption.
     */
    public void doRun() {
        try {
            HttpRequest request = new HttpRequest(new HttpBody());
            boolean loopAgain = true;
            while (loopAgain) {
                HttpResponse response = null;
                try {
                    request.readFrom(sockClient.doGetInputStream());
                    response = processRequest(request);
                } catch (MalformedHttpException x) {
                    if (logger.finestOn()) {
                        logger.finest("doRun",
                                      "Malformed HTTP request rejected");
                    }
                    response =
                        makeErrorResponse(HttpDef.HTTP_ERROR_BAD_REQUEST_ID,
                                          HttpDef.HTTP_ERROR_BAD_REQUEST);
                }
                final boolean noKeepAlive =
                    Boolean.getBoolean("com.sun.jdmk.http.server.noKeepAlive");
                if (noKeepAlive)
                    response.setHeader(response.CONNECTION_HEADER, "close");
                else
                    response.setHeader(response.CONNECTION_HEADER,
                                request.getHeader(request.CONNECTION_HEADER));
                response.writeTo(sockClient.doGetOutputStream());
                if (noKeepAlive)
                    loopAgain = false;
                else
                    loopAgain =
                        (response.hasKeepAliveFlag() && !interruptCalled);
            }
        } catch (InterruptedIOException x) {
            if (logger.finestOn()) {
                logger.finest("doRun", "Request handler interrupted");
            }
        } catch (EOFException x) {
            if (logger.finestOn()) {
                logger.finest("doRun", "Connection closed by peer");
            }
        } catch (SocketException x) {
            if (x.getMessage().equals(InterruptSysCallMsg)) {
                if (logger.finestOn()) {
                    logger.finest("doRun", "Request handler interrupted");
                }
            } else {
                if (logger.finestOn()) {
                    logger.finest("doRun", "I/O exception " + x);
                }
            }
        } catch (IOException x) {
            if (logger.finestOn()) {
                logger.finest("doRun", "I/O exception " + x);
            }
        } finally {
            closeClient();
            if (logger.finestOn()) {
                logger.finest("doRun", "Socket is now closed");
            }
        }
    }

    /**
     * Process a request and return the response to be sent to the client.
     */
    protected HttpResponse processRequest(HttpRequest request)
        throws IOException {

        if (logger.finerOn()) {
            logger.finer("processRequest","Process the HTTP request");
        }

        HttpResponse response = null;
        AuthInfo authInfo = authenticateRequest(request);
        if (authInfo == null) {
            response =
                makeErrorResponse(HttpDef.HTTP_ERROR_UNAUTHORIZED_REQUEST_ID,
                                  HttpDef.HTTP_ERROR_UNAUTHORIZED_REQUEST);
        } else if (request.method == request.METHOD_POST) {
            /* This is where we could make the AuthInfo of the
               received request available in the call-chain that
               handles the request.  Having it would enable an MBean
               at the end of the call-chain to forward the request to
               another remote MBeanServer with the same credentials.
               But we don't necessarily want to expose the AuthInfo
               (in particular the password), so this is if'd out for
               now.  */
            if (true)
                response = processPostRequest(request);
            else {
                ThreadContext oldThreadContext =
                    ThreadContext.push("AuthInfo", authInfo);
                try {
                    response = processPostRequest(request);
                } finally {
                    ThreadContext.restore(oldThreadContext);
                }
            }
        } else {
            if (logger.finestOn()) {
                logger.finest("processRequest",
                      "Bad request: Request method not supported");
            }
            response = makeErrorResponse(HttpDef.HTTP_ERROR_BAD_REQUEST_ID,
                                         HttpDef.HTTP_ERROR_BAD_REQUEST);
        }
        return response;
    }

    /**
     * Make an status OK response with specified data.
     */
    protected HttpResponse makeOkResponse(byte[] data) {
        return makeResponse(HttpDef.HTTP_REPLY_OK_ID, HttpDef.HTTP_REPLY_OK,
                            data);
    }

    /**
     * Make an status Bad Request response with specified exception.
     */
    protected HttpResponse makeExceptionResponse(Exception exception) {
        ByteArrayOutputStream bOut = null;
        try {
            bOut = serialize("Exception", exception);
        } catch (IOException e) {
            if (logger.finestOn()) {
                logger.finest("makeExceptionResponse", 
                              "Got IOException when serializing : " + e);
            }
            return makeErrorResponse(HttpDef.HTTP_ERROR_BAD_REQUEST_ID,
                                     HttpDef.HTTP_ERROR_BAD_REQUEST);
        }
        if (logger.finestOn()) {
            logger.finest("makeExceptionResponse", 
                          "Sending back Exception : " + exception);
        }
        return makeResponse(HttpDef.HTTP_ERROR_BAD_REQUEST_ID,
                            HttpDef.HTTP_ERROR_BAD_REQUEST,
                            bOut.toByteArray());
    }

    /**
     * Make an error response with specified status code.
     */
    protected HttpResponse makeErrorResponse(int statusCode,
                                             String reasonPhrase) {
        return makeResponse(statusCode, reasonPhrase, null);
    }

    protected HttpResponse makeResponse(int statusCode, String reasonPhrase,
                                        byte[] contents) {
        HttpBody body;
        if (contents == null)
            body = new HttpBody();
        else
            body = new HttpBody(contents);
        HttpResponse response = new HttpResponse(body);
        response.statusCode = statusCode;
        response.reasonPhrase = reasonPhrase;
        response.setHeader(response.CONTENT_TYPE_HEADER,
                           "application/octet-stream");
        response.setHeader(response.DATE_HEADER, new Date().toString());
        response.setHeader(response.WWW_AUTHENTICATE_HEADER, getChallenge());
        return response;
    }

    private static final AuthInfo nullAuthInfo = new AuthInfo() {
        public void setLogin(String login) {
            /* We would like to use UnsupportedOperationException here,
               but that's not present in JDK 1.1.  When we drop 1.1 support
               we can change this!  

               throw new UnsupportedOperationException();
            */
            throw new IllegalArgumentException("unsupported operation");
        }

        public void setPassword(String password) {
            /* throw new UnsupportedOperationException(); */
            throw new IllegalArgumentException("unsupported operation");
        }
    };

    protected AuthInfo makeNullAuthInfo() {
        return nullAuthInfo;
    }

    // HTTP SPECIFIC METHODS
    //----------------------

    /**
     * End the request handling.
     */
    void closeClient() {
        if (logger.finerOn())
            logger.finer("closeClient", "Close client ...");
        if (sockClient != null) {
            try {
                sockClient.doDisconnect();
            } catch (IOException e) {
                // Ignore...
            } catch (CommunicationException e) {
                // Ignore...
            } finally {
                sockClient = null;
            }
        }
    }

    /**
     * Serialize the result before sending it back to the client
     */
    protected ByteArrayOutputStream serialize(String resultType,
                                              Object result)
            throws IOException {
        ObjectOutputStream objOut;
        ByteArrayOutputStream bOut;

        bOut = new ByteArrayOutputStream();
        objOut = new ObjectOutputStream(bOut);

        objOut.writeObject(resultType);
        objOut.writeObject(result);

        return bOut;
    }

    protected abstract String getChallenge();

    // PRIVATE VARIABLES
    //------------------

    private GenericHttpSocket sockClient = null;
    private static final String InterruptSysCallMsg =
        "Interrupted system call";    
}
