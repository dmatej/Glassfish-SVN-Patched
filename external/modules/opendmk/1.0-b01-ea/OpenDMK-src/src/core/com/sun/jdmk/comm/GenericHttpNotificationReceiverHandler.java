/*
 * @(#)file      GenericHttpNotificationReceiverHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.10
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

class GenericHttpNotificationReceiverHandler 
    extends GenericHttpClientHandler {

    // CONSTRUCTOR
    //------------

    public GenericHttpNotificationReceiverHandler(GenericHttpNotificationReceiver server, int id, GenericHttpSocket s) {
        super(server, id, s, null, null);

    }

    // PROTECTED METHODS
    // -----------------

    protected AuthInfo authenticateRequest(HttpRequest request) {
	return makeNullAuthInfo();
    }

    protected String getChallenge() {
	return null;
    }

    /**
     * Process an incoming post request and return the response.
     */
    protected HttpResponse processPostRequest(HttpRequest request) throws IOException {

        String   remoteOp = null;
        Integer  opType   = null;
        Object[] params   = null;

        if (logger.finerOn()) {
            logger.finer("processPostRequest","Process a POST request = " + request.getURIPath());
        }

        //
        // Check that content is not null
        //
        if (request.getContentLength() == 0) {
            //
            // Send a Bad Request
            //
            return makeErrorResponse(HttpDef.HTTP_ERROR_BAD_REQUEST_ID, HttpDef.HTTP_ERROR_BAD_REQUEST);
        }

        //
        // Get remote operation to be performed
        //
        Object result = null;
        String resultType = null;
        ByteArrayInputStream bIn = null;
        ObjectInputStream objIn = null;
        ByteArrayOutputStream bOut = null;

        try {
            bIn = new ByteArrayInputStream(request.getContentBytes());
            objIn = new ObjectInputStream(bIn);
            remoteOp = (String) objIn.readObject();

            if (logger.finerOn())
                logger.finer("doRun", "Remote operation: " + remoteOp);

            if (remoteOp.equals("remoteRequest")) {
                //
                // Process the request:
                //
                // Object[] remoteRequest(int,Object[]);
                //
                opType = (Integer) objIn.readObject();
                params = (Object[]) objIn.readObject();

                result = remoteRequest(opType.intValue(), params);
                resultType = "Object[]";
            } else {
                //
                // Send a Bad Request
                //
                if (logger.finerOn())
                    logger.finer("doRun", "Unknown remote operation: " + remoteOp);
                return makeErrorResponse(HttpDef.HTTP_ERROR_BAD_REQUEST_ID,
					 HttpDef.HTTP_ERROR_BAD_REQUEST);
            }
        } catch (Exception e) {
            if (logger.finerOn())
                logger.finer("doRun", "Failed to do remote request: "+e);
            return makeExceptionResponse(e);
        }

        //
        // Serialize the result and send OK
        //
        bOut = serialize(resultType, result);
        return makeOkResponse(bOut.toByteArray());
    }

    // HTTP SPECIFIC METHODS
    //----------------------

    // MAPPING OF CLIENT REQUESTS TO MBEANSERVER METHODS
    //--------------------------------------------------

    /**
     * Object[]
     * remoteRequest(int opType, Object[] params)
     * Transfers a notification request from the client to the agent.
     */
    private Object[] remoteRequest(int opType, Object[] params) throws Exception {
        if (logger.finerOn())
            logger.finer("remoteRequest", "remoteRequest");
        GenericHttpNotificationReceiver server = (GenericHttpNotificationReceiver) adaptorServer;
        return server.dispatcher.remoteRequest(opType,params);
    }

    // TRACE METHODS
    //--------------

    /**
     * Returns the string used in debug traces.
     */
    protected String makeDebugTag() {
        return "GenericHttpNotificationReceiverHandler[" + adaptorServer.getProtocol() + ":" + adaptorServer.getPort() + "][" + requestId + "]";
    }

}
