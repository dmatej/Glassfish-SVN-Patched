/*
 * @(#)file      HtmlRequestHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.29
 * @(#)lastedit      07/03/08
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
import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.EOFException;
import java.net.Socket;
import java.net.SocketException;
import java.util.Date;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.ReflectionException;

// jmx RI import
//
import com.sun.jdmk.internal.ClassLogger;



class HtmlRequestHandler extends ClientHandler {

    // --------------------------------------------------------
    // CONSTRUCTORS
    // --------------------------------------------------------

    /**
     * Construct a new HtmlRequestHandler.
     */
    public HtmlRequestHandler(Socket s, HtmlAdaptorServer server, MBeanServer f, ObjectName n, int id) {
        super(server, id, f, n);
        sockClient = s;    
        // Now we can start the thread.
        // Note: it is done here (ie at the end of the constructor) because
        // the object initialization is now complete.
        //
        thread.start() ;
    }


    // --------------------------------------------------------
    // PUBLIC VARIABLES
    // --------------------------------------------------------
    
    /**
     * Treat the incoming request and send the results back to the client.
     */
    public void doRun() {
        if (logger.finerOn()) {
            logger.finer("doRun","Start Html request handler");
        }

        try {
            HttpRequest request = new HttpRequest(new HttpBody()) ;
            boolean loopAgain = true ;
            while (loopAgain) {
                HttpResponse response = null ;
                try {
                    request.readFrom(sockClient.getInputStream()) ;
                    response = processRequest(request) ;
                }
                catch(MalformedHttpException x) {
                    if (logger.finestOn()) {
                        logger.finest("doRun","Malformed HTTP request rejected. [Exception="+x+"]") ;
                    }
                    response = makeErrorResponse(HttpResponse.STATUS_BAD_REQUEST) ;
                }
                response.writeTo(sockClient.getOutputStream()) ;
                loopAgain = (request.hasKeepAliveFlag() && 
                             (response.statusCode == response.STATUS_OK) &&
                             !interruptCalled) ;
                // REMIND: do we really need to end the loop in case of error ?
            }
        } catch(InterruptedIOException x) {
            if (logger.finestOn()) {
                logger.finest("doRun", "Request handler interrupted") ;
            }
        } catch(EOFException x) {
            if (logger.finestOn()) {
                logger.finest("doRun", "Connection closed by peer") ;
            }
        } catch(SocketException x) {
            if (x.getMessage().equals(InterruptSysCallMsg)) {
                if (logger.finestOn()) {
                    logger.finest("doRun", "Request handler interrupted") ;
                }
            } else {
                if (logger.finestOn()) {
                    logger.finest("doRun", "I/O exception. [Exception="+x+"]") ;
                }
            }
        } catch(IOException x) {
            if (logger.finestOn()) {
                logger.finest("doRun", "I/O exception. [Exception="+x+"]") ;
            }
        } finally {
            try { 
                sockClient.close() ; 
                if (logger.finestOn()) {
                    logger.finest("doRun", "Socket is now closed") ;
                }
            } 
            catch(IOException e) {
                if (logger.finestOn()) {
                    logger.finest("doRun", "Socket closed with [Exception="+e+"]") ;
                }
            }
        }
    }


    // --------------------------------------------------------
    // PROTECTED VARIABLES
    // --------------------------------------------------------
    
    /**
     * Process a request and return the response to be sent to the client.
     */
    protected HttpResponse processRequest(HttpRequest request) throws IOException {
        if (logger.finerOn()) {
            logger.finer("processRequest","Process the Html request");
        }

        HttpResponse response = null ;

        if (authenticateRequest(request) == false) {
            if (logger.finerOn()) {
                logger.finer("processRequest","Authentication failed");
            }
            response = makeErrorResponse(HttpResponse.STATUS_UNAUTHORIZED) ;
        }
        else if (request.method == request.METHOD_GET) {
            response = processGetRequest(request) ;
        } else {
            if (logger.finerOn()) {
                logger.finer("processRequest","Bad request: request not supported");
            }
            response = makeErrorResponse(HttpResponse.STATUS_BAD_REQUEST) ;
        }
	response.setHeader(response.CONNECTION_HEADER,
			   request.getHeader(request.CONNECTION_HEADER));
        return response ;
    }
    
    /**
     * Authenticate the request and returns false if it fails.
     * If the authentication is disable, this method always return true .
     */
    protected boolean authenticateRequest(HttpRequest request) throws IOException {
        if (logger.finerOn()) {
            logger.finer("authenticateRequest","Authenticate the HTML request, using the 'Basic Authentication Scheme'");
        }
        
        com.sun.jdmk.comm.HtmlAdaptorServer server =
            (com.sun.jdmk.comm.HtmlAdaptorServer)adaptorServer;

	if (!server.isAuthenticationOn())
	    return true;
    
	final String authorization =
	    request.getHeader(request.AUTHORIZATION_HEADER);

	if ((authorization == null) || !authorization.startsWith("Basic "))
	    return false;

	String encoding = authorization.substring(6);
	byte decoding[] = new BASE64Decoder().decodeBuffer(encoding);
	String response = new String(decoding);
	return server.checkChallengeResponse(response);
    }

    /**
     * Process an incoming get request and returns the response.
     */
    protected HttpResponse processGetRequest(HttpRequest request) throws IOException {
        String URIPath = request.getURIPath() ;
        int resultCode = HttpResponse.STATUS_OK ;
        String resultBody = null ;
        HtmlAdaptorServer server = (HtmlAdaptorServer)adaptorServer ;
        HttpResponse response = null;

        if (logger.finerOn()) {
            logger.finer("processGetRequest","Process a GET request = "+URIPath);
        }

        while (true){ // Should make this loop only once.           
            // Use the user provided parser if specified.
            //
            if (server.getParser() != null) {
                String[] param = new String[1];
                param[0] = URIPath;
                String[] sign = new String[1];
                sign[0] = "java.lang.String";
                try {
                    resultBody = (String)mbs.invoke(server.getParser(), "parseRequest", param, sign);
                } catch(InstanceNotFoundException e) {
                    if (logger.finestOn()) {
                        logger.finest("processGetRequest", "Invalid user's parser = "+e) ;
                    }
		    String errMsg = HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND +
			            "<P>Invalid user's parser: "+ server.getParser().toString() +" is unknown";
                    server.resetParser();
                    return makeErrorResponse(HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID, errMsg);
                    
                } catch(MBeanException e) {
                    if (logger.finestOn()) {
                        logger.finest("processGetRequest", "Parser exception = "+e.getTargetException()) ;
                    }
		    String errMsg = HtmlDef.HTTP_ERROR_MBEAN + 
			            "<P>"+server.getParser().toString()+" throws <BR>"+e.getTargetException();
                    server.resetParser();
                    return makeErrorResponse(HtmlDef.HTTP_ERROR_MBEAN_ID, errMsg);
                } catch(ReflectionException e) {
                    if (logger.finestOn()) {
                        logger.finest("processGetRequest", "MBeanServer reflection exception = "+e.getTargetException()) ;
                    }
		    String errMsg = HtmlDef.HTTP_ERROR_REFLECTION + 
                                    "<P>MBeanServer throws:<BR>"+e.getTargetException()+
                                    "<BR> when reflecting: "+server.getParser().toString();
                    server.resetParser();
                    return makeErrorResponse(HtmlDef.HTTP_ERROR_REFLECTION_ID, errMsg);
                }   
                if (resultBody != null) {
                    break;
                }
            }

            if (URIPath.startsWith("/Request/")) {
                String arg = URIPath.substring(9) ;
                if (arg.equals("getDomain")) {
                    resultBody = mbs.getDefaultDomain() + ":" ;
                } else {
                    resultCode = HttpResponse.STATUS_BAD_REQUEST ;
                }
            }
            // Handle the request to view a specific m-bean.
            else if (URIPath.startsWith(HtmlDef.VIEWOBJECTRES)) {
                String objNameStr = URIPath.substring(HtmlDef.VIEWOBJECTRES.length());
                HtmlObjectPage page = new HtmlObjectPage(mbs, true, true, server);
                page.buildPage(objNameStr);
                resultBody = page.getPage() ;
            }
            // Handle the auto refresh request.
            else if (URIPath.startsWith(HtmlDef.AUTOREFRESH)) {
                String objNameStr = URIPath.substring(HtmlDef.AUTOREFRESH.length());
                HtmlObjectPage page = new HtmlObjectPage(mbs, true, true, server);
                page.buildMeta(objNameStr);
                page.buildPage(objNameStr.substring(0,objNameStr.indexOf("?period=")));
                resultBody = page.getPage() ;
            }
            // Handle the request to view a property (for array).
            else if (URIPath.startsWith(HtmlDef.VIEWPROPERTY)) {
                String objNameStr = URIPath.substring(HtmlDef.VIEWPROPERTY.length());
                HtmlArrayPage page = new HtmlArrayPage(mbs, true, true, server);
                page.buildPage(objNameStr);
                resultBody = page.getPage() ;
            }
            // Set (when apply)
            else if (URIPath.startsWith(HtmlDef.SETFORM)) {
                int   query;
                int   index;
                String setReqStr;
                String objNameStr = URIPath.substring(HtmlDef.SETFORM.length());
        
                index = objNameStr.indexOf('/',1);
                query = objNameStr.indexOf('?');
                if (index < 0 || query < 0 ) {
                    resultCode = HttpResponse.STATUS_BAD_REQUEST ;
                } else {
                    setReqStr  = objNameStr.substring(query);
                    objNameStr = objNameStr.substring(0, query);
        
                    HtmlObjectPage page = new HtmlObjectPage(mbs, true, true, server);
        
                    if (logger.finerOn()) {
                        logger.finer("processGetRequest", "SetForm for [objName=" + objNameStr + " ,request=" + setReqStr + "]");
                    }
        
                    if (!page.setObjectValue(objNameStr, setReqStr)) {
                        resultBody = page.getPage() ;
                    }
                    else {
                        // Rebuild the full page for the new Object
                        //          
                        page.buildPage(objNameStr) ;
                        resultBody = page.getPage() ;
                    }
                }
            }
            // Build the admin page.
            else if (URIPath.startsWith(HtmlDef.ADMIN)) {
                String objNameStr;
                objNameStr = URIPath.substring(HtmlDef.ADMIN.length());
                objNameStr = objNameStr.trim();
        
                HtmlAdminPage page = new HtmlAdminPage(mbs, true, true);
                page.buildPage(objNameStr);
                resultBody = page.getPage() ;
            }
            // Build perform action page.
            else if (URIPath.startsWith(HtmlDef.INVOKEACTION)) {
                String objNameStr;
                objNameStr = URIPath.substring(HtmlDef.INVOKEACTION.length());
                objNameStr = objNameStr.trim();
        
                HtmlInvokePage page = new HtmlInvokePage(mbs, true, true);
                page.buildPage(objNameStr);
                resultBody = page.getPage() ;
            }
            // Build the browser main page.
            else {
                if (URIPath.equals(HtmlDef.MAIN) ||URIPath.startsWith(HtmlDef.FILTER)) {
                    HtmlMasterPage page = new HtmlMasterPage(mbs, true, true);
                    page.buildPage(URIPath);
                    resultBody = page.getPage() ;
                } else {
                    resultCode = HttpResponse.STATUS_BAD_REQUEST ;
                }
            }

            // Call the user parser.
            //
            if (server.getParser() != null){
                String[] param = new String[1];
                param[0] = resultBody;;
                String[] sign = new String[1];
                sign[0] = "java.lang.String";
                try{
                    resultBody = (String)mbs.invoke(server.getParser(),"parsePage",param,sign);
                } catch(InstanceNotFoundException e) {
                    if (logger.finestOn()) {
                        logger.finest("processGetRequest", "Invalid user's parser ["+e+"]") ;
                    }
		    String errMsg = HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND +
			            "<P>Invalid user's parser: "+ server.getParser().toString() +" is unknown";
                    server.resetParser();
                    return makeErrorResponse(HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID, errMsg);
                    
                } catch(MBeanException e) {
                    if (logger.finestOn()) {
                        logger.finest("processGetRequest", "Parser exception = "+e.getTargetException()) ;
                    }
		    String errMsg = HtmlDef.HTTP_ERROR_MBEAN + 
			            "<P>"+server.getParser().toString()+" throws <BR>"+e.getTargetException();
                    server.resetParser();
                    return makeErrorResponse(HtmlDef.HTTP_ERROR_MBEAN_ID, errMsg);
                } catch(ReflectionException e) {
                    if (logger.finestOn()) {
                        logger.finest("processGetRequest", "MBeanServer reflection exception = "+e.getTargetException()) ;
                    }
		    String errMsg = HtmlDef.HTTP_ERROR_REFLECTION + 
                                    "<P>MBeanServer throws:<BR>"+e.getTargetException()+
                                    "<BR> when reflecting: "+server.getParser().toString();
                    server.resetParser();
                    return makeErrorResponse(HtmlDef.HTTP_ERROR_REFLECTION_ID, errMsg);
                }   
            }    
            break;
        }

        // Now build the response.
        //
        if (resultBody != null && resultCode == HttpResponse.STATUS_OK){
            return makeOkResponse(resultBody) ;
        } else {
            if (logger.finestOn()) {
                logger.finest("processGetRequest", "Bad request: request not supported or HTML page empty") ;
            }
            return makeErrorResponse(HttpResponse.STATUS_BAD_REQUEST) ;   
        }
    }
    
    /**
     * Make an error response with specified code
     */
    protected HttpResponse makeErrorResponse(int statusCode) {
        return makeErrorResponse(statusCode, null) ;
    }
       
    /**
     * Make an error response with specified code and msg
     */ 
    protected HttpResponse makeErrorResponse(int statusCode, String msg) {
        //
        // Build the Entity-Body
        //
        String body =
	    "<HTML>" + HtmlDef.CRLF +
	    "<BODY>" + HtmlDef.CRLF +
	    "<HR><P><FONT SIZE=+3 COLOR=red><B>" + statusCode + "</B></FONT><P>" + HtmlDef.CRLF +
	    "<HR><P>" + msg + "</P>" + HtmlDef.CRLF +
	    "</BODY>" + HtmlDef.CRLF +
	    "</HTML>" + HtmlDef.CRLF;
        //
        // Build the response
        //
        HttpResponse response = new HttpResponse(new HttpBody(body.getBytes())) ;
        response.statusCode = statusCode ;
        response.setHeader(response.CONTENT_TYPE_HEADER, "text/html");
        response.setHeader(response.DATE_HEADER, new Date().toString());
	final String wwwauth =
	    (statusCode == response.STATUS_UNAUTHORIZED) ?
	    "Basic realm=\"JDMK\"" : null;
	response.setHeader(response.WWW_AUTHENTICATE_HEADER, wwwauth);
    
        return response ;
    }
    
    /**
     * Make a response with specified body
     */ 
    protected HttpResponse makeOkResponse(String bodyStr) {
        HttpBody body = new HttpBody(bodyStr.getBytes());
        HttpResponse response = new HttpResponse(body);
        response.statusCode = response.STATUS_OK;
        response.setHeader(response.CONTENT_TYPE_HEADER, "text/html");
        response.setHeader(response.DATE_HEADER, new Date().toString());

        return response;
    }

    /**
     * Returns the string used in debug traces.
     */
    protected String makeDebugTag() {
        return "HtmlRequestHandler[" + adaptorServer.getProtocol() + ":" + 
	    adaptorServer.getPort() + "][" + requestId + "]";
    }

    // --------------------------------------------------------
    // PRIVATE VARIABLES
    // --------------------------------------------------------

    private Socket sockClient = null;
    private String bgPageColor = null;
    private static final String InterruptSysCallMsg = "Interrupted system call";    
}

