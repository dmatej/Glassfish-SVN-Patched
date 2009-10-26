/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.server;

import javax.xml.rpc.ServiceException;
import javax.servlet.ServletContext;
import javax.xml.rpc.handler.MessageContext;
import javax.servlet.http.HttpSession;


/** The <code>ServletEndpointContext</code> provides an endpoint 
 *  context maintained by the underlying servlet container based
 *  JAX-RPC runtime system. For service endpoints deployed on a 
 *  servlet container based JAX-RPC runtime system, the context 
 *  parameter in the <code>ServiceLifecycle.init</code> method is
 *  required to be of the Java type 
 *  <code>javax.xml.rpc.server.ServletEndpointContext</code>.
 *
 *  <p>A servlet container based JAX-RPC runtime system implements
 *  the <code>ServletEndpointContext</code> interface. The JAX-RPC
 *  runtime system is required to provide appropriate session, 
 *  message context, servlet context and user principal information 
 *  per method invocation on the endpoint class.
 *
 *  @version 1.1
 *  @author  Rahul Sharma
 *  @author  Roberto Chinnici
**/

public interface ServletEndpointContext {

  /** The method <code>getMessageContext</code> returns the 
   *  <code>MessageContext</code> targeted for this endpoint instance.
   *  This enables the service endpoint instance to acccess the 
   *  <code>MessageContext</code> propagated by request
   *  <code>HandlerChain</code> (and its contained <code>Handler</code> 
   *  instances) to the target endpoint instance and to share any 
   *  SOAP message processing related context. The endpoint instance
   *  can access and manipulate the <code>MessageContext</code> 
   *  and share the SOAP message processing related context with
   *  the response <code>HandlerChain</code>. 
   *  
   *  @return MessageContext; If there is no associated 
   *          <code>MessageContext</code>, this method returns
   *          <code>null</code>.
   *  @throws  java.lang.IllegalStateException if this method is 
   *           invoked outside a remote method implementation by 
   *           a service endpoint instance.
   *  @see javax.xml.rpc.handler.MessageContext
   *  @see javax.xml.rpc.handler.HandlerChain
   *  @see javax.xml.rpc.handler.Handler
  **/
  public javax.xml.rpc.handler.MessageContext getMessageContext();

  /** Returns a <code>java.security.Principal</code> instance that 
   *  contains the name of the authenticated user for the current
   *  method invocation on the endpoint instance. This method returns
   *  <code>null</code> if there is no associated principal yet. 
   *  The underlying JAX-RPC runtime system takes the responsibility
   *  of providing the appropriate authenticated principal for a 
   *  remote method invocation on the service endpoint instance.
   *
   *  @return A <code>java.security.Principal</code> for the 
   *           authenticated principal associated with the current
   *           invocation on the servlet endpoint instance;
   *           Returns <code>null</code> if there no authenticated
   *           user associated with a method invocation.
   *  @see java.security.Principal
  **/
  public java.security.Principal getUserPrincipal();


  /** The <code>getHttpSession</code> method returns the current 
   *  HTTP session (as a <code>javax.servlet.http.HTTPSession</code>). 
   *  When invoked by the service endpoint within a remote method 
   *  implementation, the <code>getHttpSession</code> returns the 
   *  HTTP session associated currently with this method invocation. 
   *  This method returns <code>null</code> if there is no HTTP 
   *  session currently active and associated with this service 
   *  endpoint. An endpoint class should not rely on an active 
   *  HTTP session being always there; the underlying JAX-RPC
   *  runtime system is responsible for managing whether or not 
   *  there is an active HTTP session.

   *  <p>The getHttpSession method throws <code>JAXRPCException</code> 
   *  if invoked by an non HTTP bound endpoint.
   *
   *  @return The HTTP session associated with the current
   *           invocation or <code>null</code> if there is
   *           no active session.
   *
   *  @throws  JAXRPCException If this method invoked by any 
   *                           non-HTTP bound endpoint
   *  @see javax.servlet.http.HttpSession
  **/
  public javax.servlet.http.HttpSession getHttpSession();

  /** The method <code>getServletContext</code> returns the 
   *  <code>ServletContex</code>t associated with the web 
   *  application that contain this endpoint. According to 
   *  the Servlet specification, There is one context per web
   *  application (installed as a WAR) per JVM . A servlet 
   *  based service endpoint is deployed as part of a web 
   *  application.
   *
   *  @return <code>ServletContext</code>
   *  @see javax.servlet.ServletContext
  **/
  public javax.servlet.ServletContext getServletContext();

  /** Returns a boolean indicating whether the authenticated user
   *  for the current method invocation on the endpoint instance
   *  is included in the specified logical "role".
   *
   *  @param  role a <code>String</code> specifying the name
   *               of the role
   *  @return a <code>boolean</code> indicating whether the
   *           authenticated user associated with the current
   *           method invocation belongs to a given role;
   *           <code>false</code> if the user has not been authenticated 
  **/
    public boolean isUserInRole(String role);

}
