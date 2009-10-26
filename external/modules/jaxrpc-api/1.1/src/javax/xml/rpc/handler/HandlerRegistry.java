/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.handler;

import java.util.List;
import javax.xml.rpc.JAXRPCException;
import javax.xml.namespace.QName;

/** The <code>javax.xml.rpc.handler.HandlerRegistry</code> 
 *  provides support for the programmatic configuration of 
 *  handlers in a <code>HandlerRegistry</code>.
 *
 *  <p>A handler chain is registered per service endpoint, as 
 *  indicated by the qualified name of a port. The getHandlerChain
 *  returns the handler chain (as a java.util.List) for the 
 *  specified service endpoint. The returned handler chain is 
 *  configured using the java.util.List interface. Each element 
 *  in this list is required to be of the Java type 
 *  <code>javax.xml.rpc.handler.HandlerInfo</code>.
 *
 *  @version 1.0
 *  @author  Rahul Sharma
 *  @see javax.xml.rpc.Service
**/

public interface HandlerRegistry extends java.io.Serializable {

  /** Gets the handler chain for the specified service endpoint.
   *  The returned <code>List</code> is used to configure this
   *  specific handler chain in this <code>HandlerRegistry</code>.
   *  Each element in this list is required to be of the Java type 
   *  <code>javax.xml.rpc.handler.HandlerInfo</code>.
   *
   *  @param portName Qualified name of the target service endpoint
   *  @return java.util.List Handler chain
   *  @throws java.lang.IllegalArgumentException If an invalid
   *          <code>portName</code> is specified
  **/
  public java.util.List getHandlerChain(QName portName);

  /** Sets the handler chain for the specified service endpoint
   *  as a <code>java.util.List</code>. Each element in this list
   *  is required to be of the Java type 
   *  <code>javax.xml.rpc.handler.HandlerInfo</code>.
   *
   *  @param portName Qualified name of the target service endpoint
   *  @param chain    A List representing configuration for the
   *                  handler chain
   *  @throws JAXRPCException If any error in the configuration of
   *                  the handler chain
   *  @throws java.lang.UnsupportedOperationException If this
   *          set operation is not supported. This is done to
   *          avoid any overriding of a pre-configured handler
   *          chain.
   *  @throws java.lang.IllegalArgumentException If an invalid
   *          <code>portName</code> is specified
  **/
  public void setHandlerChain(QName portName, java.util.List chain);

}
