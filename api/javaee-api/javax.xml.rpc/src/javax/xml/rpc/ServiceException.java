/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc;

/** The <code>javax.xml.rpc.ServiceException</code> is thrown from the
 *  methods in the <code>javax.xml.rpc.Service</code> interface and
 *  <code>ServiceFactory</code> class.
 *
 *  @version 1.0
 *  @author  Rahul Sharma
**/
public class ServiceException extends java.lang.Exception {
  
  private Throwable cause;

  /** Constructs a new exception with <code>null</code> as its 
   *  detail message. The cause is not initialized.
  **/
  public ServiceException() { 
    super();
    this.cause = null;
  }

  /** Constructs a new exception with the specified detail 
   *  message.  The cause is not initialized.
   *  @param message The detail message which is later 
   *                 retrieved using the <code>getMessage</code> method
  **/
  public ServiceException(String message) {
    super(message);
    this.cause = null;
  }

  /** Constructs a new exception with the specified detail 
   *  message and cause.
   *
   *  @param message The detail message which is later retrieved
   *                 using the <code>getMessage</code> method
   *  @param cause   The cause which is saved for the later
   *                 retrieval throw by the <code>getCause</code> 
   *                 method 
  **/ 
  public ServiceException(String message, Throwable cause) {
    super(message);
    this.cause = cause;
  }

  /** Constructs a new exception with the specified cause
   *  and a detail message of <tt>(cause==null ? null : 
   *  cause.toString())</tt> (which typically contains the 
   *  class and detail message of <tt>cause</tt>).
   *
   *  @param cause   The cause which is saved for the later
   *                 retrieval throw by the getCause method.
   *                 (A <tt>null</tt> value is permitted, and
   *                 indicates that the cause is nonexistent or
     *               unknown.)
  **/ 
  public ServiceException(Throwable cause) {
    super(cause==null ? null : cause.toString());
    this.cause = cause;
  }

  /** Gets the Linked cause
   * 
   *  @return The cause of this Exception or <code>null</code>
   *          if the cause is noexistent or unknown
  **/
  public Throwable getLinkedCause() {
    return this.cause;
  }
}
