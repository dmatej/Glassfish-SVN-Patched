/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc;

/** The <code>javax.xml.rpc.ParameterMode</code> is a type-safe
 *  enumeration for parameter mode. This class is used in the
 *  <code>Call</code>API to specify parameter passing modes.
 *
 *  @version 1.0
 *  @author  Rahul Sharma
 *  @see javax.xml.rpc.Call
**/

public class ParameterMode {
  
  private final String mode;

  private ParameterMode(String mode) { 
    this.mode = mode; 
  }

  /** Returns a <code>String</code> describing this <code>ParameterMode</code> object. 
   * 
   *  @return  A string representation of the object.
  **/
  public String toString() { return mode; }

  /** IN mode for parameter passing
  **/
  public static final ParameterMode IN = new ParameterMode("IN");

  /** OUT mode for parameter passing
  **/
  public static final ParameterMode OUT = new ParameterMode("OUT");

  /** INOUT mode for parameter passing
  **/
  public static final ParameterMode INOUT  = 
		      new ParameterMode("INOUT");

}
