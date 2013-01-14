/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.encoding;

import java.io.Serializable;

/** The javax.xml.rpc.encoding.Deserializer interface defines a
 *  base interface for deserializers. A Deserializer converts
 *  an XML representation to a Java object using a specific XML
 *  processing mechanism and based on the specified type 
 *  mapping and encoding style. 
 *
 *  @version   1.0
 *  @author    Rahul Sharma
**/
public interface Deserializer extends java.io.Serializable {

  /** Gets the type of the XML processing mechanism and 
   *  representation used by this Deserializer.
   *  @return XML processing mechanism type
  **/
  public String getMechanismType();
                                
}
