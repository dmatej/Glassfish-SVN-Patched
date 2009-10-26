/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.encoding;

import java.io.Serializable;

/** The javax.xml.rpc.encoding.Serializer interface defines the 
 *  base interface for serializers. A Serializer converts
 *  a Java object to an XML representation using a specific XML
 *  processing mechanism and based on the specified type 
 *  mapping and encoding style. 
 *
 *  @version   1.0
 *  @author    Rahul Sharma
**/
public interface Serializer extends java.io.Serializable {

  /** Gets the type of the XML processing mechanism and 
   *  representation used by this Serializer.
   *
   *  @return XML processing mechanism type
  **/
  public String getMechanismType();
                                
}
