/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.encoding;

import java.util.Iterator;
import javax.xml.rpc.JAXRPCException;

/** The javax.xml.rpc.encoding.DeserializerFactory is a factory of 
 *  deserializers. A DeserializerFactory is registered with a 
 *  TypeMapping instance as part of the TypeMappingRegistry.
 *
 *  @version   1.0
 *  @author    Rahul Sharma
 *  @see javax.xml.rpc.encoding.Serializer
**/
public interface DeserializerFactory extends java.io.Serializable {
  
  /** Returns a Deserializer for the specified XML processing
   *  mechanism type.
   * 
   *  @param  mechanismType  XML processing mechanism type [TBD:
   *                         definition of valid constants]
   *  @throws JAXRPCException  If DeserializerFactory does not 
   *          support the specified XML processing mechanism
  **/
  public Deserializer getDeserializerAs(String mechanismType);

  /** Returns a list of all XML processing mechanism types 
   *  supported by this DeserializerFactory.
   *
   *  @return List of unique identifiers for the supported 
   *          XML processing mechanism types
  **/
  public Iterator getSupportedMechanismTypes();
}
