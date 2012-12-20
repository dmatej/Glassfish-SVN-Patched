/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.encoding;

import java.util.Iterator;
import javax.xml.rpc.JAXRPCException;

/** The javax.xml.rpc.encoding.SerializerFactory is a factory of 
 *  the serializers. A SerializerFactory is registered with a 
 *  TypeMapping object as part of the TypeMappingRegistry.
 *
 *  @version   1.0
 *  @author    Rahul Sharma
 *  @see javax.xml.rpc.encoding.Serializer
**/
public interface SerializerFactory extends java.io.Serializable {
 
  /** Returns a Serializer for the specified XML processing
   *  mechanism type.
   * 
   *  @param  mechanismType  XML processing mechanism type [TBD:
   *                         definition of valid constants]
   *  @throws JAXRPCException  If SerializerFactory does not 
   *          support the specified XML processing mechanism
   *  @throws java.lang.IllegalArgumentException If an invalid
   *          mechanism type is specified.
  **/
  public Serializer getSerializerAs(String mechanismType);

  /** Returns a list of all XML processing mechanism types 
   *  supported by this SerializerFactory.
   *
   *  @return List of unique identifiers for the supported 
   *          XML processing mechanism types
  **/
  public Iterator getSupportedMechanismTypes();

}
