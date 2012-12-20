/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc;

/** Constants used in JAX-RPC for namespace prefixes and URIs
 *  @version 1.0
 *  @author  Rahul Sharma
**/

public class NamespaceConstants {

  /** Namespace prefix for SOAP Envelope
  **/
  public static final String NSPREFIX_SOAP_ENVELOPE = "soapenv"; 

  /** Namespace prefix for SOAP Encoding
  **/
  public static final String NSPREFIX_SOAP_ENCODING = "soapenc"; 

  /** Namespace prefix for XML schema XSD
  **/
  public static final String NSPREFIX_SCHEMA_XSD    = "xsd"; 

  /** Namespace prefix for XML Schema XSI
  **/
  public static final String NSPREFIX_SCHEMA_XSI    = "xsi"; 

  /** Nameapace URI for SOAP 1.1 Envelope
  **/
  public static final String NSURI_SOAP_ENVELOPE    = 
	    "http://schemas.xmlsoap.org/soap/envelope/";

  /** Nameapace URI for SOAP 1.1 Encoding
  **/  
  public static final String NSURI_SOAP_ENCODING    =
	    "http://schemas.xmlsoap.org/soap/encoding/";

  /** Nameapace URI for SOAP 1.1 next actor role
  **/
  public static final String NSURI_SOAP_NEXT_ACTOR  =
	    "http://schemas.xmlsoap.org/soap/actor/next";

  /** Namespace URI for XML Schema XSD
  **/
  public static final String NSURI_SCHEMA_XSD = 
            "http://www.w3.org/2001/XMLSchema";


  /** Namespace URI for XML Schema XSI
  **/
  public static final String NSURI_SCHEMA_XSI =
            "http://www.w3.org/2001/XMLSchema-instance";

}
