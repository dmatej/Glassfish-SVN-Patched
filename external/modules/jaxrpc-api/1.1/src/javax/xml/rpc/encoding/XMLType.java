/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.encoding;

/** QNames for XML Types
 *
 *  @version   1.0
 *  @author    Rahul Sharma
**/

import javax.xml.namespace.QName;
import javax.xml.rpc.NamespaceConstants;

/** Constants for common XML Schema and SOAP 1.1 types.
 *  @version 1.0
 *  @author  Rahul Sharma
**/

public class XMLType {
  
  /** The name of the <code>xsd:string</code> type.
  **/
  public static final QName XSD_STRING = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "string");
  /** The name of the <code>xsd:float</code> type.
  **/
  public static final QName XSD_FLOAT = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "float");
  /** The name of the <code>xsd:boolean</code> type.
  **/
  public static final QName XSD_BOOLEAN = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "boolean");
  /** The name of the <code>xsd:double</code> type.
  **/
  public static final QName XSD_DOUBLE = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "double");
  /** The name of the <code>xsd:integer</code> type.
  **/
  public static final QName XSD_INTEGER = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "integer");
  /** The name of the <code>xsd:int</code> type.
  **/
  public static final QName XSD_INT = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "int");
  /** The name of the <code>xsd:long</code> type.
  **/
  public static final QName XSD_LONG = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "long");
  /** The name of the <code>xsd:short</code> type.
  **/
  public static final QName XSD_SHORT = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "short");
  /** The name of the <code>xsd:decimal</code> type.
  **/
  public static final QName XSD_DECIMAL = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "decimal");
  /** The name of the <code>xsd:base64Binary</code> type.
  **/
  public static final QName XSD_BASE64 = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "base64Binary");
  /** The name of the <code>xsd:hexBinary</code> type.
  **/
  public static final QName XSD_HEXBINARY = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "hexBinary");
  /** The name of the <code>xsd:byte</code> type.
  **/
  public static final QName XSD_BYTE = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "byte");
  /** The name of the <code>xsd:dateTime</code> type.
  **/
  public static final QName XSD_DATETIME = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "dateTime");
  /** The name of the <code>xsd:QName</code> type.
  **/
  public static final QName XSD_QNAME = new QName(
		NamespaceConstants.NSURI_SCHEMA_XSD, "QName");

  /** The name of the <code>SOAP-ENC:string</code> type.
  **/
  public static final QName SOAP_STRING = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "string");
  /** The name of the <code>SOAP-ENC:boolean</code> type.
  **/
  public static final QName SOAP_BOOLEAN = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "boolean");
  /** The name of the <code>SOAP-ENC:double</code> type.
  **/
  public static final QName SOAP_DOUBLE = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "double");
  /** The name of the <code>SOAP-ENC:base64</code> type.
  **/
  public static final QName SOAP_BASE64 = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "base64");
  /** The name of the <code>SOAP-ENC:float</code> type.
  **/
  public static final QName SOAP_FLOAT = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "float");
  /** The name of the <code>SOAP-ENC:int</code> type.
  **/
  public static final QName SOAP_INT = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "int");
  /** The name of the <code>SOAP-ENC:long</code> type.
  **/
  public static final QName SOAP_LONG = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "long");
  /** The name of the <code>SOAP-ENC:short</code> type.
  **/
  public static final QName SOAP_SHORT = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "short");
  /** The name of the <code>SOAP-ENC:byte</code> type.
  **/
  public static final QName SOAP_BYTE = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "byte");
  /** The name of the <code>SOAP-ENC:Array</code> type.
  **/
  public static final QName SOAP_ARRAY = new QName(
		NamespaceConstants.NSURI_SOAP_ENCODING, "Array");
}
