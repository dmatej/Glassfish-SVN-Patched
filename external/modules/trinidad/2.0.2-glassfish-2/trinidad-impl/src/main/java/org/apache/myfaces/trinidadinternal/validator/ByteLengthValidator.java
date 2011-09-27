/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.validator;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFValidator;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;

/**
 * <p>Enables byte length validation at the client side. </p>
 *
 */
@JSFValidator(
        name="tr:validateByteLength",
        bodyContent="empty",
        id="org.apache.myfaces.trinidad.ByteLength",
        tagClass="org.apache.myfaces.trinidadinternal.taglib.validator.ValidateByteLengthTag")
public class ByteLengthValidator
              extends org.apache.myfaces.trinidad.validator.ByteLengthValidator
              implements ClientValidator
{
  public ByteLengthValidator()
  {
    super();
  }

  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }

  /**
   * {@inheritDoc}
   */
  public String getClientScript(
    FacesContext context,
    UIComponent component
    )
  {
    return null;
  }

  /**
   * {@inheritDoc}
   */
  public String getClientValidation(
    FacesContext context,
    UIComponent component
    )
  {

    int encodingType = _getType(getEncoding());

    //to create the constructor.
    StringBuilder constr = new StringBuilder(150);
    switch(encodingType)
    {
      case _SINGLE_BYTE_TYPE :
      {
        constr.append("new SBFormat(");
        break;
      }
      case _CJK_TYPE :
      {
        constr.append("new CjkFormat(");
        break;
      }
      case _UTF8_TYPE :
      {
        constr.append("new Utf8Format(");
        break;
      }
      // encoding supported by JVM but not by us at the client side
      case _UNSUPPORTED_TYPE :
      {
        return null;
      }
      default :
      {
        assert false : "Unexpected Encoding type " + encodingType +
                       " Encoding is " + getEncoding();
      }
    }

    String maxLength = String.valueOf(getMaximum());
    constr.append(maxLength);
    
    _applyCustomMessages(context, constr, maxLength);
        
    constr.append(")");

    return constr.toString();
  }

  /**
   * {@inheritDoc}
   */
  public Collection<String> getClientImportNames()
  {

    switch (_getType(getEncoding()))
    {
      case _SINGLE_BYTE_TYPE:
        return _IMPORT_NAMES_SINGLE_BYTE;
      case _CJK_TYPE:
        return _IMPORT_NAMES_CJK;
      case _UTF8_TYPE:
        return _IMPORT_NAMES_UTF8;

      case _UNSUPPORTED_TYPE:
      default:
        return null;
    }
  }
  
  private void _applyCustomMessages(FacesContext context, StringBuilder constr, String maxLength)
  {
    Map<String, String> messages = new HashMap<String, String>();
    
    String maxMsgDetail = getMessageDetailMaximum();
    if(maxMsgDetail != null)
    {
      messages.put("detail", maxMsgDetail);
    }
    
    String hintMax = getHintMaximum();
    if(hintMax != null)
    {
      messages.put("hint", hintMax);
    }
    
    constr.append(',');
    try
    {
      JsonUtils.writeMap(constr, messages, false);
    }
    catch (IOException e)
    {
      constr.append("null");
    }
  }

  static private int _getType(String encoding)
  {

    // After null check, this should be the first check to see that the
    // encoding is supported. It is possible that encodings which are listed
    // as being part of _cjkEncodings and singlByteEncodings might not be
    // supported by the JVM
    if ( !Charset.isSupported(encoding))
      throw new IllegalCharsetNameException(_LOG.getMessage(
        "ENCODING_UNSUPPORTED_BY_JVM", encoding));

    encoding = encoding.toLowerCase();

    if ("utf-8".equals(encoding))
      return _UTF8_TYPE;
    else if (_cjkEncodings.contains(encoding))
      return _CJK_TYPE;
    else if (_singleByteEncodings.contains(encoding))
      return _SINGLE_BYTE_TYPE;

   /**
    * When we come to this point, it means that the JVM supports the given
    * encoding but we don't support it. So we log a message and render no
    * script to the client side, hence no client side validation will be
    * performed for component associated with this validator.
    */
    if (_LOG.isWarning())
    {
      _LOG.warning("CLIENT_SIDE_ENCODING_NOT_SUPPORTED", encoding);
    }
    return _UNSUPPORTED_TYPE;
  }

  // Type where all characters are single byte
  static private final int _SINGLE_BYTE_TYPE = 0;

  // Standard set of CJK characters (but _not_ GB18030  or EUC)
  static private final int _CJK_TYPE         = 1;

  static private final int _UTF8_TYPE        = 2;

  // Unsupported type - perform validation on the server only
  static private final int _UNSUPPORTED_TYPE = 3;

  static private Set<String> _cjkEncodings = new HashSet<String>();

  static private Set<String> _singleByteEncodings = new HashSet<String>();

  static
  {
    // List of CJK encoding we support - note that this _does not_
    // include GB18030 or the EUC encodings, as these have very
    // different byte length tables.
    String[] cjkArray = new String[] {
      "shift_jis",
      "ms_kanji",
      "csshiftjis",
      "windows-31j",
      "cswindows31j",
      "ks_c_5601-1987",
      "iso-ir-149",
      "ks_c_5601-1989",
      "ksc_5601",
      "korean",
      "csksc56011987",
      "euc-kr",
      "cseuckr",
      "windows-949",
      "gb2312",
      "csgb2312",
      "chinese",
      "csiso58gb231280",
      "hz-gb-2312",
      "gbk",
      "cp936",
      "ms936",
      "windows-936",
      "big5",
      "csbig5",
      "windows-950"
    };

    for (int i = 0; i < cjkArray.length; i++)
      _cjkEncodings.add(cjkArray[i]);

    // List of known Java single-byte character sets (lowercased)
    String[] singleByteArray = new String[] {
      "ascii",
      "iso646-us",
      "iso_8859-1:1987",
      "iso-ir-100",
      "iso_8859-1",
      "iso-8859-1",
      "latin1",
      "l1",
      "ibm819",
      "cp819",
      "csisolatin1",
      "iso_8859-2:1987",
      "iso-ir-101",
      "iso_8859-2",
      "iso-8859-2",
      "latin2",
      "l2",
      "csisolatin2",
      "iso_8859-3:1988",
      "iso-ir-109",
      "iso_8859-3",
      "iso-8859-3",
      "latin3",
      "l3",
      "csisolatin3",
      "iso_8859-4:1988",
      "iso-ir-110",
      "iso_8859-4",
      "iso-8859-4",
      "latin4",
      "l4",
      "csisolatin4",
      "iso_8859-5:1988",
      "iso-ir-144",
      "iso_8859-5",
      "iso-8859-5",
      "cyrillic",
      "csisolatincyrillic",
      "iso_8859-6:1987",
      "iso-ir-127",
      "iso_8859-6",
      "iso-8859-6",
      "ecma-114",
      "asmo-708",
      "arabic",
      "csisolatinarabic",
      "iso_8859-7:1987",
      "iso-ir-126",
      "iso_8859-7",
      "iso-8859-7",
      "elot_928",
      "ecma-118",
      "greek",
      "greek8",
      "csisolatingreek",
      "iso_8859-8:1988",
      "iso-ir-138",
      "iso_8859-8",
      "iso-8859-8",
      "hebrew",
      "csisolatinhebrew",
      "iso-ir-148",
      "iso_8859-9",
      "iso-8859-9",
      "latin5",
      "l5",
      "csisolatin5",
      "iso-8859-13",
      "iso-8859-15",
      "iso_8859-15",
      "ibm037",
      "cp037",
      "ibm273",
      "cp273",
      "ibm277",
      "ibm278",
      "cp278",
      "278",
      "ibm280",
      "cp280",
      "ibm284",
      "cp284",
      "ibm285",
      "cp285",
      "ibm297",
      "cp297",
      "ibm420",
      "cp420",
      "420",
      "ibm424",
      "cp424",
      "ibm437",
      "cp437",
      "437",
      "cspc8codepage437",
      "ibm500",
      "cp500",
      "ibm775",
      "cp775",
      "ibm850",
      "cp850",
      "850",
      "cspc850multilingual",
      "ibm852",
      "cp852",
      "852",
      "cspcp852",
      "ibm855",
      "cp855",
      "855",
      "cspcp855",
      "ibm857",
      "cp857",
      "857",
      "csibm857",
      "ibm860",
      "cp860",
      "860",
      "csibm860",
      "ibm861",
      "cp861",
      "cp-is",
      "861",
      "csibm861",
      "ibm862",
      "cp862",
      "862",
      "cspc862latinhebrew",
      "ibm863",
      "cp863",
      "863",
      "csibm863",
      "ibm864",
      "cp864",
      "csibm864",
      "ibm865",
      "cp865",
      "865",
      "csibm865",
      "ibm866",
      "cp866",
      "866",
      "csibm866",
      "ibm868",
      "cp868",
      "ibm869",
      "cp869",
      "869",
      "cp-gr",
      "csibm869",
      "ibm870",
      "cp870",
      "ibm871",
      "cp871",
      "ibm918",
      "cp918",
      "ibm1026",
      "cp1026",
      "windows-1250",
      "windows-1251",
      "windows-1252",
      "windows-1253",
      "windows-1254",
      "windows-1255",
      "windows-1256",
      "windows-1257",
      "windows-1258",
      "koi8-r",
      "cskoi8r",
    };

    for (int i = 0; i < singleByteArray.length; i++)
      _singleByteEncodings.add(singleByteArray[i]);
  }
  private static final Collection<String> _IMPORT_NAMES_SINGLE_BYTE = Collections.singletonList("SBFormat()" ); 
  private static final Collection<String> _IMPORT_NAMES_CJK = Collections.singletonList("CjkFormat()" ); 
  private static final Collection<String> _IMPORT_NAMES_UTF8 = Collections.singletonList("Utf8Format()" ); 
  
 private static final TrinidadLogger _LOG =  TrinidadLogger.createTrinidadLogger(ByteLengthValidator.class);


}
