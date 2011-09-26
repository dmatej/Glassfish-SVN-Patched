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
package org.apache.myfaces.trinidadinternal.convert;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFConverter;
import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFJspProperty;
import org.apache.myfaces.trinidad.convert.ClientConverter;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.share.text.RGBColorFormat;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;

/**
 */
@JSFConverter(
        name="tr:convertColor",
        bodyContent="empty",
        id="org.apache.myfaces.trinidad.Color",
        tagClass="org.apache.myfaces.trinidadinternal.taglib.convert.ConvertColorTag")
public class ColorConverter extends org.apache.myfaces.trinidad.convert.ColorConverter
                            implements ClientConverter
{

  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }
  
  public Collection<String> getClientImportNames()
  {
    return _IMPORT_NAMES;
  }

  @SuppressWarnings("unchecked")
  public String getClientScript(FacesContext context, UIComponent component)
  {

    if ( component == null)
    {
      _LOG.severe("NEEDED_COMPONENT_NULL_NO_SCRIPT_WRITTEN");
      return null;
    }

    // Add a JavaScript Object to store the colorfield formats
    // on the client-side.  We currently store the format string
    // for each and every field.  It'd be more efficient to have
    // an array of formats, then store for each field the
    // index of the format, especially if we could delay outputting
    // these objects to when the <form> closes.

    String clientId = component.getClientId(context);

    if (clientId != null)
    {
      // FIX - figure out size!!!
      StringBuilder buff = new StringBuilder();

      Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
      // =-=JRF Only if Javascript...
      if (requestMap.get(_PATTERN_WRITTEN_KEY) == null)
      {
        requestMap.put( _PATTERN_WRITTEN_KEY, Boolean.TRUE);

        buff.append("_cfTrans=\"");
        buff.append(XhtmlUtils.escapeJS(getTransparentString(context)));
        buff.append("\";");

        // only create the _cfs object if it doesn't exist, so we don't
        // wipe out _cfs[xxx] values if we ppr the first color field on a
        // page with multiple color fields.
        buff.append("if(window['_cfs']==undefined){window['_cfs']=new Object();window['_cfts']=new Object();}");
      }

      String[] patterns = getPatterns();
      buff.append("_cfs[\"");
      buff.append(clientId);
      buff.append("\"]=");

      if (patterns.length == 1)
      {
        buff.append("\"");
        buff.append(patterns[0]);
        buff.append("\"");
      }
      else
      {
        buff.append("new Array(");

        for (int i = 0; i < patterns.length; i++)
        {
          buff.append("\"");
          buff.append(patterns[i]);
          buff.append("\"");

          if (i < (patterns.length - 1))
            buff.append(",");
        }

        buff.append(")");
      }

      buff.append(";");

      if (isTransparentAllowed())
      {
        buff.append("_cfts[\"");
        buff.append(clientId);
        buff.append("\"]=true;");
      }

      return buff.toString();
    }
    else
    {
      _LOG.severe("NULL_CLIENT_ID_NO_SCRIPT_RENDERED");
    }

    return null;

  }

  /**
   * @todo add message detail as part of RGBColorFormat constructor.
   */
  public String getClientConversion(FacesContext context, UIComponent component)
  {
    StringBuilder sb = new StringBuilder();

    StringBuilder patterns = new StringBuilder();
    String[] setPatterns = getPatterns();
    for (int i = 0; i < setPatterns.length ; i++)
    {
      patterns.append(setPatterns[i]);
      patterns.append(' ');
    }
    String patternsString = patterns.toString();

    sb.append("new TrColorConverter(");

    _appendPatternsArg(sb);

    if (isTransparentAllowed())
    {
      sb.append(",true,'");
    }
    else
    {
      sb.append(",false,'");
    }
    
    sb.append(XhtmlLafUtils.escapeJS(patternsString));
    
    Map<String, String> messages = new HashMap<String, String>();
    
    String convMsgDet = getMessageDetailConvert();
    if(convMsgDet != null)
    {
      messages.put("detail", convMsgDet);
    }
    
    String hint = getHint();

    if(hint != null)
    {
      messages.put("hint", hint);
    }
    
    sb.append("',");
    try
    {
      JsonUtils.writeMap(sb, messages, false);
    }
    catch (IOException e)
    {
      sb.append("null");
    }
    sb.append(')');

    return sb.toString();
  }

  public int getColumns(
    FacesContext context)
  {
    int columns = 0;

    if (isTransparentAllowed())
      columns = getTransparentString(context).length();

    String[] patterns = getPatterns();
    for (int i=0; i < patterns.length; i++)
      columns = Math.max(columns, new RGBColorFormat(patterns[i]).length());

    return columns;
  }

  // Appends the patterns argument to the StringBuilder
  private void _appendPatternsArg(StringBuilder buffer)
  {
    String[] patterns = getPatterns();
    int count = patterns.length;

    if (count == 1)
    {
      buffer.append("'");
      buffer.append(patterns[0]);
      buffer.append("'");
    }
    else
    {
      buffer.append("new Array(");

      for (int i = 0; i < count; i++)
      {
        buffer.append("'");
        buffer.append(patterns[i]);
        buffer.append("'");

        if (i < (count - 1))
          buffer.append(",");
      }

      buffer.append(")");
    }
  }

  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "ColorFormat()" );
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ColorConverter.class);
  private static final String _PATTERN_WRITTEN_KEY = "org.apache.myfaces.trinidadinternal.convert.ColorConverter._PATTERN_WRITTEN";
}
