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
package org.apache.myfaces.trinidadinternal.image.xml.encode;

import java.awt.Color;
import java.io.PrintWriter;
import java.util.Map;

import org.apache.myfaces.trinidadinternal.image.ImageContext;

/**
 * Encoder for colorized icons
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/encode/ColorizedIconEncoder.java#0 $) $Date: 10-nov-2005.19:04:45 $
 */
public class ColorizedIconEncoder extends AbstractXMLEncoder
{
  /**
   * Creates a ColorizedIconEncoder
   */
  public ColorizedIconEncoder()
  {
  }

  @Override
  protected void encodeAttributes(
    ImageContext context,
    Map<Object, Object> properties, 
    Map<Object, Object> responseProperties,
    PrintWriter out
    )
  {
    super.encodeAttributes(context, properties, responseProperties, out);

    String source = (String)properties.get(SOURCE_KEY);
    encodeAttribute(SOURCE_ATTR, source, out);

    Class<?> laf = (Class<?>)properties.get(LAF_KEY);
    
    if ( laf != null )
    {
      String lafString = laf.getName();
      encodeAttribute(LAF_ATTR, lafString, out);
    }
  }

  /**
   * Override of AbstractXMLEncoder.encodeBody.
   */
  @Override
  protected void encodeBody(
    ImageContext context,
    Map<Object, Object> properties, 
    Map<Object, Object> responseProperties,
    PrintWriter out
    )
  {
    super.encodeBody(context, properties, responseProperties, out);

    Object o;

    if ((o = properties.get(DARK_COLOR_KEY)) != null)
      encodeColor(DARK_COLOR_NAME, (Color)o, out);
    if ((o = properties.get(DARK_ACCENT_COLOR_KEY)) != null)
      encodeColor(DARK_ACCENT_COLOR_NAME, (Color)o, out);
  }
}

