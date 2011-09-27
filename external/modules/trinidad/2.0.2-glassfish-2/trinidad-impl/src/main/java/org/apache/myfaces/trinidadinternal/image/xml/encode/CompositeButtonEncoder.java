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

import java.io.PrintWriter;
import java.util.Map;

import org.apache.myfaces.trinidadinternal.image.ImageContext;

/**
 * Encoder for composite buttons
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/encode/CompositeButtonEncoder.java#0 $) $Date: 10-nov-2005.19:04:45 $
 */
public class CompositeButtonEncoder extends AbstractXMLEncoder
{
  /**
   * Creates a CompositeButtonEncoder
   */
  public CompositeButtonEncoder()
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

    Object o;

    if ((o = properties.get(LOOK_AND_FEEL_ID_KEY)) != null)
      encodeAttribute(LOOK_AND_FEEL_ID_ATTR, o.toString(), out);
    if ((o = properties.get(ACCESS_KEY_KEY)) != null)
      encodeAttribute(ACCESS_KEY_ATTR, ((Character)o).toString(), out);
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

    if ((o = properties.get(TEXT_KEY)) != null)
      encodeValueTag(TEXT_NAME, (String)o, out);
  }
}



