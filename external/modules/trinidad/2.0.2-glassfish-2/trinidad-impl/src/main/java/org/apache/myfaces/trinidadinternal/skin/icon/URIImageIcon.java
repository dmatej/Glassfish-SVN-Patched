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
package org.apache.myfaces.trinidadinternal.skin.icon;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.style.Style;

/**
 * ImageIcon implementation which takes a full URI.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/icon/URIImageIcon.java#0 $) $Date: 10-nov-2005.18:59:05 $
 */
public class URIImageIcon extends BaseImageIcon
{
  /**
   * Creates a URIImageIcon which uses the specified URI
   * regardless of the reading direction.
   *
   * @param uri The full URI to the image
   * @param width An Integer representing the width of the icon, 
   *              or null if the width is not known.
   * @param height An Integer representing the height of the icon, 
   *               or null if the height is not known.
   */
  public URIImageIcon(
    String  uri,
    Integer width,
    Integer height
    )
  {
    this(uri, null, width, height, null, null);
  }

  /**
   * Creates a URIImageIcon which has a different URI depending on the
   * reading direction. 
   *
   * @param uri The URI of the left-to-right version of the image
   * @param rtlURI The URI of the right-to-left version of the image
   * @param width An Integer representing the width of the icon, 
   *              or null if the width is not known.
   * @param height An Integer representing the height of the icon, 
   *               or null if the height is not known.
   */
  public URIImageIcon(
    String  uri,
    String  rtlURI,
    Integer width,
    Integer height
    )
  {
    this(uri, rtlURI, width, height, null, null);
  }

  /**
   * Creates a URIImageIcon which has a different URI depending on the
   * reading direction. 
   *
   * @param uri The URI of the left-to-right version of the image
   * @param rtlURI The URI of the right-to-left version of the image
   * @param width An Integer representing the width of the icon, 
   *              or null if the width is not known.
   * @param height An Integer representing the height of the icon, 
   *               or null if the height is not known.
   * @param styleClass The style class for the image icon
   * @param inlineStyle The inline style for the image icon
   */
  public URIImageIcon(
    String  uri,
    String  rtlURI,
    Integer width,
    Integer height,
    String  styleClass,
    Style   inlineStyle
    )
  {
    super(uri, rtlURI, width, height, styleClass, inlineStyle);
  }

  /**
   * Implementation of BaseImageIcon.getBaseURI().
   * The base URI for URIImageIcon is always null,
   * as a full URI has already been provided.
   */
  @Override
  protected String getBaseURI(
    FacesContext     context,
    RenderingContext arc)
  {
    return null;
  }
}
