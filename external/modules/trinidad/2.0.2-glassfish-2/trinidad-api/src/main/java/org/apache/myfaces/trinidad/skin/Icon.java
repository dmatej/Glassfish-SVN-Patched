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
package org.apache.myfaces.trinidad.skin;

import java.io.IOException;
import java.io.InputStream;

import java.util.Map;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * An object which represents a customizable icon that is capable of
 * rendering itself.  Icons objects are registered with a Skin
 * via the Skin.registerIcon() method and are retrieved by Renderers
 * via Skin.getIcon().  Customizers can override icons declaratively
 *in the skin css file.
 * <p>
 * Icon implementations which are capable of providing an image
 * representation of the icon should override getImageURI(), getImageWidth()
 * and getImageHeight().  The default implementations of these methods
 * return null, which is an acceptable default for Icons which do
 * not make use of images (ie. for text-based Icon implementations).
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/icon/Icon.java#0 $) $Date: 10-nov-2005.18:59:03 $
 */
abstract public class Icon
{
  // keys that can be used in the attribute Map in renderIcon
  public static final String SHORT_DESC_KEY = "shortDesc";
  public static final String ALIGN_KEY = "align";
  public static final String WIDTH_KEY = "width";
  public static final String HEIGHT_KEY = "height";
  public static final String EMBEDDED_KEY = "embedded";
  public static final String ID_KEY = "id";
  public static final String STYLE_CLASS_KEY = "styleClass";
  public static final String INLINE_STYLE_KEY = "inlineStyle";

  /**
   * Renders the Icon.
   *
   * @param context FacesContext 
   * @param arc The RenderingContext for the
   *                                 current request.
   * @param attrs A Map which which provides access to
   *             values that might be useful to Icon implementations,
   *             such as "id" and "shortDesc".
   */
  abstract public void renderIcon(
    FacesContext context,
    RenderingContext arc,
    Map<String, ? extends Object> attrs
    ) throws IOException;

  /**
   * Returns a URI to the image that is used as the icon's content.
   * The default implementation of this method simply returns null,
   * which indicates to the caller that no image representation of
   * the Icon is available.
   * @param context FacesContext
   * @param arc The RenderingContext for the
   *                                 current request.
   */
  public Object getImageURI(
    FacesContext        context,
    RenderingContext arc)
  {
    return null;
  }

  /**
   * Returns the width of the image.
   * The default implementation of this method simply returns null,
   * which indicates to the caller the width of the image is not
   * known - or that the Icon does not provide an image representation.
   * @param arc RenderingContext 
   */
  public Integer getImageWidth(RenderingContext arc)
  {
    return null;
  }

  /**
   * Returns the height of the image.
   * The default implementation of this method simply returns null,
   * which indicates to the caller the height of the image is not
   * known - or that the Icon does not provide an image representation.
   * @param arc RenderingContext 
   *
   */
  public Integer getImageHeight(RenderingContext arc)
  {
    return null;
  }

  /**
   * Returns an InputStream which provides access to the
   * image data for image-based Icons.
   * @param context FacesContext 
   * @param arc The RenderingContext for the
   *                                 current request.
   */
  // TODO: Delete if unnecessary
  public InputStream openStream(
    FacesContext        context,
    RenderingContext arc
    ) throws IOException
  {
    // This operation is not supported by default.  Log a warning.
    if (_LOG.isWarning())
      _LOG.warning("UNABLE_RETRIEVE_IMAGE_DATA", getClass().getName());

    return null;
  }

  /**
   * Returns true if the icon is non-existent, and will render no content
   * at all.
   */
  public boolean isNull()
  {
    return false;
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(Icon.class);

}
