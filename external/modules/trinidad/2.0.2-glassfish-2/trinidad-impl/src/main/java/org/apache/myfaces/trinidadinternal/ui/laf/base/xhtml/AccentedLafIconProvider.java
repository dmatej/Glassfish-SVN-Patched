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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.awt.Color;



import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.NameResolver;


import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProvider;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.image.ImageProviderResponse;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;
import org.apache.myfaces.trinidadinternal.ui.laf.base.ColorizedLafIconProvider;
import org.apache.myfaces.trinidadinternal.ui.laf.base.IconKey;
import org.apache.myfaces.trinidadinternal.ui.laf.base.Icon;

/**
 * Abstracts out the retrieval of ImageProviderResponses for
 * dual ramp colorization.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/AccentedLafIconProvider.java#0 $) $Date: 10-nov-2005.18:53:11 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class AccentedLafIconProvider extends ColorizedLafIconProvider
                                              implements XhtmlLafConstants
{

  /**
   * Returns an icon, given its index.
   */
  protected abstract Icon getIcon(IconKey iconKey);

  /**
   * Tests whether the the icon at the specified index is transparent.
   * By default, all icons are considered transparent.
   */
  protected boolean isTransparent(IconKey iconKey)
  {
    return true;
  }

  protected abstract ImageProviderRequest
    createCoreIconRequest(
      ImageContext       context,
      String             source,
      Class<LookAndFeel> lookAndFeel,
      int                direction,
      Color              color,
      Color              surroundingColor,
      NameResolver       resolver);

  protected abstract ImageProviderRequest
    createAccentIconRequest(
      ImageContext       context,
      String             source,
      Class<LookAndFeel> lookAndFeel,
      int                direction,
      Color              color,
      Color              surroundingColor,
      NameResolver       resolver);


  /**
   * Returns an image from the ImageProvider
   */
  @Override
  public ImageProviderResponse getColorizedIcon(
    UIXRenderingContext context,
    IconKey iconKey
    )
  {
    ImageProvider provider = (ImageProvider)
      context.getProperty(ImageConstants.TECATE_NAMESPACE,
                          ImageConstants.IMAGE_PROVIDER_PROPERTY);

    Icon icon = getIcon(iconKey);
    String iconName = null;

    if ( icon != null )
      iconName = icon.getName();

    if (provider == null)
    {
      if (_LOG.isWarning())
        _LOG.warning("CANNOT_GET_IMAGE_PROVIDER_FOR_ICON", iconName);

      return null;
    }

    // Get the context and request objects
    ImageContext imageContext = context.getImageContext();
    ImageProviderRequest request = _getIconRequest(context, iconKey);

    // Make the request
    ImageProviderResponse response = provider.getImage(imageContext, request);

    // Log any problems
    if (response == null)
    {
      if (_LOG.isWarning())
        _LOG.warning("CANNOT_GET_COLORIZED_ICON", iconName);
    }

    return response;
  }


  // Returns the request object for the icon at the specified index
  private ImageProviderRequest _getIconRequest(
    UIXRenderingContext context,
    IconKey iconKey
    )
  {
    // We need three pieces of info for the icon request:

    Icon icon = getIcon(iconKey);

    // Get the index of the icon name in the _ICONS list
    String source = null;
    boolean isDirectionIndependent = false;
    boolean isCoreColor = false;
    Class<LookAndFeel> lookAndFeel = null;
    NameResolver resolver = null;

    if ( icon != null )
    {
      source = icon.getName();
      isDirectionIndependent = icon.isSymmetric();
      isCoreColor = icon.isCoreColor();
      lookAndFeel = icon.getLookAndFeel();
      resolver = icon.getNameResolver();
    }

    if ( source == null )
    {
      _LOG.warning("CANNOT_FIND_ICON_WITH_GIVEN_KEY");
      return null;
    }

    // Get the direction.  If the source icon is symmetrical, we
    // always use the LTR version.
    int direction = -1;

    if (isDirectionIndependent)
      direction = LocaleUtils.DIRECTION_LEFTTORIGHT;
    else
      direction = LocaleUtils.getReadingDirection(context.getLocaleContext());

    assert (direction != -1);


    // Get the color for this request.  The color is either dark or
    // dark accent depending on whether the source icon uses core or
    // accent colors.
    Color color = null;
    ImageContext imageContext = context.getImageContext();

    if (isCoreColor)
      color = _getCoreColor(context);
    else
      color = _getAccentColor(context);

    // Get the surrounding color for this icon.
    Color surroundingColor = _getSurroundingColor(context, iconKey);


    // =-=ags To avoid lots of object allocations, we might want
    //        to keep a pool of IconRequest objects around or something.
    if (isCoreColor)
    {
      return createCoreIconRequest(imageContext,
                                   source,
                                   lookAndFeel,
                                   direction,
                                   color,
                                   surroundingColor,
                                   resolver);
    }

    return createAccentIconRequest(imageContext,
                                   source,
                                   lookAndFeel,
                                   direction,
                                   color,
                                   surroundingColor,
                                   resolver);
  }

  // Gets the Core color to use as the background color when
  // colorizing blue icons.
  private static Color _getCoreColor(UIXRenderingContext context)
  {
    return _getColor(context,
                     BGCOLOR_DARK_STYLE_CLASS,
                     _CORE_COLOR_KEY,
                     _DEFAULT_CORE_COLOR);
  }

  // Gets the Accent color to use as the background color when
  // colorizing tan icons.
  private static Color _getAccentColor(UIXRenderingContext context)
  {
    return _getColor(context,
                     BGACCENT_DARK_STYLE_CLASS,
                     _ACCENT_COLOR_KEY,
                     _DEFAULT_ACCENT_COLOR);
  }


  /**
   * Gets a Color stored as a property on the RenderingContext using the
   * specified key.  Or if not found on, gets the color from the
   * Ocelot StyleMap using the specified style class name.
   */
  private static Color _getColor(
    UIXRenderingContext context,
    String           styleClass,
    Object           key,
    Color            defaultColor
    )
  {
    // First check for the color on the RenderingContext
    Color color = (Color)context.getProperty(UIConstants.MARLIN_NAMESPACE,
                                             key);

    if (color != null)
      return color;

    /***
    // If the color hasn't been stored on the RenderingContext, get it
    // from the style map.
    StyleMap map =context.getStyleContext().getStyleMap();

    if (map != null)
    {
      CoreStyle style = (CoreStyle)map.getStyleByClass(context.getStyleContext(), styleClass);
      if (style != null)
      {
        try
        {
          color = (Color)style.getParsedProperty(CoreStyle.BACKGROUND_KEY);
        }
        catch (PropertyParseException e)
        {
          // This should really be reported at parse time
          _LOG.info(e);
        }
      }
    }
*/

    if (color == null)
      color = defaultColor;

    // Cache the color on the RenderingContext
    context.setProperty(UIConstants.MARLIN_NAMESPACE, key, color);

    return color;
  }

  // Return the surrounding color to use for the icon at
  // the specified index
  private Color _getSurroundingColor(
    UIXRenderingContext context,
    IconKey iconKey
    )
  {
    // If the image is transparent and the Agent does not support
    // transparent images, we grab the current background color off of
    // the style attrs stack
    if (isTransparent(iconKey) &&
        !XhtmlLafUtils.supportsTransparentImages(context))
    {
      return XhtmlLafUtils.getBackgroundColor(context);
    }

    return null;
  }

  // Keys for obtaining colorization colors from the RenderingContext
  private static final String _ACCENT_COLOR_KEY = "_accentColor";
  private static final String _CORE_COLOR_KEY   = "_coreColor";

  // Default values for colorization colors
  private static final Color _DEFAULT_ACCENT_COLOR = new Color(204, 204, 153);
  private static final Color _DEFAULT_CORE_COLOR = new Color(51, 102, 153);
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(AccentedLafIconProvider.class);
}
