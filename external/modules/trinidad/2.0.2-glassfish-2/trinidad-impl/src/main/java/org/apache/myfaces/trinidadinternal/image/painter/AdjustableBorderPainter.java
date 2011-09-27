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
package org.apache.myfaces.trinidadinternal.image.painter;


import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.apache.myfaces.trinidadinternal.style.PropertyParseException;
import org.apache.myfaces.trinidadinternal.style.CoreStyle;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;


/**
 * A border that insets painting of the wrapped painter by an amount
 * which is adjustable from font to font.  The font-specific offsets
 * are defined by named styles in an XSS document.  Style names for
 * each font are dervied by prepending the provided prefix to the font
 * name.  For example, if the prefix is "BLAFServerButtonPadding"
 * the style with the name "BLAFServerButtonPaddingDialog" defines the insets
 * for the "Dialog" font.  Padding values for each inset are defined using
 * the CSS style properties "padding-top", "padding-bottom", "padding-left"
 * and "padding-right".
 * This class and everything in its package needs to be deleted.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/AdjustableBorderPainter.java#0 $) $Date: 10-nov-2005.19:04:53 $
 */
public class AdjustableBorderPainter extends AbstractBorderPainter
{
  /**
   * Creates a BorderPainter that insets painting by the specified amount.
   * <p>
   * The specified insets are used as default values if font-specific
   * insets are not found when measuring/painting the border.
   * <p>
   * @param wrappedPainter Painter to wrap this border around.
   * @param top            Amount to inset painting from the top.
   * @param left           Amount to inset painting from the left.
   * @param bottom         Amount to inset painting from the bottom.
   * @param right          Amount to inset painting from the right.
   * @param styleNamePrefix The prefix to use for the style name
   *                        which defines the insets for this instance.
   *
   */
  public AdjustableBorderPainter(
    Painter wrappedPainter,
    int     top,
    int     left,
    int     bottom,
    int     right,
    String  styleNamePrefix
    )
  {
    super(wrappedPainter);

    _defaultInsets = new ImmInsets(top, left, bottom, right);
    _styleNamePrefix = styleNamePrefix;
  }

  /**
   * Returns the insets of just this BorderPainter.  These are the insets
   * that were passed into our constructor.
   * <p>
   * @param context PaintContext to use when getting our own Insets
   * <p>
   * @return The insets of just this BorderPainter.
   * <p>
   * @see org.apache.myfaces.trinidadinternal.image.painter.AbstractBorderPainter#getInsets
   */
  @Override
  protected ImmInsets getOwnInsets(
    PaintContext context
    )
  {
    int top = _defaultInsets.top;
    int left = _defaultInsets.left;
    int bottom = _defaultInsets.bottom;
    int right = _defaultInsets.right;

    CoreStyle style = _getPaddingStyle(context);
    if (style != null)
    {
      top = _getPadding(style, _TOP_PADDING, top);
      left = _getPadding(style, _LEFT_PADDING, left);
      bottom = _getPadding(style, _BOTTOM_PADDING, bottom);
      right = _getPadding(style, _RIGHT_PADDING, right);
    }

    if (context.getReadingDirection() == LocaleUtils.DIRECTION_RIGHTTOLEFT)
      return new ImmInsets(top, right, bottom, left);

    return new ImmInsets(top, left, bottom, right);
  }


  // Get the Style object which contains padding information
  // for this specific paint.
  private CoreStyle _getPaddingStyle(PaintContext context)
  {
    return null;
  }

  // Returns the padding value for the specified
  // property name.  If the style does not define
  // a property with the specified name, or if the
  // value can not be converted to an int, the
  // defaultValue is used.
  private static int _getPadding(
    CoreStyle  style,
    String propertyName,
    int    defaultValue
    )
  {
    if (style != null)
    {
      String value = style.getProperties().get(propertyName);

      if (value != null)
      {
        Integer parsedValue = null;

        try
        {
          parsedValue = CSSUtils.parseLength(value);
        }
        catch (PropertyParseException e)
        {
          // Blow this off - this should be caught at parse time.
          ;
        }

        if (parsedValue != null)
          return parsedValue.intValue();
      }
    }

    return defaultValue;
  }

  //
  // PRIVATE INSTANCE VARIABLES
  //
  private ImmInsets _defaultInsets;

  // The prefix to use when looking up the font-specific style
  private String _styleNamePrefix;

  // Constants for padding property names
  private static final String _TOP_PADDING    = "padding-top";
  private static final String _LEFT_PADDING   = "padding-left";
  private static final String _BOTTOM_PADDING = "padding-bottom";
  private static final String _RIGHT_PADDING  = "padding-right";
}
