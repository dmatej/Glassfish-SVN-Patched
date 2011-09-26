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


import java.awt.Dimension;
import java.awt.Graphics;

/**
 * Abstract baseclass combining a BorderPainter with the ability to
 * wrap other components.  Most BorderPainters are subclasses
 * of this class.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/AbstractBorderPainter.java#0 $) $Date: 10-nov-2005.19:04:51 $
 */
public abstract class AbstractBorderPainter extends AbstractWrappingPainter
                                            implements BorderPainter
{
  /**
   * Creates an AbstractBorderPainter.
   */
  public AbstractBorderPainter()
  {
    // by default we always add on the insets
    _init(null, true);
  }


  /**
   * Creates an AbstractBorderPainter wrapping the <b>wrappedPainter</b>
   * <p>
   * @param wrappedPainter Painter to wrap this border around.
   */
  public AbstractBorderPainter(
    Painter wrappedPainter
    )
  {
    this(wrappedPainter, true);
  }


  /**
   * Creates an AbstractBorderPainter wrapping the <b>wrappedPainter</b>
   * and using <b>alwaysAddInsets</b> to determine whether to add the
   * border's insets when computing its size.
   * <p>
   * @param wrappedPainter  Painter to wrap this border around.
   * @param alwaysAddInsets True if the border should always add its insets
   *                        to the size of the wrapped painter when computing
   *                        its size, even if the wrapped painter's size is
   *                        zero.
   */
  public AbstractBorderPainter(
    Painter wrappedPainter,
    boolean alwaysAddInsets
    )
  {
    super(wrappedPainter);

    _init(wrappedPainter, alwaysAddInsets);
  }



  /**
   * Returns the preferred size of the AbstractBorderPainter.
   * <p>
   * In general, BorderPainters should not need to override this method,
   * since it already accounts for the Border's insets.
   * <p>
   * @param context Context for determining the preferred size.
   * <p>
   * @return The preferred size of the Painter.
   */
  @Override
  public Dimension getPreferredSize(
    PaintContext context
    )
  {
    return _addInsets(context, super.getPreferredSize(context));
  }


  /**
   * Returns the minimum size of the AbstractBorderPainter.
   * <p>
   * In general, BorderPainters should not need to override this method,
   * since it already accounts for the Border's insets.
   * <p>
   * @param context Context for determining the minimum size.
   * <p>
   * @return The minimum size of the Painter.
   */
  @Override
  public Dimension getMinimumSize(
    PaintContext context
    )
  {
    return _addInsets(context, super.getMinimumSize(context));
  }



  /**
   * Paints the Painter wrapped by the AbstractBorderPainter with the
   * AbstractBorderPainter painted around it.  Subclassers that wish
   * to override the way the border paints itself should override
   * <i>paintBorder</i> rather than this method.
   * <p>
   * Painters that modify the Graphics context are responsible for
   * returning it to its initial state when done painting.  Further
   * Painters that paint outside the bounds of <b>x</b>, <b>y</b>,
   * <b>width</b>, and <b>height</b> are responsible for clipping
   * themselves to the bounds.
   * <p>
   * @param context PaintContext to use when painting
   * @param g       Graphics context to use when painting
   * @param x       x coordinate to begin painting at
   * @param y       y coordinate to begin painting at
   * @param width   width of area to paint
   * @param height  height of area to paint
   * <p>
   * @see #paintBorder
   */
  @Override
  public final void paint(
    PaintContext context,
    Graphics     g,
    int          x,
    int          y,
    int          width,
    int          height
    )
  {
    // paint ourselves
    paintBorder(context, g, x, y, width, height);

    ImmInsets borderSizes = getOwnInsets(context);

    // paint the wrapped component inside of us
    super.paint(context,
                g,
                x + borderSizes.left,
                y + borderSizes.top,
                width  - borderSizes.left - borderSizes.right,
                height - borderSizes.top  - borderSizes.bottom);
  }


  /**
   * Returns the insets of this stack of BorderPainters. Subclasses of
   * AbstractBorderPainter that wish to modify the insets of their
   * border should override <i>getOwnInsets</i> rather than this method.
   * <p>
   * @param context PaintContext to use when getting the insets
   * <p>
   * @return The insets of this stack of BorderPainters
   * <p>
   * @see #getOwnInsets
   */
  public final ImmInsets getInsets(
    PaintContext context
    )
  {
    ImmInsets ownInsets = getOwnInsets(context);

    // add in the insets of any wrapped borders
    if (_wrappedBorderPainter != null)
    {
      ImmInsets wrappedInsets = _wrappedBorderPainter.getInsets(context);

      ownInsets = new ImmInsets(ownInsets.top    + wrappedInsets.top,
                           ownInsets.left   + wrappedInsets.left,
                           ownInsets.bottom + wrappedInsets.bottom,
                           ownInsets.right  + wrappedInsets.right);
    }
    
    return ownInsets;
  }


  /**
   * Returns the amount of space by which fills
   * should be inset.  By default, just returns getInsets().
   * <p>
   * @param context the context for painting
   */
  public final ImmInsets getFillInsets(PaintContext context)
  {
    // add in the insets of any wrapped borders
    if (_wrappedBorderPainter != null)
    {
      // fill insets of our wrapped border painter
      ImmInsets wrappedInsets = _wrappedBorderPainter.getFillInsets(context);

      //
      // if the border painter that we are wrapping doesn't have any fill insets
      // then we need to retrun our own fill insets.  Otherwise, we need to add
      // on our own Insets, since that is how much the wrapped painter will
      // be inset when it is filled.
      //
      if (wrappedInsets.equals(ImmInsets.getEmptyInsets()))
      {
        return getOwnFillInsets(context);
      }
      else
      {
        //
        // add our insets onto the returned fill insets
        //
        ImmInsets ownInsets = getOwnInsets(context);

        return new ImmInsets(ownInsets.top    + wrappedInsets.top,
                             ownInsets.left   + wrappedInsets.left,
                             ownInsets.bottom + wrappedInsets.bottom,
                             ownInsets.right  + wrappedInsets.right);
      }
    }
    else
    {
      // no wrapped painter, so our insets it is
      return getOwnFillInsets(context);
    }
  }


  /**
   * Returns the insets of just this BorderPainter, as opposed to the
   * entire nested stack of BorderPainters like <i>getInsets</i> does.
   * <p>
   * @param context PaintContext to use when getting our own Insets
   * <p>
   * @return The insets of just this BorderPainter.
   * <p>
   * @see #getInsets
   */
  protected ImmInsets getOwnInsets(
    PaintContext context
    )
  {
    // no insets at all
    return ImmInsets.getEmptyInsets();
  }


  /**
   * Returns the amount of space by which fills should be inset for
   * this BorderPainter.  By default, returns this BorderPainter's insets.
   * <p>
   * @param context the context for painting
   * <p>
   * @see #getFillInsets
   */
  protected ImmInsets getOwnFillInsets(
    PaintContext context
    )
  {
    return getOwnInsets(context);
  }


  /**
   * Paint just the border of the BorderPainter.  Subclasses should
   * override this method rather than <i>paint</i> when modifying
   * the manner in which the border is painted
   * <p>
   * Painters that modify the Graphics context are responsible for
   * returning it to its initial state when done painting.  Further
   * Painters that paint outside the bounds of <b>x</b>, <b>y</b>,
   * <b>width</b>, and <b>height</b> or inside the area inset by
   * their insets are responsible for clipping themselves.
   * <p>
   * @param context PaintContext to use when painting the Border
   * @param g       Graphics context to use when painting
   * @param x       x coordinate to begin painting at
   * @param y       y coordinate to begin painting at
   * @param width   width of area to paint
   * @param height  height of area to paint
   * <p>
   * @return The insets of just this BorderPainter.
   * <p>
   * @see #paint
   */
  protected void paintBorder(
    PaintContext context,
    Graphics     g,
    int          x,
    int          y,
    int          width,
    int          height
    )
  {
    // do nothing.  Subclasses will override
  }



  /**
   * Add the Border's insets as necessary to the specified inner size.
   */
  private Dimension _addInsets(
    PaintContext context,
    Dimension    innerSize
    )
  {
    // don't add any size onto a zero width inner size
    if (_alwaysAddInsets ||
        ((innerSize.width > 0) && (innerSize.height > 0)))
    {
      // add our own size, as opposed to the size of all of the
      // nested insets, as these sizes have already been accounted
      // for when calculating the size of the wrapped painter
      ImmInsets borderSizes = getOwnInsets(context);

      innerSize.width  += borderSizes.left + borderSizes.right;
      innerSize.height += borderSizes.top  + borderSizes.bottom;
    }

    return innerSize;
  }


  /**
   * Initialize the state of the AbstractBorderPainter
   */
  private void _init(
    Painter wrappedPainter,
    boolean alwaysAddInsets
    )
  {
    // save the wrappedPainter if it is a BorderPainter so that
    // we can calculate the nested insets in getInsets()
    if (wrappedPainter instanceof BorderPainter)
    {
      _wrappedBorderPainter = (BorderPainter)wrappedPainter;
    }

    _alwaysAddInsets = alwaysAddInsets;
  }

  // BorderPainter that this Border is wrapping.  null if the
  // wrappedComponent is not a BorderPainter
  private BorderPainter _wrappedBorderPainter;

  // true if we always add our insets on ins getSize(), regardless
  // of the size of the wrapped painter
  private boolean _alwaysAddInsets;
}
