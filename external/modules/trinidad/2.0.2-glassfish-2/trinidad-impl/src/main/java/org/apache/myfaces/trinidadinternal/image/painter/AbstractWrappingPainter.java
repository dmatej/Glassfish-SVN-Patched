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
 * Abstract baseclass for Painters that wrap other painters, modifying
 * the attributes of the wrapped painter.  This class delegates all
 * Painter methods through to the wrapped Painter.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/AbstractWrappingPainter.java#0 $) $Date: 10-nov-2005.19:04:52 $
 */
public abstract class AbstractWrappingPainter extends  AbstractPainter
{
  /**
   * Create an AbstractWrappingPainter wrapping a NullPainter.
   * <p>
   * @see NullPainter#getPainter
   */
  public AbstractWrappingPainter()
  {
    this(null);
  }


  /**
   * Create an AbstractWrappingPainter wrapping another Painter.
   * <p>
   * @param wrappedPainter The Painter to wrap.
   */
  public AbstractWrappingPainter(
    Painter wrappedPainter
    )
  {
    // make sure that we always have valid wrappedPainter
    if (wrappedPainter == null)
    {
      wrappedPainter = NullPainter.getPainter();
    }

    _wrappedPainter = wrappedPainter;
  }




  /**
   * Returns the preferred size of the wrapped painter.
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
    return _getWrappedPainter(context).getPreferredSize(context);
  }


  /**
   * Returns the minimum size of the wrapped painter.
   * <p>
   * @param context Context for determining the minimum size.
   * @return The minimum size of the Painter.
   */
  public Dimension getMinimumSize(
    PaintContext context
    )
  {
    return _getWrappedPainter(context).getMinimumSize(context);
  }


  /**
   * Paints the wrapped Painter.
   * <p>
   * @param context Context for painting.
   * @param g Graphics object to draw into.
   * @param x X position to draw at.
   * @param y Y position to draw at.
   * @param width Width to draw into.
   * @param height Height to draw into.
   */
  public void paint(
    PaintContext context,
    Graphics     g,
    int          x,
    int          y,
    int          width,
    int          height
    )
  {
    _getWrappedPainter(context).paint(context,
                                      g,
                                      x,
                                      y,
                                      width,
                                      height);
  }



  /**
   * Returns the wrapped painter.  By default, just returns
   * the painter passed to the constructor, but subclasses
   * can override this method to dynamically adjust the wrapped
   * painter depending on the PaintContext.
   *
   * @param context Context to use for determining the wrapped painter.
   */
  protected Painter getWrappedPainter(
    PaintContext context
    )
  {
    return _wrappedPainter;
  }


  // Helper method that handles "null"
  private Painter _getWrappedPainter(PaintContext context)
  {
    Painter painter = getWrappedPainter(context);
    if (painter == null)
      painter = NullPainter.getPainter();

    return painter;
  }


  // the Painter that the WrappingPainter is wrapping
  private Painter _wrappedPainter;
}
