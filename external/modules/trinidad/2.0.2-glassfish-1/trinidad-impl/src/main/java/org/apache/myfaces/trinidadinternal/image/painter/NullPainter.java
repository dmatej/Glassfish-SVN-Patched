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
 * A completely transparent painter object that has no size.
 * This is useful as a default value and as a transparent painter.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/NullPainter.java#0 $) $Date: 10-nov-2005.19:04:59 $
 */
public class NullPainter extends AbstractPainter implements BorderPainter
{
  /**
   * Creates a new Nullpainter instance.  NullPainter is not
   * a singleton, although there is a shared instance which
   * most programmers will use.  Being able to create new
   * instances allows programmers to differentiate Nullpainter
   * instances.
   * <p>
   * @see #getPainter
   */
  public NullPainter()
  {
  }


  /**
   * Returns the shared instance of the NullPainter class.  This is
   * the instance that most users should use.
   * <p>
   * @return The shared instance of the NullPainter class.
   */
  static public BorderPainter getPainter()
  {
    if (_sPainter == null)
    {
      _sPainter = new NullPainter();
    }

    return _sPainter;
  }


  /**
   * Returns the minimum size of the NullPainter.
   * <p>
   * @param context Context for determining the minimum size.
   * @return The minimum size of the Painter.
   */
  public Dimension getMinimumSize(
    PaintContext context
    )
  {
    return new Dimension();
  }



  /**
   * Paints the NullPainter at the given location.
   * <p>
   * @param context Context for painting.
   * @param g       Graphics object to draw into.
   * @param x       X position to draw at.
   * @param y       Y position to draw at.
   * @param width   Width to draw into.
   * @param height  Height to draw into.
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
    // do nothing
  }



  /**
   * Returns the amount of space the border will
   * require on each side.
   * <p>
   * @param context the context for painting
   */
  public ImmInsets getInsets(
    PaintContext context
    )
  {
    return ImmInsets.getEmptyInsets();
  }


  /**
   * Returns the amount of space by which fills should
   * be inset.
   * <p>
   * @param context the context for painting
   */
  public ImmInsets getFillInsets(
    PaintContext context
    )
  {
    return ImmInsets.getEmptyInsets();
  }


  // shared instance
  private static NullPainter _sPainter;
}

