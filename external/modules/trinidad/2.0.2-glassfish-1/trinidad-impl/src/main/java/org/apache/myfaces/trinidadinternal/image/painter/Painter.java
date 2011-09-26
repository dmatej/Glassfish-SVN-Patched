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
 * Interface implemented by objects that paint something.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/Painter.java#0 $) $Date: 10-nov-2005.19:05:02 $
 */
public interface Painter
{
  /**
   * Returns the preferred size of the painter.
   * <p>
   * @param context Context for determining the preferred size.
   * <p>
   * @return The preferred size of the Painter.
   */
  public Dimension getPreferredSize(PaintContext context);


  /**
   * Returns the minimum size of the painter.
   * <p>
   * @param context Context for determining the minimum size.
   * <p>
   * @return The minimum size of the Painter.
   */
  public Dimension getMinimumSize(PaintContext context);



  /**
   * Paints the Painter at the given location.
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
    int          height);
}
