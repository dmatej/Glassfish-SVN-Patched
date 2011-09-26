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

/**
 * Abstract base class for most of the painters.  This class takes care
 * of appropriately stubbing out the methods of the Painter interface,
 * making writing Painters easier.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/AbstractPainter.java#0 $) $Date: 10-nov-2005.19:04:52 $
 */
public abstract class AbstractPainter implements Painter
{
  /**
   * Create an AbstractPainter.
   */
  public AbstractPainter()
  {
  }


  /**
   * Returns the preferred size of the painter.
   * <p>
   * @param context Context for determining the preferred size.
   * <p>
   * @return The preferred size of the Painter.
   */
  public Dimension getPreferredSize(
    PaintContext context
    )
  {
    return getMinimumSize(context);
  }


  protected Object getData(
    PaintContext context
    )
  {
    return context.getPaintData(getDataKey());
  }


  protected Object getDataKey()
  {
    return null;
  }
}

