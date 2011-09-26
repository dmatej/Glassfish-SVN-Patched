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
package org.apache.myfaces.trinidadinternal.image.util;

import java.awt.Polygon;
import java.awt.Rectangle;

import org.apache.myfaces.trinidad.util.IntegerUtils;

/**
 * MapArea describes an area of an image map.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/util/MapArea.java#0 $) $Date: 10-nov-2005.19:04:49 $
 */
public class MapArea
{
  /**
   * Shape for rectangles
   */
  public static final String RECTANGLE_SHAPE = "rect";

  /**
   * Shape for polygons
   */
  public static final String POLYGON_SHAPE  = "poly";

  /**
   * Crates a MapArea.
   * @param shape The shape of the area.  This is one of the *_SHAPE constants
   *   defined by MapArea.
   * @param coordinates The coordinates of the area.
   */
  public MapArea(String shape, int[] coordinates)
  {
    _init(shape, coordinates);
  }

  /**
   * Creates a MapArea.
   * @param shape The shape of the area.  This is one of the *_SHAPE constants
   *   defined by MapArea.
   * @param coordinates The String representing the coordinates of the area.
   */
  public MapArea(String shape, String coordinatesString)
  {
    _shape = shape;
    _coords = coordinatesString;
  }

  /**
   * Creates a rectangle MapArea.
   */
  public MapArea(Rectangle rectangle)
  {
    int[] coords = null;

    if (rectangle != null)
    {
      coords = new int[4];
      coords[0] = rectangle.x;
      coords[1] = rectangle.y;
      coords[2] = rectangle.x + rectangle.width;
      coords[3] = rectangle.y + rectangle.height;
    }

    _init(RECTANGLE_SHAPE, coords);
  }

  /**
   * Creates a polygon MapArea
   */
  public MapArea(Polygon polygon)
  {
    int[] coords = null;

    if ((polygon != null) && (polygon.npoints != 0))
    {
      coords = new int[polygon.npoints * 2];
      for (int i = 0; i < polygon.npoints; i++)
      {
        coords[i*2] = polygon.xpoints[i];
        coords[i*2+1] = polygon.ypoints[i];
      }
    }

    _init(POLYGON_SHAPE, coords);
  }

  /**
   * Returns a String indicating the shape of the MapArea,
   * suitable for use in an HTML image map.
   */
  public String getShape()
  {
    return _shape;
  }

  /**
   * Returns a String representing the area's coordinates, suitable
   * for inclusion in an HTML image map.
   */
  public String getCoordinatesString()
  {
    return _coords;
  }

  // Initializes the MapArea
  private void _init(String shape, int[] coordinates)
  {
    _shape = shape;

    if (coordinates == null)
      return;

    StringBuffer buffer = new StringBuffer(coordinates.length*4);
    for (int i = 0; i < coordinates.length; i++)
    {
      buffer.append(IntegerUtils.getString(coordinates[i]));

      if (i < (coordinates.length - 1))
        buffer.append(_SEPARATOR);
    }

    _coords = buffer.toString();
  }

  private String _shape;
  private String _coords;

  private static final char _SEPARATOR = ',';
}
