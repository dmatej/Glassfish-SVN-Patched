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
package org.apache.myfaces.trinidadinternal.image.xml.parse;

import java.util.Vector;

import org.xml.sax.Attributes;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.image.util.MapArea;
import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;

/**
 * NodeParser for imageMap elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/ImageMapParser.java#0 $) $Date: 10-nov-2005.19:04:06 $
 */
public class ImageMapParser extends BaseNodeParser
{
  /**
   * Implementation of NodeParser.endElement()
   */
  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    )
  {
    MapArea[] areas = new MapArea[_areas.size()];
    _areas.copyInto(areas);
    return areas;
  }

  /**
   * Implementation of NodeParser.startChildElement()
   */
  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    )
  {
    if (XMLConstants.IMAGE_MAP_AREA_NAME.equals(localName))
    {
      String shape = getRequiredAttribute(context, 
                                          attrs,
                                          XMLConstants.SHAPE_ATTR);
      String coords = getRequiredAttribute(context,
                                           attrs,
                                           XMLConstants.COORDINATES_ATTR);

      if ((shape != null) && (coords != null))
        _areas.addElement(new MapArea(shape, coords));

      return BaseNodeParser.getIgnoreParser();
    }
    else
    {
      return null;
    }
  }

  // -= Simon Lessard =-
  // FIXME: Wow! Another Vector... Change that to ArrayList
  //        or Collections.synchronizedList(ArrayList)
  private Vector<MapArea> _areas = new Vector<MapArea>();
}
