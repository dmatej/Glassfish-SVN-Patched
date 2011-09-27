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

import java.io.IOException;



import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PanelHeaderRenderer;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/HeaderRenderer.java#0 $) $Date: 10-nov-2005.18:53:55 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class HeaderRenderer extends XhtmlLafRenderer
{

  /**
  * Sets the size of the header,
  */
  protected static void setPrevHeaderSize(
    UIXRenderingContext context
    ) throws IOException
  {

    context.setLocalProperty( _PREV_HEADER_SIZE,
                              getContextHeaderSize( context, null ));

  }
  protected static Number getContextHeaderSize(
    UIXRenderingContext context,
    Number           defaultValue
    )throws IOException
  {
    return (Number)getRenderingProperty( context,
                                 _HEADER_SIZE,
                                 defaultValue);
  }

   /**
   * Set the size of the header stored on the context
   */
  protected static void setContextHeaderSize(
    UIXRenderingContext context,
    Number           size
    )throws IOException
  {
    setRenderingProperty( context, _HEADER_SIZE, size);
  }





  /**
  * resets header size to value saved as local property
  */
  protected static void resetHeaderSize(
    UIXRenderingContext context
    )
  {
    setRenderingProperty( context,
                          _HEADER_SIZE,
                          context.getLocalProperty(0,
                                                   _PREV_HEADER_SIZE,
                                                   ZERO));
  }



   /**
   * Returns the current depth of the nesting.
   */
  protected static int getHeaderNestLevel(
    UIXRenderingContext context
    )
  {
    return ((Number)getRenderingProperty(context,
                                          _HEADER_NEST_LEVEL,
                                          ZERO)).intValue();
  }


  protected static void incrementHeaderNestLevel(
    UIXRenderingContext context
    )throws IOException
  {
    setRenderingProperty( context,
                          _HEADER_NEST_LEVEL,
                          getInteger( getHeaderNestLevel(context) + 1) );
  }

  protected static void decrementHeaderNestLevel(
    UIXRenderingContext context
    )throws IOException
  {
    int headerNestLevel = getHeaderNestLevel(context);

    assert (headerNestLevel > 0):"cannot decrement header nest level";


    setRenderingProperty( context,
                          _HEADER_NEST_LEVEL,
                          getInteger( headerNestLevel - 1) );
  }


  /**
  * Returns text of header
  */
  protected Object getText(
    UIXRenderingContext context,
    UINode           node,
    String           messageType
    )
  {
    Object label = node.getAttributeValue(context, TEXT_ATTR);

    if (label != null)
      return label;

    if ( messageType == null )
      return null;

    String key = null;

    if (MESSAGE_TYPE_ERROR.equals( messageType ))
      key = _ERROR_KEY;
    else if (MESSAGE_TYPE_WARNING.equals(messageType))
      key = _WARNING_KEY;
    else if (MESSAGE_TYPE_INFO.equals(messageType))
      key = _INFORMATION_KEY;
    else if (MESSAGE_TYPE_CONFIRMATION.equals(messageType))
      key = _CONFIRMATION_KEY;
    else if (MESSAGE_TYPE_PROCESSING.equals(messageType))
      key = _PROCESSING_KEY;
    else
      return null;

    return getTranslatedValue(context, key);

  }

  /**
  * Returns the uri for icon.
  */
  protected Object getIconURI(
    UIXRenderingContext context,
    UINode           node,
    String           messageType
    )
  {
    return getFlippableURI(context, node, ICON_ATTR);
  }


  // key for retrieving nesting level from the RenderingContext
  private static final Object _HEADER_NEST_LEVEL = PanelHeaderRenderer.HEADER_NEST_LEVEL;

  // key for retrieving size from the RenderingContext
  private static final Object _HEADER_SIZE = PanelHeaderRenderer.HEADER_SIZE;

  private final static Object _PREV_HEADER_SIZE = new Object();


  // text keys
  static private final String _INFORMATION_KEY  = "af_panelHeader.INFORMATION";
  static private final String _WARNING_KEY      = "af_panelHeader.WARNING";
  static private final String _ERROR_KEY        = "af_panelHeader.ERROR";
  static private final String _CONFIRMATION_KEY = "af_panelHeader.CONFIRMATION";
  static private final String _PROCESSING_KEY   = "af_panelHeader.PROCESSING";


}
