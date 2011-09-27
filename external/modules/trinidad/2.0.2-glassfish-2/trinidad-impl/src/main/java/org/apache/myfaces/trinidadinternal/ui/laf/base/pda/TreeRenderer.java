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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import java.io.IOException;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * Renderer for trees.  A TreeDataProxy may be set on the tree to create
 * specify different values of node properties at render time.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/oracle/desktop/TreeRenderer.java#0 $) $Date: 10-nov-2005.18:52:33 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class TreeRenderer extends org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.TreeRenderer
{
  @Override
  protected String getConnectingBackgroundIcon(
   boolean isLine, 
   boolean leftToRight
  )
  {
    String backgroundIcon = null;

    if(isLine)
    {        
      if ( leftToRight )
        backgroundIcon = _TLINE_GIF;
      else
        backgroundIcon = _TLINEF_GIF;
    }
    return backgroundIcon;
  }
  
  @Override
  protected String getIconBackgroundIcon(
    int     expand,
    boolean isLeftToRight
  )
  {
    String backgroundIcon = null ;

    if ((expand == EXPAND_OPEN) || (expand == EXPAND_ALWAYS))
    {
      if (isLeftToRight )
        backgroundIcon = _TLINER_GIF;
      else
        backgroundIcon = _TLINERF_GIF;      
    }

    return backgroundIcon;
  }
  
  @Override
  protected String getDefaultIconName()
  {
    return _DEFAULT_ICON_NAME;
  }

  
  // render the correct icon for a specific node
  @Override
  protected void renderExpandCell(
    UIXRenderingContext context,
    UIXHierarchy tree,
    boolean isLeftToRight,
    boolean isRoot,
    boolean isLastSibling,
    int expanded,
    String expandOnclick
    ) throws IOException
  {
    // first find out row
    int index = (isRoot)
                  ? _NODE_DECORATION_TOP_START
                  : 0;


    if (isLastSibling)
    {
      index = index + _NODE_DECORATION_BOTTOM_INCREMENT;
    }
    
    String backgroundIcon = null;
    
    if ( isLeftToRight )
      backgroundIcon = _NODE_BACKGROUND[index];
    else
      backgroundIcon =
           _NODE_BACKGROUND[index + _NODE_BACKGROUND_RTL_INCREMENT];


    index = index % _NODE_DECORATION_ROWS;

    // multiply by row length
    index *= _NODE_DECORATION_ROW_SIZE;

    Object altText = null;

    // add in the expandability
    switch (expanded)
    {
      case NO_CHILDREN:
        index += _NODE_DECORATION_NO_CHILDREN_INCREMENT;
        break;
      case EXPAND_CLOSED:
        index += _NODE_DECORATION_CLOSED_INCREMENT;
        altText = getTranslatedValue(context, _EXPAND_TIP_KEY);        
        break;
      case EXPAND_OPEN:
        index += _NODE_DECORATION_OPEN_INCREMENT;
        altText = getTranslatedValue(context, _COLLAPSE_TIP_KEY);
        break;
      case EXPAND_ALWAYS:
        index += _NODE_DECORATION_OPEN_INCREMENT;
        altText = getTranslatedValue(context, _DISABLED_COLLAPSE_TIP_KEY);
        break;
    }

    // finally, left to right.
    if (!isLeftToRight)
      index += _NODE_DECORATION_RTL_INCREMENT;
      
    renderIconCell(context,
                    tree,
                    backgroundIcon,
                    _NODE_DECORATIONS[index],
                    false,
                    altText,
                    _ICON_WIDTH,
                    _ICON_HEIGHT,
                    expandOnclick);

  }

  


  private static final String _ICON_WIDTH  = "16";
  private static final String _ICON_HEIGHT = "22";


  // rendered for straight line connections - ltr
  private static final String _TLINE_GIF = "tline.gif";
  // rendered for straight line connections - rtl
  private static final String _TLINEF_GIF = "tlinef.gif";

  // rendered behind icon (eg folder icon) so it looks connected - ltr
  private static final String _TLINER_GIF = "tliner.gif";
  // rendered behind icon (eg folder icon) so it looks connected - rtl
  private static final String _TLINERF_GIF = "tlinerf.gif";


  private static final String _DEFAULT_ICON_NAME = "tfold.gif";

  // calculation table for decorations
  private static final String[] _NODE_DECORATIONS =
  {
//                          Left-to-right                               Right-to-left
//             no_children   closed       open            no_children    closed         open

/* alone  */   "tleaf.gif",  "tplusa.gif", "tminusa.gif", "tleaff.gif",  "tplusaf.gif", "tminusaf.gif",
/* top    */   "tleaf.gif",  "tplusr.gif", "tminusr.gif", "tleaff.gif",  "tplusrf.gif", "tminusrf.gif",
/* bottom */   "tleafb.gif", "tplusb.gif", "tminusb.gif", "tleafbf.gif", "tplusbf.gif", "tminusbf.gif",

  };


  private static final String[] _NODE_BACKGROUND =
  {
             /* middle        top           bottom      alone  */
  /* ltr */  "tline.gif",  "tliner.gif",  "tlineb.gif",  null,
  /* rtl */  "tlinef.gif", "tlinerf.gif", "tlinebf.gif", null,
  };


  private static final int _NODE_BACKGROUND_RTL_INCREMENT         = 4;

  private static final int _NODE_DECORATION_ROWS                  = 3;
  private static final int _NODE_DECORATION_ROW_SIZE              = 6;
  private static final int _NODE_DECORATION_TOP_START             = 1;
  private static final int _NODE_DECORATION_BOTTOM_INCREMENT      = 2;
  private static final int _NODE_DECORATION_NO_CHILDREN_INCREMENT = 0;
  private static final int _NODE_DECORATION_CLOSED_INCREMENT      = 1;
  private static final int _NODE_DECORATION_OPEN_INCREMENT        = 2;
  private static final int _NODE_DECORATION_RTL_INCREMENT         = 3;
  
  // translation keys
  private static final String _DISABLED_COLLAPSE_TIP_KEY = 
    "af_tree.DISABLED_COLLAPSE_TIP";
  private static final String _COLLAPSE_TIP_KEY =
    "af_tree.COLLAPSE_TIP";
  private static final String _EXPAND_TIP_KEY = 
    "af_tree.EXPAND_TIP";

}
