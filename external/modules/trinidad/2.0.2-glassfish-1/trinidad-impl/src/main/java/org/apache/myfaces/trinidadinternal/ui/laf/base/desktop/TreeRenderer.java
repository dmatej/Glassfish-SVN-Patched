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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.io.IOException;

import java.util.HashMap;
import java.util.Map;

import javax.faces.component.NamingContainer;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TreeUtils;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.DataBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.DefaultingBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.IfBoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.SkinTranslatedBoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.FormValueRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.trinidadinternal.uinode.UIComponentUINode;

/**
 * Renderer for trees.  A TreeDataProxy may be set on the tree to create
 * specify different values of node properties at render time.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/TreeRenderer.java#0 $) $Date: 10-nov-2005.18:56:22 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class TreeRenderer extends HtmlLafRenderer
{
  @Override
  protected void renderContent(
    UIXRenderingContext context, 
    UINode node) throws IOException
  {
    final String name = 
      (String) node.getAttributeValue(context, UIConstants.ID_ATTR);
    assert name != null;
    
    UIXHierarchy tree = getTree(context, node);
    Object oldPath = tree.getRowKey();
    try
    {
      boolean continueRendering = setInitialPath(context, node, tree);
      if (!continueRendering)
        return;
  
      _renderContent(context, node, tree, name);
    }
    finally
    {
      tree.setRowKey(oldPath);
    }
  }

  private void _renderContent(
    UIXRenderingContext context,
    UINode           node,
    UIXHierarchy     tree,
    String name
    ) throws IOException
  {
    
    UINode stamp = getStamp(context, node);
    if(stamp == null)
      stamp = _DEFAULT_STAMP;  

//@todo - will this tree.getFocusPath survive?
//    List focusPath = getFocusPath(context, node);
    Object focusPath = tree.getFocusRowKey();
    String formName = getParentFormName(context);
    if (formName == null)
    {
      _LOG.warning("TREE_COMPONENT_MUST_INSIDE_FORM");
      return;
    }


    // Bug 3931544:  don't use colons in Javascript variable names.
    // We'll just replace colons with underscores;  not perfect, but adequate
    final String varName = "_adftree" +
            XhtmlUtils.getJSIdentifier(name);

    boolean leftToRight = !context.getLocaleContext().isRightToLeft();
    int rootSize = tree.getRowCount();
    RowKeySet state = getExpandedRowKeys(tree);      
    Map<Object, Boolean> selectedPaths = getSelectedPaths(focusPath);
    UINode icon = getIcon();

    // render each of the root nodes
    for (int i = 0; i < rootSize; i++)
    {
      tree.setRowIndex(i);
      _renderNode(context,
                  tree,
                  icon,
                  stamp,
                  varName,
                  state,
                  selectedPaths,
                  new Boolean[_DEFAULT_TREE_DEPTH],
                  leftToRight,
                  (i == 0),
                  (i == rootSize - 1),
                  0);
    }

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(SCRIPT_ELEMENT, null);
    renderScriptDeferAttribute(context);

    // Bug #3426092:
    // render the type="text/javascript" attribute in accessibility mode
    XhtmlLafRenderer.renderScriptTypeAttribute(context);

    _renderTreeJS(context, node);

    //out.writeText("_setNodes('"+name+"','"+nodesRendered+"');");

    String selectedParam =
      name + NamingContainer.SEPARATOR_CHAR +
      org.apache.myfaces.trinidadinternal.renderkit.uix.TreeRenderer.SELECTED_PARAM;

    writer.writeText("var "+varName+" = "+
                     _createNewJSSelectionState(formName, name, selectedParam), null);
    writer.endElement(SCRIPT_ELEMENT);

    FormValueRenderer.addNeededValue(context, formName, selectedParam);
    FormValueRenderer.addNeededValue(context, formName, PARTIAL_TARGETS_PARAM);
    FormValueRenderer.addNeededValue(context, formName, PARTIAL_PARAM);
    FormValueRenderer.addNeededValue(context, formName, _PATH_PARAM);
    
    
  }
  
  protected UIXHierarchy getTree(
    UIXRenderingContext context, 
    UINode node)
  {
    return (UIXHierarchy) node.getUIComponent();    
  }
  
  protected UINode getStamp(
    UIXRenderingContext context, 
    UINode node)
  {
    return getNamedChild(context, node, NODE_STAMP_CHILD);
  }    
  
  // return whether to continue with rendering
  protected boolean setInitialPath(
    UIXRenderingContext context, 
    UINode           node,
    UIXHierarchy     tree)
  {
    tree.setRowKey(null);
    return true;
  }  
  
 
  private boolean _isShownSelected(
    UIXHierarchy         tree,
    Map<Object, Boolean> selectedPaths,
    Object               currPath
  )
  {

    boolean selected = false;
    if (tree instanceof UIXTree)
      selected = ((UIXTree)tree).getSelectedRowKeys().isContained();
      
    if (selected)
      return true;
      
    Object value = selectedPaths.get(currPath);
    
    if (value != null)
      return true;
      
    return false;
  }

  protected Map<Object, Boolean> getSelectedPaths(Object focusPath)
  {
    if ( focusPath == null)
      return new HashMap<Object, Boolean>(0);
      
    Map<Object, Boolean> selectedPaths = 
      new HashMap<Object, Boolean>(1);
    
    selectedPaths.put(focusPath, Boolean.TRUE);
    return selectedPaths;
  }

  protected RowKeySet getExpandedRowKeys(UIXHierarchy tree)
  {
    return ((UIXTree)tree).getDisclosedRowKeys();    
  }
  
  protected String getConnectingBackgroundIcon(
   boolean isLine,
   boolean leftToRight
  )
  {
    return null;
  }

  protected String getIconBackgroundIcon(
    int     expand,
    boolean isLeftToRight
  )
  {
    return null;
  }


  // render the correct icon for a specific node
  protected void renderExpandCell(
    UIXRenderingContext context,
    UIXHierarchy tree,
    boolean isLeftToRight,
    boolean isRoot,
    boolean isLastSibling,
    int expanded,
    String onclick
    ) throws IOException
  {


    Object altText = null;

    String text = null;

    // add in the expandability
    switch (expanded)
    {
      case NO_CHILDREN:
        break;
      case EXPAND_CLOSED:
        // "\u21D2"; // Double Arrow right

        if ( context.getAgent().getAgentOS() == TrinidadAgent.OS_MACOS )
          text = isRightToLeft(context)? "\u2190"// single arrow left
                                       : "\u2192";// single arrow right
        else
          text = isRightToLeft(context)? "\u25C4" // triangle left
                                        : "\u25BA"; // triangle right
        altText = getTranslatedValue(context, mapKey(_EXPAND_TIP_KEY));
        break;
      case EXPAND_OPEN:
        //"\u21D3"; // double arrow down
        if ( context.getAgent().getAgentOS() == TrinidadAgent.OS_MACOS )
          text = "\u2193"; // single arrow down
        else
          text="\u25BC"; // triangle down
        altText = getTranslatedValue(context, mapKey(_COLLAPSE_TIP_KEY));
        break;
      case EXPAND_ALWAYS:
        if ( context.getAgent().getAgentOS() == TrinidadAgent.OS_MACOS )
          text = "\u2193"; // single arrow down
        else
          text="\u25BC"; // triangle down
        altText = getTranslatedValue(context, 
                                     mapKey(_DISABLED_COLLAPSE_TIP_KEY));
        break;
    }

    _renderTextCell(context,
                    text,
                    altText,
                    _ICON_WIDTH,
                    onclick,
                    TREE_DISCLOSED_SYMBOL_STYLE_CLASS);

  }


  private void _renderTextCell(
    UIXRenderingContext context,
    String           text,
    Object           altText,
    String           width,
    String           onclick,
    Object           styleClass
  ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(WIDTH_ATTRIBUTE, width, null);
    writer.writeAttribute("title", altText, null);
    renderStyleClassAttribute(context, styleClass);

    if (onclick != null)
    {
      writer.startElement(LINK_ELEMENT, null);
      writer.writeAttribute(HREF_ATTRIBUTE, "#", null);
      writer.writeAttribute(ONCLICK_ATTRIBUTE, onclick, null);
    }

    if (text != null)
      writer.writeText(text, null);

    if (onclick != null)
    {
      writer.endElement(LINK_ELEMENT);
    }

    writer.endElement(TABLE_DATA_ELEMENT);
  }


  protected void renderIconCell(
    UIXRenderingContext context,
    UIXHierarchy         tree,
    String           backgroundIcon,
    String           icon,
    boolean          isIconAbsoluteURI,
    Object           altText,
    String           width,
    String           height,
    String           onclick
  ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(WIDTH_ATTRIBUTE, width, null);

    if(backgroundIcon != null)
    {
      String backgroundIconURI = getAbsoluteImageURI( context,
                                                      backgroundIcon);
      writer.writeAttribute(STYLE_ATTRIBUTE,
                            _BACKGROUND_IMAGE_URL + backgroundIconURI +_END_FUNC, null);
    }

    if (onclick != null)
    {
      writer.startElement(LINK_ELEMENT, null);
      writer.writeAttribute(HREF_ATTRIBUTE, "#", null);
      writer.writeAttribute(ONCLICK_ATTRIBUTE, onclick, null);
      String treeName = tree.getClientId(context.getFacesContext());
      String id = treeName + NamingContainer.SEPARATOR_CHAR + "lnk"; 
      writer.writeAttribute(ID_ATTRIBUTE, id, null);
    }

    _renderIcon(context,
                icon,
                isIconAbsoluteURI,
                altText,
                width,
                height);

    if (onclick != null)
    {
      writer.endElement(LINK_ELEMENT);
    }

    writer.endElement(TABLE_DATA_ELEMENT);
  }


  private static String _createNewJSSelectionState(
    String formName,
    String treeClientId,
    String selectParam)
  {
    return "new _adfTreeSelector('"+selectParam+"',"+
      TreeUtils.createNewJSCollectionComponentState(
        formName,
        treeClientId)+");";
  }

  private static String _callJSSelect(UIXHierarchy tree, String jsVarName)
  {
    String currencyStr = tree.getClientRowKey();
    return jsVarName+".select(this,'"+currencyStr+"');";
  }

  private void _renderTreeJS(UIXRenderingContext context, UINode node)
    throws IOException
  {
    if (!isPreviouslyRendered(context, _JS_RENDERED_KEY))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.writeText
        ("function _adfTreeSelector(selectParam,tState) {"+
           "this._selectParam = selectParam;"+
           "this._pTag = null;"+
           "this.treeState = tState;"+
         "}"+
         "_adfTreeSelector.prototype.select = function(tag,path) {"+
           "if (this._pTag != null) {"+
             "this._pTag.className='"+TREE_ROW_STYLE_CLASS+
             "';"+
           "}"+
           "this._pTag = tag;"+
           //if there are any problems see TRINIDAD-935
           "tag.className='"+TREE_ROW_SELECTED_STYLE_CLASS+
           "';"+
         "};"
        ,null);


     // _setSelection(..) and _getSelection(..) are called by the
      // ClientStateTreeDataProxy (if selection is enabled for this particular
      // tree). _setSelection is called to set the initial selection (as
      // determined by the proxy). _getSelection is called to get at the
      // current selection state (to send back to the server by the proxy)

      // @param source the name or ID of the tree
      // @param sel something that identifies the current selected node
      // _setSelection(source,sel)

      // @param source the name or ID of the tree
      // @return something that identifies the current selected node
      // _getSelection(source)

//      writer.writeText
//        ("var _treeSel = new Object();"  +
//         "var _treeNodes = new Object();"  +
//         "function _setSelection(source,sel) {"
//         + "_treeSel[source]=sel;"  +
//         "}"  +
//         "function _getSelection(source) {"
//         + "return _treeSel[source];"  +
//         "}"  +
//
//         // _setNodes is used to indicate the number of tree nodes that are
//         // currently visible. This is so that _clearSelection knows exactly
//         // how many elements to clear
//         "function _setNodes(source,nodes) {"
//         + "_treeNodes[source]=nodes;"  +
//         "}"  +
//         "function _getNodes(source) {"
//         + "return _treeNodes[source];"  +
//         "}",
//         null  );
//
//      writer.writeText
//        ("function _select(name,index,nodeID) {"
//         + "_clearSelection(name);"
//         + "var e =_getElementById(document,name+index);"
//         + "e.className = '",
//         null  );
//      writer.writeText(TREE_ROW_SELECTED_STYLE_CLASS+"';", null);
//      writer.writeText
//        ("  _setSelection(name,nodeID);return true;"  +
//         "}",
//         null  );
//
//      writer.writeText
//        ("function _clearSelection(name) {"
//         + "var sz = _getNodes(name);"
//         + "for (var i = 0; i < sz; i++) {"
//         +   "var e =_getElementById(document,name+i);"
//         +   "e.className='",
//         null);
//      writer.writeText(TREE_ROW_STYLE_CLASS+"';", null);
//      writer.writeText
//        ("  }"  +
//         "}",
//         null);

      Object immediate = NodeUtils.getUIComponent(context, node).getAttributes().get("immediate");
      boolean validate = !Boolean.TRUE.equals(immediate);
      String buff = TreeUtils.setupJSTreeCollectionComponent(validate)+";";
      writer.writeText(buff, null);
    }
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return DIV_ELEMENT;
  }


  // render one row of the tree
  private void _renderNode(
    UIXRenderingContext context,
    UIXHierarchy tree,
    UINode icon,
    UINode stamp,
    final String varName,
    RowKeySet state,
    Map<Object, Boolean> selectedPaths,
    Boolean[] prepend,
    boolean leftToRight,
    boolean isFirstSibling,
    boolean isLastSibling,
    int nodeDepth
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // each row is a table
    writer.startElement(TABLE_ELEMENT, null);
    renderLayoutTableAttributes(context, ZERO, ZERO, ZERO, null);
    writer.startElement(TABLE_ROW_ELEMENT, null);



    // render the prepend
    _prependIcons(context, tree, prepend, leftToRight);


    String onclickExpand = null;
    int expand = _getExpandValue(tree, state);

    if ((expand != NO_CHILDREN) &&
        supportsNavigation(context))
    {
      onclickExpand = TreeUtils.callJSExpandNode(tree, varName+".treeState",
                                                 (expand == EXPAND_CLOSED));
    }

    renderExpandCell(context,
                     tree,
                     leftToRight,
                     isFirstSibling,
                     isLastSibling,
                     expand,
                     onclickExpand);



//    DataObject curData = BeanAdapterUtils.getAdapter(context, tree.getRowData());
    String treeStyle = TREE_ROW_STYLE_CLASS;


    // location was a colon separated list of IDs
    //boolean selected = proxy.isSelected(context, node, location);
    Object currPath = tree.getRowKey();
    boolean selected = _isShownSelected(tree, selectedPaths, currPath); 

    String onClick = _callJSSelect(tree, varName);

//    if ( proxy.selectionEnabled(context) )
//    {
//      // selection with the proxy doesn't work on netscape
//      // filed as bug 1817185 - so far we have not figured
//      // out a way without using layers and we are seeing nodes
//      // jump around with layers so disabling selection on netscape
//      if ( isNetscape(context) )
//        selected = false;
//      else
//      {
//        if (supportsNavigation(context))
//          onClick = "return _select('" + treename + "'," + renderedIndex +
//            ",'" + location + "');";
//      }
//    }

    if (selected)
    {
        treeStyle = TREE_ROW_SELECTED_STYLE_CLASS;
    }

//    context.setCurrentDataObject(curData);

    // render the icon
    if( icon != null)
    {
      String backgroundIcon = getIconBackgroundIcon( expand, leftToRight );
  
      writer.startElement(TABLE_DATA_ELEMENT, null);
  
      if(backgroundIcon != null)
      {
        String backgroundIconURI = getAbsoluteImageURI( context,
                                                        backgroundIcon);
  
        StringBuffer backgroundStyle = new StringBuffer(
                                        _BACKGROUND_IMAGE_URL.length() +
                                        backgroundIconURI.length() +
                                        _END_FUNC.length() );
  
        backgroundStyle.append( _BACKGROUND_IMAGE_URL );
        backgroundStyle.append( backgroundIconURI );
        backgroundStyle.append( _END_FUNC );
        
        writer.writeAttribute(STYLE_ATTRIBUTE,
                              backgroundStyle.toString(),
                null);
      }

      icon.render(context);
      writer.endElement(TABLE_DATA_ELEMENT);
  
      // render space between icon and node stamp
      renderIconCell( context,
                      tree,
                      null,
                      TRANSPARENT_GIF,
                      false,
                      null, // alt Text
                      _NODE_SPACER,
                      _ICON_HEIGHT,
                      null );
    }

    // render the node stamp
    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute( NOWRAP_ATTRIBUTE, Boolean.FALSE, null);
    renderStyleClassAttribute(context, TREE_NODE_ADJUST_STYLE_CLASS);


    writer.startElement(SPAN_ELEMENT, null);
//    out.writeAttribute(ID_ATTRIBUTE,
//                       treename + IntegerUtils.getString(renderedIndex));
    renderStyleClassAttribute(context, treeStyle);
    writer.writeAttribute(ONCLICK_ATTRIBUTE, onClick, null);

    // if screen reader mode render the stamp with level of node from root
    _renderStampBasedOnAccessibilty(context, stamp, nodeDepth);

    writer.endElement(SPAN_ELEMENT);
    writer.endElement(TABLE_DATA_ELEMENT);

    // end row
    writer.endElement(TABLE_ROW_ELEMENT);
    //end table
    writer.endElement(TABLE_ELEMENT);

    // render children
    if ((expand == EXPAND_OPEN) || (expand == EXPAND_ALWAYS))
    {
      tree.enterContainer();
      int childCount = tree.getRowCount();

      if (childCount > 0)
      {
        // prepare the prepended icons for the child nodes
        prepend = _appendIcon(prepend,
                              (isLastSibling)
                                ? Boolean.FALSE
                                : Boolean.TRUE);
        Boolean[] currClone;


        ++nodeDepth; // increment the depth of the child from the root

        int oldIndex = tree.getRowIndex();
        for (int i = 0; i < childCount; i++)
        {
          currClone = new Boolean[prepend.length];
          System.arraycopy(prepend, 0, currClone, 0, prepend.length);

          tree.setRowIndex(i);
          _renderNode(context,
                      tree,
                      icon,
                      stamp,
                      varName,
                      state,
                      selectedPaths,
                      currClone,
                      leftToRight,
                      false,
                      (i == childCount - 1),
                      nodeDepth);
        }
        tree.setRowIndex(oldIndex);
        --nodeDepth;
      }
      tree.exitContainer();
    }
  }

  // is this row childless, open, or closed?
  private int _getExpandValue(
    UIXHierarchy tree,
    RowKeySet state
    )
  {
    if (tree.isContainer())
    {
      if (state.isContained())
        return EXPAND_OPEN;
      else
        return EXPAND_CLOSED;
    }

    return NO_CHILDREN;
  }


    // render an icon with our own special formatting
  private void _renderIcon(
    UIXRenderingContext context,
    String           icon,
    boolean          isIconAbsoluteURI,
    Object           text,
    String           width,
    String           height
    ) throws IOException
  {
    if ( icon != null )
    {
      ResponseWriter writer = context.getResponseWriter();

      writer.startElement(IMAGE_ELEMENT, null);
      renderStyleClassAttribute(context, TREE_ICON_STYLE_CLASS);
      writer.writeAttribute(WIDTH_ATTRIBUTE, width, null);
      writer.writeAttribute(HEIGHT_ATTRIBUTE, height, null);

      // Convert iconURL to an absolute uri
      if (isIconAbsoluteURI)
        renderEncodedResourceURI(context, SOURCE_ATTRIBUTE, icon);
      else
        writeAbsoluteImageURI(context, SOURCE_ATTRIBUTE, icon);
        
      // Ensure that we're never rendering null;  see bug 4161181
      // why this logic is not in renderAltAndTooltipForImage().
      // This is, in essence, re-introducing a more restricted version
      // of that bug.
      renderAltAndTooltipForImage(context, text == null ? "" : text);

      writer.writeAttribute(BORDER_ATTRIBUTE, ZERO, null);

      // =-=gc Not necessary?
      // writer.writeAttribute("vspace", ZERO, null);
      // writer.writeAttribute("hspace", ZERO, null);

      writer.endElement(IMAGE_ELEMENT);
    }
  }





  // add a boolean flag to the chain of icons.
  // the chain is rendered before each icon
  private Boolean[] _appendIcon(
    Boolean[] prepend,
    Boolean isLine
    )
  {
    int currLength = prepend.length;

    if (prepend[currLength - 1] != null)
    {
      // resize, incrementing should be fine
      Boolean[] newBools = new Boolean[currLength +  _DEFAULT_TREE_INCREMENT];
      System.arraycopy(prepend, 0, newBools, 0, currLength);
      prepend = newBools;
    }

    for (int i = 0; i < currLength; i++)
    {
      if (prepend[i] == null)
      {
        prepend[i] = isLine;
        break;
      }
    }

    return prepend;
  }


  private void _prependIcons(
    UIXRenderingContext context,
    UIXHierarchy tree,
    Boolean[] prepend,
    boolean leftToRight
    ) throws IOException
  {
    int currLength = prepend.length;
    Boolean isLine;

    for (int i = 0; i < currLength; i++)
    {

      isLine = prepend[i];

      if (isLine != null)
      {
        String icon = TRANSPARENT_GIF;

        String backgroundIcon =
                      getConnectingBackgroundIcon(isLine.booleanValue(),
                                                  leftToRight);

        renderIconCell( context,
                        tree,
                        backgroundIcon,
                        icon,
                        false,
                        null, // alt text
                        _ICON_WIDTH,
                        _ICON_HEIGHT,
                        null );
      }
    }

  }



  protected UINode getIcon()
  {
    if ( _ICON != null )
      return _ICON;

    _ICON = _createIcon();

    return _ICON;
  }

  protected String getDefaultIconName()
  {
    return null;
  }

  private UINode _createIcon()
  {

    BoundValue isExpandable = new BoundValue()
    {
      public Object getValue(UIXRenderingContext rc)
      {
        UINode uinode = rc.getAncestorNode(1);
        UIXHierarchy tree = (UIXHierarchy) ((UIComponentUINode) uinode).getUIComponent();
        return tree.isContainer()
          ? Boolean.TRUE
          : Boolean.FALSE;
      }
    };

    // icon source
    TreeImageURIBoundValue defaultURI =
      new TreeImageURIBoundValue(getDefaultIconName());
    IfBoundValue defaultIcon = new IfBoundValue(isExpandable, defaultURI, null);
    DataBoundValue specifiedIcon = new DataBoundValue(ICON_KEY);
    DefaultingBoundValue iconSource = new DefaultingBoundValue(specifiedIcon,
                                                               defaultIcon);

    // icon short description
    SkinTranslatedBoundValue defaultIconShortDesc =
                           new SkinTranslatedBoundValue(mapKey(_FOLDER_TIP_KEY));
    DataBoundValue specifiedIconShortDesc =
                                     new DataBoundValue(ICON_SHORT_DESC_KEY);
    DefaultingBoundValue iconShortDesc =
                         new DefaultingBoundValue(specifiedIconShortDesc,
                                                  defaultIconShortDesc);

    MarlinBean icon = new MarlinBean(IMAGE_NAME);
    icon.setAttributeValue(SOURCE_ATTR, iconSource);
    icon.setAttributeValue(STYLE_CLASS_ATTR, TREE_ICON_STYLE_CLASS);
    icon.setAttributeValue(WIDTH_ATTR, _ICON_WIDTH);
    icon.setAttributeValue(HEIGHT_ATTR, _NODE_ICON_HEIGHT);
    icon.setAttributeValue(SHORT_DESC_ATTR, iconShortDesc);
    return icon;
  }

  // create the UINode that will be used as the default stamp:  link
  private static UINode _createDefaultStamp()
  {
    MarlinBean link = new MarlinBean(LINK_NAME);
    link.setAttributeValue(TEXT_ATTR, new DataBoundValue(TEXT_KEY));
    link.setAttributeValue(DESTINATION_ATTR, DESTINATION_KEY);
    link.setAttributeValue(TARGET_FRAME_ATTR,
                           new DataBoundValue(TARGET_FRAME_KEY));

    return link;
  }

  private void _renderStampBasedOnAccessibilty(
    UIXRenderingContext context,
    UINode node,
    int depth
    ) throws IOException
  {
    if (isScreenReaderMode(context))
    {
      RenderingContext arc = RenderingContext.getCurrentInstance();
      FacesContext fc = FacesContext.getCurrentInstance();
      if(isRightToLeft(context))
      {
        node.render(context);
        TreeUtils.writeNodeLevel(fc, arc, depth, mapKey(_NODE_LEVEL_TEXT_KEY));
      }
      else
      {
        TreeUtils.writeNodeLevel(fc, arc, depth, mapKey(_NODE_LEVEL_TEXT_KEY));
        node.render(context);
      }
    }
    else
      node.render(context);
  }

  private static final String _BACKGROUND_IMAGE_URL = "background-image:url(";
  private static final String _END_FUNC = ");";

  private static final UINode _DEFAULT_STAMP = _createDefaultStamp();

  private static UINode _ICON;

  private static final String _ICON_WIDTH  = "16";
  private static final String _ICON_HEIGHT = "22";
  private static final String _NODE_ICON_HEIGHT = "16";
  private static final String _NODE_SPACER = "6";


  // expanded states
  protected static final int    NO_CHILDREN       = 0;
  protected static final int    EXPAND_CLOSED = 1;
  protected static final int    EXPAND_OPEN   = 2;
  protected static final int    EXPAND_ALWAYS = 3;

  // prepend chain constants
  private static final int     _DEFAULT_TREE_DEPTH     = 10;
  private static final int     _DEFAULT_TREE_INCREMENT = 5;

  // =-= ACW: this key is used to make sure that certain javascript functions
  // used by this renderer, are rendered only once per render cycle.
  private static final Object _JS_RENDERED_KEY = new Object();

  // Key used by StyledTextBean to query style class
  static final String _STYLE_CLASS_KEY = "_styleClass";

  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  public static class TreeImageURIBoundValue implements BoundValue
  {
    public TreeImageURIBoundValue(String imageName)
    {
      _imageName = imageName;
    }

    public Object getValue(UIXRenderingContext context)
    {
      return getAbsoluteImageURI(context, _imageName);
    }

    private String _imageName;
  }

  protected String mapKey(String key)
  {
    return key;
  }

  // translation keys
  private static final String _DISABLED_COLLAPSE_TIP_KEY = 
    "af_tree.DISABLED_COLLAPSE_TIP";
  private static final String _COLLAPSE_TIP_KEY =
    "af_tree.COLLAPSE_TIP";
  private static final String _EXPAND_TIP_KEY = 
    "af_tree.EXPAND_TIP";
  private static final String _FOLDER_TIP_KEY =
    "af_tree.FOLDER_TIP";
  private static final String _NODE_LEVEL_TEXT_KEY = 
    "af_tree.NODE_LEVEL";    
  
  private static final String _PATH_PARAM = 
    "path";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TreeRenderer.class);
}
