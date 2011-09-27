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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.component.core.data.CoreTree;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TreeUtils;


/**
 * Renderer for trees.
 *
 */
public class TreeRenderer extends XhtmlRenderer
{
  public TreeRenderer()
  {
    this(CoreTree.TYPE);
  }

  protected TreeRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _immediateKey = type.findKey("immediate");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   * @todo do not mess with selection here. queue an event.
   */
  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    String       clientId)
  {
    Map<String, String> parameters =
      facesContext.getExternalContext().getRequestParameterMap();
    String source = parameters.get(XhtmlConstants.SOURCE_PARAM);

    if (clientId == null)
    {
      clientId = getClientId(facesContext, component);
    }
    if (!clientId.equals(source))
      return;

    TreeUtils.decodeExpandEvents(parameters, component,
                                 Collections.emptyList());
    String currencyStrParam =
      source + NamingContainer.SEPARATOR_CHAR + SELECTED_PARAM;
    String currencyStr = parameters.get(currencyStrParam);
    if ((currencyStr != null) && (!"".equals(currencyStr)))
    {
      UIXTree tree = (UIXTree) component;
      Object oldPath = tree.getRowKey();
      tree.setClientRowKey(currencyStr);
      tree.getSelectedRowKeys().clear();
      tree.getSelectedRowKeys().add();
      tree.setRowKey(oldPath);
    }

    RequestContext.getCurrentInstance().addPartialTarget(component);
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // Since Train is a naming container, we can be more
    // efficient about skipping its children
    if (!PartialPageUtils.containsPprTargets(rc,
                                             component,
                                             getClientId(context, component)))
    {
      return;
    }

    UIXHierarchy tree = (UIXHierarchy) component;
    TreeUtils.expandFocusRowKey((UIXTree) component);

    Object oldPath = tree.getRowKey();
    try
    {
      boolean continueRendering = setInitialPath(tree, bean);
      if (!continueRendering)
        return;

      _renderContent(context, rc, tree, bean);
    }
    finally
    {
      tree.setRowKey(oldPath);
    }
  }

  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  private void _renderContent(
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     tree,
    FacesBean        bean
    ) throws IOException
  {
    FormData fd = rc.getFormData();
    if (fd == null)
    {
      _LOG.warning("TREE_COMPONENT_MUST_INSIDE_FORM");
      return;
    }

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("div", tree);
    renderId(context, tree);
    renderAllAttributes(context, rc, tree, bean);

    final String id = getClientId(context, tree);
    UIComponent stamp = getFacet(tree, CoreTree.NODE_STAMP_FACET);

    //@todo - will this tree.getFocusPath survive?
    //    List focusPath = getFocusPath(context, node);
    Object focusPath = tree.getFocusRowKey();
    String formName = fd.getName();

    // Bug 3931544:  don't use colons in Javascript variable names.
    // We'll just replace colons with underscores;  not perfect, but adequate
    final String varName = "_adftree" + XhtmlUtils.getJSIdentifier(id);

    boolean leftToRight = !rc.isRightToLeft();
    int rootSize = tree.getRowCount();
    RowKeySet state = getExpandedRowKeys(tree);
    Map<Object, Boolean> selectedPaths = getSelectedPaths(focusPath);

    // render each of the root nodes
    for (int i = 0; i < rootSize; i++)
    {
      tree.setRowIndex(i);
      _renderNode(context, rc, tree, bean, stamp, varName, state,
                  selectedPaths, new Boolean[_DEFAULT_TREE_DEPTH],
                  leftToRight, (i == 0), (i == rootSize - 1), 0);
    }

    //HKuhn - not needed in printable mode (scripting disabled)
    if (supportsScripting(rc))
    {
      rw.startElement("script", null);
      renderScriptDeferAttribute(context, rc);
      renderScriptTypeAttribute(context, rc);

      _renderTreeJS(context, rc, tree, bean);

      //out.writeText("_setNodes('"+name+"','"+nodesRendered+"');");

      String selectedParam =
        id + NamingContainer.SEPARATOR_CHAR + SELECTED_PARAM;
      String focusNodeId = TreeUtils.getFocusNodeClientId(context, tree);

      rw.writeText("var " + varName + " = " +
                       _createNewJSSelectionState(formName, id, selectedParam,
                                                  focusNodeId), null);
      rw.endElement("script");

      fd.addNeededValue(selectedParam);
    }
    rw.endElement("div");

    fd.addNeededValue(_PATH_PARAM);

  }

  // return whether to continue with rendering

  protected boolean setInitialPath(
    UIXHierarchy tree,
    FacesBean    bean)
  {
    tree.setRowKey(null);
    return true;
  }

  private boolean _isShownSelected(
    UIXHierarchy         tree,
    Map<Object, Boolean> selectedPaths,
    Object               currPath)
  {

    boolean selected = false;
    if (tree instanceof UIXTree)
      selected = ((UIXTree) tree).getSelectedRowKeys().isContained();

    if (selected)
      return true;

    Object value = selectedPaths.get(currPath);

    if (value != null)
      return true;

    return false;
  }

  protected Map<Object, Boolean> getSelectedPaths(
    Object focusPath)
  {
    if (focusPath == null)
      return new HashMap<Object, Boolean>(0);

    Map<Object, Boolean> selectedPaths = new HashMap<Object, Boolean>(1);

    selectedPaths.put(focusPath, Boolean.TRUE);
    return selectedPaths;
  }

  protected RowKeySet getExpandedRowKeys(
    UIXHierarchy tree)
  {
    return ((UIXTree) tree).getDisclosedRowKeys();
  }

  /**
   * Returns true if the tree connecting lines should be rendered
   * @param rc the RenderingContext
   * @return the value of the AF_TREE_SHOW_LINES skin property
   */
  protected boolean isShowLines(
    RenderingContext rc)
  {
    Object showLines = rc.getSkin().getProperty(SkinProperties.AF_TREE_SHOW_LINES);
    return showLines == null || showLines.equals(Boolean.TRUE);
  }

  /**
   * Returns the URI of the vertical line icon
   *
   * @param context     the FacesContext
   * @param rc          the RenderingContext
   * @param isLine      if there is need for a vertical line
   * @param leftToRight left to right
   * @return the URI of the icon
   */
  protected String getConnectingBackgroundIcon(
    FacesContext     context,
    RenderingContext rc,
    boolean          isLine,
    boolean          leftToRight)
  {
    if (!isLine || !isShowLines(rc))
      return null;
    Icon icon = rc.getIcon(SkinSelectors.AF_TREE_LINE_ICON);
    return (icon == null) ? null : icon.getImageURI(context, rc).toString();
  }

  /**
   * Returns the URI of the expanded-icon's background icon
   * Usually the lines connecting a node
   * @param context the FacesContext
   * @param rc the RenderingContext
   * @param isLastSibling true if the node is the last child of its parent node
   * @param isLeftToRight left to right
   * @return the URI of the icon
   */
  protected String getIconBackgroundIcon(
    FacesContext     context,
    RenderingContext rc,
    boolean          isLastSibling,
    boolean          isLeftToRight)
  {
    Object showLines = rc.getSkin().getProperty(SkinProperties.AF_TREE_SHOW_LINES);
    if (!isShowLines(rc))
      return null;
    Icon nodeBackgroundIcon = rc.getIcon(isLastSibling
        ? SkinSelectors.AF_TREE_LINE_LAST_ICON : SkinSelectors.AF_TREE_LINE_MIDDLE_ICON);
    return (nodeBackgroundIcon != null)
        ? nodeBackgroundIcon.getImageURI(context, rc).toString() : null;
  }

  // render the correct icon for a specific node

  protected void renderExpandCell(
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     tree,
    int              expanded,
    boolean          isLastSibling,
    String           onclick
    ) throws IOException
  {
    Icon icon = null;
    String iconURI = null;

    String backgroundIconURI;
    String nodeBackgroundIconURI;

    boolean isAbsoluteImageURI = true;

    String iconHeight = null;

    Object altText = null;

    String text = null;

    boolean isMacOS =
               Agent.PLATFORM_MACOS.equals(rc.getAgent().getPlatformName());
    // add in the expandability
    switch (expanded)
    {
      case NO_CHILDREN:
        icon = rc.getIcon(SkinSelectors.AF_TREE_NO_CHILDREN_ICON);
        if (icon == null || icon.getImageURI(context, rc) == null)
        {
          iconURI = TRANSPARENT_GIF;
          iconHeight = _ICON_HEIGHT;
          isAbsoluteImageURI = false;
        }
        break;
      case EXPAND_CLOSED:
        // "\u21D2"; // Double Arrow right

        if (isMacOS)
          // single arrow left
          text = rc.isRightToLeft() ? "\u2190" : "\u2192"; // single arrow right
        else if (isPDA(rc))
          //for PDAs use a simple "+" or "-" since miscellaneous unicode characters
          //are not supported
          text = "[+]"; //plus sign
        else // triangle left
          text = rc.isRightToLeft() ? "\u25C4" : "\u25BA"; // triangle right

        altText = rc.getTranslatedString(_EXPAND_TIP_KEY);
        icon = rc.getIcon(SkinSelectors.AF_TREE_COLLAPSED_ICON);
        break;
      case EXPAND_OPEN:
        //"\u21D3"; // double arrow down
        if (isMacOS)
          text = "\u2193"; // single arrow down
        else if (isPDA(rc))
          //for PDAs use a simple "+" or "-" since miscellaneous unicode characters
          //are not supported
          text = "[-]"; //plus sign
        else
          text = "\u25BC"; // triangle down

        altText = rc.getTranslatedString(_COLLAPSE_TIP_KEY);
        icon = rc.getIcon(SkinSelectors.AF_TREE_EXPANDED_ICON);
        break;
      case EXPAND_ALWAYS:
        if (isMacOS)
          text = "\u2193"; // single arrow down
        else if (isPDA(rc))
          text = "[-]"; //plus sign
        else
          text = "\u25BC"; // triangle down
        //for PDAs use a simple "+" or "-" since miscellaneous unicode character
        //s are not supported

        altText = rc.getTranslatedString(_DISABLED_COLLAPSE_TIP_KEY);
        icon = rc.getIcon(SkinSelectors.AF_TREE_EXPANDED_ICON);
        break;
    }

    if (iconURI == null && icon != null)
    {
      //This can be null so we need to check for it before doing toString
      Object o = icon.getImageURI(context, rc);
      if(o != null)
      {
        iconURI = o.toString();
      }
    }

    backgroundIconURI = getConnectingBackgroundIcon(context, rc, !isLastSibling, true);
    nodeBackgroundIconURI = getIconBackgroundIcon(context, rc, isLastSibling, true);

    if (iconURI != null)
    {
      renderExpandIconCell(context, rc,
                            backgroundIconURI, nodeBackgroundIconURI,
                            iconURI, isAbsoluteImageURI,
                            altText, _ICON_WIDTH, iconHeight, onclick);
    } else
    {
      _renderTextCell(context, rc, tree, expanded, text, altText, _ICON_WIDTH, onclick,
                      SkinSelectors.TREE_DISCLOSED_SYMBOL_STYLE_CLASS);
    }
  }

  private void _renderTextCell(
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     tree,
    int              expanded,
    String           text,
    Object           altText,
    String           width,
    String           onclick,
    String           styleClass
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, width, null);
    writer.writeAttribute("title", altText, null);
    renderStyleClass(context, rc, styleClass);
    boolean jsSupport = supportsScripting(rc);

    if (onclick != null)
    {
      if (jsSupport)
      {
        writer.startElement(XhtmlConstants.LINK_ELEMENT, null);
        writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
        writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onclick, null);
      }
      else
      {
        // For Non-JavaScript browsers, render an input element(type= submit) to
        // submit the page. Encode the name attribute with the parameter name
        // and value thus it would enable the browsers to include the name of
        // this element in its payLoad if it submits the page.
        String nameAttr = TreeUtils.renderEncodedNameAttri(
                                        context,
                                        rc,
                                        tree,
                                        getClientId(context, tree),
                                        expanded == EXPAND_CLOSED);

        writer.startElement("input", null);
        writer.writeAttribute("type", "submit",null);
        writer.writeAttribute("name", nameAttr, null);
        writer.writeAttribute("value", text, null);
      }
    }

    if (text != null && jsSupport)
      writer.writeText(text, null);

    if (onclick != null)
    {
      if (jsSupport)
      {
        writer.endElement(XhtmlConstants.LINK_ELEMENT);
      }
      else
      {
        writer.endElement("input");
      }
    }

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }

  protected void renderExpandIconCell(
    FacesContext     context,
    RenderingContext rc,
    String           backgroundIcon,
    String           nodeBackgroundIcon,
    String           icon,
    boolean          isIconAbsoluteURI,
    Object           altText,
    String           width,
    String           height,
    String           onclick
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, width, null);
    writer.writeAttribute(XhtmlConstants.HEIGHT_ATTRIBUTE, "100%", null);
    writer.writeAttribute(XhtmlConstants.VALIGN_ATTRIBUTE, XhtmlConstants.V_ALIGN_TOP,
                          null);

    if (backgroundIcon != null)
    {
//      String backgroundIconURI = getAbsoluteImageUri(context, rc, backgroundIcon);
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
                            _BACKGROUND_IMAGE_URL + backgroundIcon +
                                _END_FUNC, null);
    }

    if (nodeBackgroundIcon != null)
    {
      writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
                            _BACKGROUND_IMAGE_URL + nodeBackgroundIcon +
                                _END_FUNC + _BACKGROUND_NO_REPEAT, null);
    }

    if (onclick != null)
    {
      writer.startElement(XhtmlConstants.LINK_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
      writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onclick, null);
    }

    _renderIcon(context, rc, icon, isIconAbsoluteURI, altText, null, height);

    if (onclick != null)
    {
      writer.endElement(XhtmlConstants.LINK_ELEMENT);
    }

    if (nodeBackgroundIcon != null)
    {
      writer.endElement(XhtmlConstants.DIV_ELEMENT);
    }

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }


  protected void renderIconCell(
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     tree,
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

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, width, null);

    if (backgroundIcon != null)
    {
      String backgroundIconURI = backgroundIcon;//getAbsoluteImageUri(context, rc, backgroundIcon);
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
                            _BACKGROUND_IMAGE_URL + backgroundIconURI +
                            _END_FUNC, null);
    }

    if (onclick != null)
    {
      writer.startElement(XhtmlConstants.LINK_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
      writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onclick, null);
      String treeName = getClientId(context, tree);
      String id = treeName + NamingContainer.SEPARATOR_CHAR + "lnk";
      writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, id, null);
    }

    _renderIcon(context, rc, icon, isIconAbsoluteURI, altText, width, height);

    if (onclick != null)
    {
      writer.endElement(XhtmlConstants.LINK_ELEMENT);
    }

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }


  private static String _createFocusNodeGetter(
    String focusNodeId)
  {
    return (focusNodeId != null) ? "document.getElementById('" + focusNodeId + "')" : null;
  }

  private static String _callJSSelect(
    UIXHierarchy tree,
    String       jsVarName)
  {
    String currencyStr = tree.getClientRowKey();
    return jsVarName + ".select(this,'" + currencyStr + "');";
  }

  private String _createNewJSSelectionState(
    String formName,
    String treeClientId,
    String selectParam,
    String focusNodeId)
  {
    String treeState = TreeUtils.createNewJSCollectionComponentState(formName, treeClientId);
    String focusNode = _createFocusNodeGetter(focusNodeId);
    String jsSelectionState = null;
    if (focusNode == null)
    {
      jsSelectionState = "new _adfTreeSelector('" + selectParam + "'," + treeState + ");";
    }
    else
    {
      jsSelectionState = "new _adfTreeSelector('" + selectParam + "'," + treeState + "," + focusNode + ");";
    }
    return jsSelectionState;
  }

  private void _renderTreeJS(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (!rc.getProperties().containsKey(_JS_RENDERED_KEY))
    {
      rc.getProperties().put(_JS_RENDERED_KEY, Boolean.TRUE);
      ResponseWriter writer = context.getResponseWriter();
      writer.writeText("function _adfTreeSelector(selectParam,tState,focusNode) {" +
                       "this._selectParam = selectParam;" +
                       "this.treeState = tState;" +
                       "this._pTag = focusNode;" +
                       "}" +
                       "_adfTreeSelector.prototype.select = function(tag,path) {" +
                       "if (this._pTag != null) {" +
                       "this._pTag.className='" + SkinSelectors.TREE_ROW_STYLE_CLASS +
                       "';" + "}" + "this._pTag = tag;" +
                       //if there are any problems see TRINIDAD-935
                       "tag.className='" + SkinSelectors.TREE_ROW_SELECTED_STYLE_CLASS +
                       "';" + "};", null);


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

      boolean immediate = getImmediate(component, bean);
      String buff =
        TreeUtils.setupJSTreeCollectionComponent(!immediate) + ";";
      writer.writeText(buff, null);
    }
  }

  // render one row of the tree

  private void _renderNode(
    FacesContext         context,
    RenderingContext     rc,
    UIXHierarchy         tree,
    FacesBean            bean,
    UIComponent          stamp,
    final String         varName,
    RowKeySet            state,
    Map<Object, Boolean> selectedPaths,
    Boolean[]            prepend,
    boolean              leftToRight,
    boolean              isFirstSibling,
    boolean              isLastSibling,
    int                  nodeDepth
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // each row is a table
    writer.startElement(XhtmlConstants.TABLE_ELEMENT, null);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", "0", "0", null);
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);


    // render the prepend
    _prependIcons(context, rc, tree, prepend, leftToRight);


    String onclickExpand = null;
    int expand = _getExpandValue(tree, state);

    if ((expand != NO_CHILDREN) && supportsNavigation(rc))
    {
      onclickExpand =
          TreeUtils.callJSExpandNode(tree, varName + ".treeState",
                                     (expand == EXPAND_CLOSED));
    }

    renderExpandCell(context, rc, tree, expand, isLastSibling, onclickExpand);


    //    DataObject curData = BeanAdapterUtils.getAdapter(context, tree.getRowData());
    String treeStyle = SkinSelectors.TREE_ROW_STYLE_CLASS;


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
      treeStyle = SkinSelectors.TREE_ROW_SELECTED_STYLE_CLASS;
    }

    renderNodeIconCell(context, rc, tree, expand);

    // render space between icon and node stamp
    // alt Text
    renderIconCell(context, rc, tree, null, TRANSPARENT_GIF, false, null,
                   _NODE_SPACER, _ICON_HEIGHT, null);

    // render the node stamp
    renderStampCell(context, rc, tree, stamp, onClick, treeStyle, nodeDepth);

    // end row
    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
    //end table
    writer.endElement(XhtmlConstants.TABLE_ELEMENT);

    // render children
    if ((expand == EXPAND_OPEN) || (expand == EXPAND_ALWAYS))
    {
      _renderNodeChildren(context, rc, tree, bean, stamp, varName, state, selectedPaths,
                          prepend, leftToRight, isFirstSibling, isLastSibling, nodeDepth);
    }
  }

  protected void renderNodeIconCell(
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     tree,
    int              expand
    ) throws IOException
  {
    String nodeType = getNodeType(tree);
    Icon nodeIcon = getNodeIcon(rc, nodeType, expand);

    ResponseWriter writer = context.getResponseWriter();
    // render the node icon
    if (nodeIcon != null)
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, _ICON_WIDTH, null);
      writer.writeAttribute(XhtmlConstants.HEIGHT_ATTRIBUTE, _ICON_HEIGHT, null);
      writer.writeAttribute(XhtmlConstants.VALIGN_ATTRIBUTE, XhtmlConstants.V_ALIGN_TOP,
                            null);

      _renderIcon(context, rc, nodeIcon.getImageURI(context, rc).toString(),
                  true, null, null, null);

      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
  }

  protected String getNodeType(
    UIXHierarchy tree)
  {
    String nodeType = null;
    Object rowData = tree.getRowData();
    Class rowClass = rowData.getClass();
    Method method = null;

    try
    {
      method = rowClass.getMethod("getNodeType");
      if (method != null && method.getReturnType().equals(String.class))
      {
        nodeType = (String) method.invoke(rowData);
      }
    }
    catch (IllegalAccessException e) { ; }
    catch (NoSuchMethodException e) { ; }
    catch (InvocationTargetException e) { ; }
    return nodeType;
  }

  protected String getNodeIconSelector(
    String nodeType,
    int    expandedState)
  {
    switch (expandedState)
    {
      case EXPAND_OPEN:
        nodeType += NODE_ICON_EXPANDED_SUFFIX;
        break;
      case EXPAND_CLOSED:
        nodeType += NODE_ICON_COLLAPSED_SUFFIX;
        break;
    }
    return SkinSelectors.AF_TREE_NODE_ICON + ":" + nodeType;
  }

  protected Icon getNodeIcon(
    RenderingContext rc,
    String           nodeType,
    int              expandedState
  )
  {
    if (nodeType == null || nodeType.length() == 0)
    {
      return null;
    }
    Icon icon = rc.getIcon(getNodeIconSelector(nodeType, expandedState));
    if (icon == null)
      if (expandedState != NO_CHILDREN)
      {
        icon = rc.getIcon(getNodeIconSelector(nodeType, NO_CHILDREN));
      }
      else
      {
        icon = rc.getIcon(getNodeIconSelector(nodeType, EXPAND_CLOSED));
      }
    return icon;
  }


  protected void renderStampCell(
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     tree,
    UIComponent      stamp,
    String           onClick,
    String           treeStyle,
    int              nodeDepth
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.NOWRAP_ATTRIBUTE, Boolean.FALSE, null);
    renderStyleClass(context, rc, SkinSelectors.TREE_NODE_ADJUST_STYLE_CLASS);


    writer.startElement(XhtmlConstants.SPAN_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, getClientId(context, tree), null);
    renderStyleClass(context, rc, treeStyle);

    if (supportsScripting(rc))
    {
      writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onClick, null);
    }

    // if screen reader mode render the stamp with level of node from root
    _renderStampBasedOnAccessibilty(context, rc, stamp, nodeDepth);

    writer.endElement(XhtmlConstants.SPAN_ELEMENT);
    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }

  private void _renderNodeChildren(
    FacesContext         context,
    RenderingContext     rc,
    UIXHierarchy         tree,
    FacesBean            bean,
    UIComponent          stamp,
    final String         varName,
    RowKeySet            state,
    Map<Object, Boolean> selectedPaths,
    Boolean[]            prepend,
    boolean              leftToRight,
    boolean              isFirstSibling,
    boolean              isLastSibling,
    int                  nodeDepth
    ) throws IOException
  {
    tree.enterContainer();
    int childCount = tree.getRowCount();

    if (childCount > 0)
    {
      // prepare the prepended icons for the child nodes
      prepend = _appendIcon(prepend, (isLastSibling) ? Boolean.FALSE : Boolean.TRUE);
      Boolean[] currClone;

      ++nodeDepth; // increment the depth of the child from the root

      int oldIndex = tree.getRowIndex();
      for (int i = 0; i < childCount; i++)
      {
        currClone = new Boolean[prepend.length];
        System.arraycopy(prepend, 0, currClone, 0, prepend.length);

        tree.setRowIndex(i);
        _renderNode(context, rc, tree, bean, stamp, varName, state,
                    selectedPaths, currClone, leftToRight, i == 0,
                    (i == childCount - 1), nodeDepth);
      }
      tree.setRowIndex(oldIndex);
      --nodeDepth;
    }
    tree.exitContainer();
  }

  // is this row childless, open, or closed?

  private int _getExpandValue(
    UIXHierarchy tree,
    RowKeySet    state)
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
    FacesContext     context,
    RenderingContext rc,
    String           icon,
    boolean          isIconAbsoluteURI,
    Object           text,
    String           width,
    String           height
    ) throws IOException
  {
    if (icon != null)
    {
      ResponseWriter writer = context.getResponseWriter();

      // TODO: change
      writer.startElement("img", null);
      renderStyleClass(context, rc, SkinSelectors.TREE_ICON_STYLE_CLASS);
      if (width != null)
        writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, width, null);
      if (height != null)
        writer.writeAttribute(XhtmlConstants.HEIGHT_ATTRIBUTE, height, null);

      // Convert iconURL to an absolute uri
      if (!isIconAbsoluteURI)
        icon = getAbsoluteImageUri(context, rc, icon);

      renderEncodedResourceURI(context, "src", icon);

      // Ensure that we're never rendering null;  see bug 4161181
      // why this logic is not in renderAltAndTooltipForImage().
      // This is, in essence, re-introducing a more restricted version
      // of that bug.
      OutputUtils.renderAltAndTooltipForImage(context, rc, text == null? "": text);

      writer.writeAttribute("border", "0", null);
      writer.endElement("img");
    }
  }


  // add a boolean flag to the chain of icons.
  // the chain is rendered before each icon

  private Boolean[] _appendIcon(
    Boolean[] prepend,
    Boolean   isLine)
  {
    int currLength = prepend.length;

    if (prepend[currLength - 1] != null)
    {
      // resize, incrementing should be fine
      Boolean[] newBools =
        new Boolean[currLength + _DEFAULT_TREE_INCREMENT];
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
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     tree,
    Boolean[]        prepend,
    boolean          leftToRight
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
            getConnectingBackgroundIcon(context, rc, isLine.booleanValue(), leftToRight);

        // alt text
        renderIconCell(context, rc, tree, backgroundIcon, icon, false, null,
                       _ICON_WIDTH, "100%", null);
      }
    }
  }

  protected boolean getImmediate(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_immediateKey);
    if (o == null)
      o = _immediateKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected String getDefaultIconName()
  {
    return null;
  }

  private void _renderStampBasedOnAccessibilty(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      stamp,
    int              depth
    ) throws IOException
  {
    if (isScreenReaderMode(rc))
    {
      RenderingContext arc = RenderingContext.getCurrentInstance();
      FacesContext fc = FacesContext.getCurrentInstance();
      if (rc.isRightToLeft())
      {
        //TODO: do we need default stamp support???
        encodeChild(context, stamp);
        TreeUtils.writeNodeLevel(fc, arc, depth, _NODE_LEVEL_TEXT_KEY);
      }
      else
      {
        TreeUtils.writeNodeLevel(fc, arc, depth, _NODE_LEVEL_TEXT_KEY);
        encodeChild(context, stamp);
      }
    }
    else
      encodeChild(context, stamp);
  }

  private static final String _BACKGROUND_IMAGE_URL =
    "background-image:url(";
  private static final String _END_FUNC = ");";
  private static final String _BACKGROUND_NO_REPEAT = "background-repeat:no-repeat;";

  private static final String _ICON_WIDTH = "19";
  private static final String _ICON_HEIGHT = "18";
  private static final String _NODE_SPACER = "3";

  // expanded states
  protected static final int NO_CHILDREN = 0;
  protected static final int EXPAND_CLOSED = 1;
  protected static final int EXPAND_OPEN = 2;
  protected static final int EXPAND_ALWAYS = 3;

  // prepend chain constants
  private static final int _DEFAULT_TREE_DEPTH = 10;
  private static final int _DEFAULT_TREE_INCREMENT = 5;

  // =-= ACW: this key is used to make sure that certain javascript functions
  // used by this renderer, are rendered only once per render cycle.
  private static final Object _JS_RENDERED_KEY = new Object();

  // Key used by StyledTextBean to query style class
  static final String _STYLE_CLASS_KEY = "_styleClass";

  private PropertyKey _immediateKey;

  // translation keys
  private static final String _DISABLED_COLLAPSE_TIP_KEY =
    "af_tree.DISABLED_COLLAPSE_TIP";
  private static final String _COLLAPSE_TIP_KEY = "af_tree.COLLAPSE_TIP";
  private static final String _EXPAND_TIP_KEY = "af_tree.EXPAND_TIP";
  private static final String _NODE_LEVEL_TEXT_KEY = "af_tree.NODE_LEVEL";

  private static final String _PATH_PARAM = "path";
  public static final String SELECTED_PARAM = "_selected";

  public static final String NODE_ICON_EXPANDED_SUFFIX = "-expanded";
  public static final String NODE_ICON_COLLAPSED_SUFFIX = "-collapsed";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TreeRenderer.class);
}
