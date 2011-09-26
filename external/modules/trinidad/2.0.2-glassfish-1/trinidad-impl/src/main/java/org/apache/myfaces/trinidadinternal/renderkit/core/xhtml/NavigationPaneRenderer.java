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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.application.Application;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.TableUtils;
import org.apache.myfaces.trinidad.component.UIXCommand;
import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationLevel;
import org.apache.myfaces.trinidad.component.core.nav.CoreNavigationPane;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.RenderUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;


public class NavigationPaneRenderer extends XhtmlRenderer
{
  public NavigationPaneRenderer()
  {
    super(CoreNavigationPane.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _hintKey = type.findKey("hint");
    _titleKey = type.findKey("title");
    _disabledKey = type.findKey("disabled");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // Since NavigationPane is a naming container, we can be more
    // efficient about skipping its children
    if (!PartialPageUtils.containsPprTargets(rc,
                                             component,
                                             getClientId(context, component)))
    {
      return;
    }

    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("div", component);
    renderAllAttributes(context, rc, component, bean, false);
    _renderStyleAttributes(context, rc, component, bean);
    renderId(context, component);

    int renderedItemCount = _getItemCount((UIXHierarchy)component);
    int renderedRowCount = ((UIXHierarchy)component).getRowCount();

    // no kids, no NavigationLevel -- but you still get the span.
    if (renderedItemCount > 0 || renderedRowCount > 0)
    {
      renderContent(context, rc,
                    (UIXHierarchy)component, bean);
    }

    writer.endElement("div");
  }

  /**
   * Gets the stamp to use to render each link
   */
  private UIComponent _getStamp(
    UIXHierarchy component
    )
  {
    UIComponent stamp = component.getFacet("nodeStamp");
    return stamp;
  }

  @SuppressWarnings("unchecked")
  protected void renderContent(
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     component,
    FacesBean        bean
    ) throws IOException
  {
    NavItemData navItemData = new NavItemData();
    List<UIComponent> nonNavItemList = new ArrayList<UIComponent>();
    String renderingHint = _getHint(component, bean);

    UIComponent nodeStamp = _getStamp(component);
    if (nodeStamp == null)
    {
      // we aren't stamping, but rather have explicitly defined children:
      for(UIComponent child : (List<UIComponent>)component.getChildren())
      {
        if (child.isRendered())
        {
          UIXCommand navItem;
          if (child instanceof UIXCommand)
          {
            navItem = (UIXCommand) child;
            // collect the information needed to render this nav item:
            _collectNavItemData(navItemData, navItem, -1, component);
          }
          else if(renderingHint == NavigationPaneRenderer._HINT_BAR ||
                  renderingHint == NavigationPaneRenderer._HINT_BUTTONS)
          {
            navItemData.addItemData(null);
            nonNavItemList.add(child);
          }
          else
          {
            // we don't support a non-command child for other hints.
            _LOG.severe("ILLEGAL_COMPONENT_HIERARCHY_UIXCOMMAND_EXPECTED");
            return;
          }
        }
      }
    }
    else
    {
      if (!(nodeStamp instanceof UIXCommand))
      {
        _LOG.severe("ILLEGAL_COMPONENT_HIERARCHY_UIXCOMMAND_EXPECTED");
        return;

      }

      UIXCommand navStamp = (UIXCommand) nodeStamp;

      // we are stamping the children:
      // Save the current key
      Object oldPath = component.getRowKey();

      _setStartDepthPath(component,
                         ((UIXNavigationLevel) component).getLevel());

      int componentRowCount = component.getRowCount();
      if (componentRowCount != 0)
      {
        int startIndex = component.getFirst();
        int endIndex = TableUtils.getLast(component, startIndex);

        for (int i = startIndex; i <= endIndex; i++)
        {
          component.setRowIndex(i);

          if (navStamp.isRendered())
          {
            // collect the information needed to render this nav item:
            _collectNavItemData(navItemData, navStamp, i, component);
          }
        }
      }

      // Restore the old path
      component.setRowKey(oldPath);
    }

    // We must loop this second time because we support overlapping items,
    // arbitrary child wrappers; we need to know information like being the
    // first, the last, or whether nearby another active item.
    int visibleItemCount = navItemData.getItemCount();
    if (visibleItemCount != 0)
    {
      // starting outer markup:
      ResponseWriter rw = context.getResponseWriter();
      boolean isRtl = rc.getLocaleContext().isRightToLeft();
      boolean isChoiceHint =
        (NavigationPaneRenderer._HINT_CHOICE.equals(renderingHint));
      String choiceSelectId = null;
      if (isChoiceHint)
      {
        choiceSelectId = getClientId(context, component) + _CHOICE_SELECT_ID_SUFFIX;
        if (isRtl)
        {
          _renderChoiceButton(context, rc, rw, isRtl, choiceSelectId);
        }
        else
        {
          _renderChoiceLabel(context, rc, rw, isRtl, component, bean);
        }

        rw.startElement("select", null);
        rw.writeAttribute("id", choiceSelectId, null);

        // For Non-JavaScript browsers, render the name attribute thus it would
        // enable the browsers to include the name and value of this element
        // in its payLoad.

        if ( !supportsScripting(rc) )
        {
          rw.writeAttribute("name", choiceSelectId, null);
        }
        renderStyleClass(context, rc,
          SkinSelectors.AF_NAVIGATION_LEVEL_CHOICE_OPTIONS_STYLE_CLASS);
        if (getDisabled(component, bean))
          rw.writeAttribute("disabled", Boolean.TRUE, null);
      }

      int lastRowIndex = visibleItemCount - 1;
      boolean previousActive = false;
      int nextActiveIndex = navItemData.getEffectiveActiveIndex() - 1;
      Object oldPath = component.getRowKey();

      _setStartDepthPath(component,
                           ((UIXNavigationLevel)component).getLevel());

      Iterator<UIComponent> iter = nonNavItemList.iterator();
      for (int i=0; i<visibleItemCount; i++)
      {
        Map<String, Object> currentItemData = navItemData.getItemData(i);
        //if currentItemData is null,we have a non-command child.
        if (currentItemData != null)
        {
          currentItemData.put("isFirst", (i == 0));
          currentItemData.put("isLast", (i == lastRowIndex));
          currentItemData.put("previousActive", previousActive);
          currentItemData.put("nextActive", (i == nextActiveIndex));
          Integer rowIndex = (Integer) currentItemData.get("rowIndex");
          if (rowIndex != null)
            component.setRowIndex(rowIndex.intValue());
          _renderNavigationItem(context, rc, rw, currentItemData,
              renderingHint, isRtl);
          previousActive =
              getBooleanFromProperty(currentItemData.get("isActive"));
        }
        else
        {
          renderNonCommandChild(i, context, rc,iter.next(), (i == lastRowIndex), renderingHint);
        }
      }
      component.setRowKey(oldPath);

      // Close up any opened choice tags:
      if (isChoiceHint)
      {
        rw.endElement("select");
        if (isRtl)
        {
          _renderChoiceLabel(context, rc, rw, isRtl, component, bean);
        }
        {
          _renderChoiceButton(context, rc, rw, isRtl, choiceSelectId);
        }
      }
    }
  }

  protected boolean hasChildren(
    UIComponent component)
  {
    int childCount = component.getChildCount();
    return childCount > 0;
  }

  /**
   * renderStyleAttributes - use the NavigationLevel style class as the default
   * styleClass
   */
  private void _renderStyleAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String renderingHint = _getHint(component, bean);

    // Since navigation items are rendered vertically for narrow-screen PDAs,
    // Tab style class, which sets the navigationPane's height, is no longer
    // applicable for narrow-screen PDAs
    if (!supportsNarrowScreen(rc) &&
                NavigationPaneRenderer._HINT_TABS.equals(renderingHint))
    {
      renderStyleAttributes(context, rc, component, bean,
                            SkinSelectors.AF_NAVIGATION_LEVEL_TABS_STYLE_CLASS);
    }
    else if (NavigationPaneRenderer._HINT_BAR.equals(renderingHint))
    {
      renderStyleAttributes(context, rc, component, bean,
                            SkinSelectors.AF_NAVIGATION_LEVEL_BAR_STYLE_CLASS);
    }
    else
    {
      renderStyleAttributes(context, rc, component, bean,
                            SkinSelectors.AF_NAVIGATION_LEVEL_STYLE_CLASS);
    }
  }

  private String _getHint(
    UIComponent component,
    FacesBean   bean)
  {
    String renderingHint = toString(bean.getProperty(_hintKey));

    if (renderingHint == null)
    {
      // =-= mcc TODO pull from rc, e.g. when placed by Page, PanelPage
      renderingHint = _HINT_DEFAULT;
    }
    return renderingHint;
  }

  protected String getTitle(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_titleKey));
  }

  protected boolean getDisabled(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_disabledKey);
    if (o == null)
      o = _disabledKey.getDefault();
    return Boolean.TRUE.equals(o);
  }

  /**
   * Renders the client ID as both "id" and "name"
   */
  private void _renderCommandChildId(
    FacesContext context,
    UIXCommand   component
    ) throws IOException
  {
    String clientId = getClientId(context, component);
    // For links, these are actually URI attributes
    context.getResponseWriter().writeURIAttribute("id", clientId, "id");
    context.getResponseWriter().writeURIAttribute("name", clientId, "id");
  }

  private Object _getFocusRowKey(
    UIXHierarchy component
    )
  {
    return component.getFocusRowKey();
  }

  private int _getItemCount(
    UIXHierarchy component
    )
  {
    Object focusPath = _getFocusRowKey(component);
    int kids = getRenderedChildCount(component);

    if (focusPath == null)
      return kids;

    // =-= mcc TODO level is different than path, so the following is wrong:
    return kids + component.getDepth(focusPath) + 1;
  }

  private void _collectNavItemData(
    NavItemData navItemData,
    UIXCommand  commandChild,
    int         rowIndex,
    UIXHierarchy component)
  {
    int itemDataIndex = navItemData.getItemCount();
    boolean isActive;

    // If we're stamping, "active" is based on the model's focus row key
    if (rowIndex >= 0)
    {
      isActive = _isOnFocusPath(component);
    }
    // But if we're not stamping, "active" is just based on the
    // "selected" property of the command component
    else
    {
      isActive = getBooleanFromProperty(
                   _getCommandChildProperty(commandChild,"selected"));
    }

    if (isActive)
    {
      if (navItemData.getEffectiveActiveIndex() == -1)
      {
        navItemData.setEffectiveActiveIndex(itemDataIndex);
      }
      else
      {
        isActive = false; // there can only be 1 active item
      }
    }
    HashMap<String, Object> itemDataMap = new HashMap<String, Object>();
    itemDataMap.put("accessKey", _getCommandChildProperty(commandChild, "accessKey"));
    itemDataMap.put("component", commandChild);
    itemDataMap.put("dataIndex", itemDataIndex);
    itemDataMap.put("destination", _getCommandChildProperty(commandChild, "destination"));
    itemDataMap.put("icon", _getCommandChildProperty(commandChild, "icon"));
    itemDataMap.put("immediate", _getCommandChildProperty(commandChild, "immediate"));
    itemDataMap.put("inlineStyle", _getCommandChildProperty(commandChild, "inlineStyle"));
    itemDataMap.put("isActive", isActive);
    itemDataMap.put("isDisabled", _getCommandChildProperty(commandChild, "disabled"));
    itemDataMap.put("partialSubmit", _getCommandChildProperty(commandChild, "partialSubmit"));
    itemDataMap.put("shortDesc", _getCommandChildProperty(commandChild, "shortDesc"));
    itemDataMap.put("styleClass", _getCommandChildProperty(commandChild, "styleClass"));
    itemDataMap.put("targetFrame", _getCommandChildProperty(commandChild, "targetFrame"));
    itemDataMap.put("text", _getCommandChildProperty(commandChild, "text"));
    // Store the row index for this iteration so it can be re-set
    // when rendering
    if (rowIndex >= 0)
      itemDataMap.put("rowIndex", rowIndex);

    navItemData.addItemData(itemDataMap);
  }

  protected boolean getBooleanFromProperty(
    Object value)
  {
    if (value == null)
    {
      return false;
    }

    return ("true".equals(value.toString()));
  }


  private Object _getCommandChildProperty(
    UIXCommand commandChild,
    String     propertyName)
  {
    FacesBean childFacesBean = commandChild.getFacesBean();
    FacesBean.Type type = childFacesBean.getType();
    PropertyKey propertyKey = type.findKey(propertyName);
    if (propertyKey == null)
    {
      if (_LOG.isSevere())
      {
        _LOG.severe("NAVIGATIONLEVELRENDERER_NOT_FOUND_CHILD_PROPERTY", propertyName);
      }
      return null;
    }
    else
    {
      return childFacesBean.getProperty(propertyKey);
    }
  }

  private void _renderNavigationItem(
    FacesContext        context,
    RenderingContext    rc,
    ResponseWriter      rw,
    Map<String, Object> itemData,
    String              renderingHint,
    boolean             isRtl
    ) throws IOException
  {
    if (_HINT_BAR.equals(renderingHint))
    {
      renderNonOverlappingItem(context, rc, rw, itemData, isRtl, true, false);
    }
    else if (_HINT_BUTTONS.equals(renderingHint))
    {
      renderNonOverlappingItem(context, rc, rw, itemData, isRtl, false, false);
    }
    else if (_HINT_CHOICE.equals(renderingHint))
    {
      _renderChoiceItem(context, rc, rw, itemData);
    }
    else if (_HINT_LIST.equals(renderingHint))
    {
      renderNonOverlappingItem(context, rc, rw, itemData, isRtl, false, true);
    }
    else // _HINT_TABS
    {
      renderTabItem(context, rc, rw, itemData, isRtl);
    }
  }

  protected void writeInlineStyles(
    ResponseWriter rw,
    Object         userInlineStyle,
    String         appendedInlineStyle
    ) throws IOException
  {
    if (userInlineStyle == null)
    {
      rw.writeAttribute("style", appendedInlineStyle, null);
    }
    else
    {
      String userInlineStyleString = toString(userInlineStyle).trim();
      boolean needSemicolon = false;
      int lastIndex = userInlineStyleString.length()-1;
      if ( (lastIndex > 0) && (userInlineStyleString.charAt(lastIndex) != ';') )
      {
        needSemicolon = true;
      }
      StringBuilder itemInlineStyle = new StringBuilder();
      itemInlineStyle.append(userInlineStyleString);
      if (needSemicolon)
      {
        itemInlineStyle.append(";");
      }
      if (appendedInlineStyle != null)
      {
        itemInlineStyle.append(appendedInlineStyle);
      }
      rw.writeAttribute("style", itemInlineStyle.toString(), null);
    }
  }

  private void _writeInlineTbodyStyles(
    RenderingContext rc,
    ResponseWriter   rw
    ) throws IOException
  {
    // IE and Firefox require the TABLE to be "inline".
    if (rc.getAgent().getAgentName() == Agent.AGENT_GECKO)
    {
      // Firefox 1.5 also requires the TBODY to be "inline":
      rw.writeAttribute("style", "display: inline;", null);
    }
  }

  protected void appendIconAndText(
    FacesContext        context,
    RenderingContext    rc,
    ResponseWriter      rw,
    String              iconUri,
    Map<String, Object> itemData,
    boolean             isDisabled,
    boolean             isRtl
    ) throws IOException
  {
    if ( (iconUri != null) && !isRtl )
    {
      _appendIcon(context, rw, iconUri, isRtl, rc);
    }
     _writeItemLink(context, rc, rw, itemData, isDisabled);
    if ( (iconUri != null) && isRtl )
    {
      _appendIcon(context, rw, iconUri, isRtl, rc);
    }
  }

  private void _appendIcon(
    FacesContext     context,
    ResponseWriter   rw,
    String           iconUri,
    boolean          isRtl,
    RenderingContext rc
    ) throws IOException
  {
    String styleAppender = "";
    rw.startElement("img", null);
    rw.writeAttribute("border", "0", null);
    rw.writeAttribute("align", "absmiddle", null);

    if (isPDA(rc))
    {
      if (isRtl)
      {
        rw.writeAttribute("style", "padding-left: 5px;", null);
      }
      else
      {
        rw.writeAttribute("style", "padding-right: 5px;", null);
      }
    }
    else
    {
      if (isRtl)
      {
        rw.writeAttribute("style", "padding-left: 5px; float: right;", null);
      }
      else
      {
        rw.writeAttribute("style", "padding-right: 5px; float: left;", null);
      }
    }

    Application application = context.getApplication();
    ViewHandler handler = application.getViewHandler();
    String resolvedIconUri = handler.getResourceURL(context, iconUri);
    renderEncodedResourceURI(context, "src", resolvedIconUri);
    rw.endElement("img");
  }

  @SuppressWarnings("unchecked")
  private void _renderCommandChildren(
    FacesContext context,
    UIXCommand   uiComp
    ) throws IOException
  {
    for(UIComponent child : (List<UIComponent>)uiComp.getChildren())
    {
      RenderUtils.encodeRecursive(context, child);
    }
  }

  private void _renderText(
    ResponseWriter      rw,
    Map<String, Object> itemData
    ) throws IOException
  {
    String text = toString(itemData.get("text"));
    if(text != null)
    {
      rw.write(text);
    }
  }

  private void _writeItemLink(
    FacesContext        context,
    RenderingContext    rc,
    ResponseWriter      rw,
    Map<String, Object> itemData,
    boolean             isDisabled
    ) throws IOException
  {

    UIXCommand commandChild = (UIXCommand)itemData.get("component");
    if (isDisabled)
    {
      _renderText(rw, itemData);
      _renderCommandChildren(context, commandChild);
      return;
    }

    String destination = toResourceUri(context, (itemData.get("destination")));
    boolean immediate = false;
    boolean partialSubmit = false;
    if (destination == null)
    {
      immediate = getBooleanFromProperty(itemData.get("immediate"));
      partialSubmit = getBooleanFromProperty(itemData.get("partialSubmit"));
      if (partialSubmit)
      {
        AutoSubmitUtils.writeDependencies(context, rc);
      }
      String clientId = commandChild.getClientId(context);
      // Make sure we don't have anything to save
      assert(rc.getCurrentClientId() == null);
      rc.setCurrentClientId(clientId);

      // Find the params up front, and save them off -
      // getOnClick() doesn't have access to the UIComponent
      String extraParams = AutoSubmitUtils.getParameters(commandChild);
      rc.getProperties().put(_EXTRA_SUBMIT_PARAMS_KEY, extraParams);
    }

    boolean isActive = getBooleanFromProperty(itemData.get("isActive"));
    boolean isDesktop = (rc.getAgent().getType().equals(Agent.TYPE_DESKTOP));

    boolean nonJavaScriptSubmit = (!supportsScripting(rc))
                                             && (destination == null);

    // For non-javascript browsers, we need to render a submit element
    // instead of an anchor tag if the anchor tag doesn't have a destination

    if (nonJavaScriptSubmit)
    {

      rw.startElement("input", commandChild);

      rw.writeAttribute("type", "submit", null);
      rw.writeAttribute("value", toString(itemData.get("text")), "text");

      String clientId = getClientId(context, commandChild);
      rw.writeAttribute("id", clientId, "id");



      // For Non-JavaScript browsers, encode the name attribute with the
      // parameter name and value thus it would enable the browsers to
      // include the name of this element in its payLoad if it submits the
      // page.

      rw.writeAttribute("name", XhtmlUtils.getEncodedParameter
                                  (XhtmlConstants.SOURCE_PARAM)
                                   + clientId, null);
      renderStyleClass(context, rc,
                  SkinSelectors.AF_COMMAND_BUTTON_STYLE_CLASS);
      String linkConverter =
             "border:none;background:inherit;text-decoration:underline;";

      // Few mobile browsers couldn't apply css property for active elements
      // so making it inline
      if (isActive && !isDesktop)
      {
        linkConverter = linkConverter + "font-weight: bold;";
      }

      writeInlineStyles(rw, null,linkConverter);

    }
    else
    {
      rw.startElement("a", commandChild); // linkElement

      // Few mobile browsers couldn't apply css property for active elements
      // so making it inline
      if (isActive && !isDesktop)
      {
        writeInlineStyles(rw, null,"font-weight: bold;");
      }
      _renderCommandChildId(context, commandChild);

      if (destination == null)
      {
        rw.writeURIAttribute("href", "#", null); // required for IE to support ":hover" styles
      }
      else
      {
        renderEncodedActionURI(context, "href", destination);
        String targetFrame = toString(itemData.get("targetFrame"));
        if ( (targetFrame != null) && !Boolean.FALSE.equals(
          rc.getAgent().getCapabilities().get(TrinidadAgent.CAP_TARGET)) )
        {
          rw.writeAttribute("target", targetFrame, null);
        }
      }



      // Cannot use super.renderEventHandlers(context, bean); because the wrong
      // property keys would be in use so must do it this way:
      // Also render the events only if the browser supports JavaScript
      if (supportsScripting(rc))
      {
        _writeOnclickProperty(
          rc,
          rw,
          commandChild,
          (destination == null),
          immediate,
          partialSubmit); // special for actions!
        _writeCommandChildProperty(rw, commandChild, "ondblclick");
        _writeCommandChildProperty(rw, commandChild, "onkeydown");
        _writeCommandChildProperty(rw, commandChild, "onkeyup");
        _writeCommandChildProperty(rw, commandChild, "onkeypress");
        _writeCommandChildProperty(rw, commandChild, "onmousedown");
        _writeCommandChildProperty(rw, commandChild, "onmousemove");
        _writeCommandChildProperty(rw, commandChild, "onmouseout");
        _writeCommandChildProperty(rw, commandChild, "onmouseover");
        _writeCommandChildProperty(rw, commandChild, "onmouseup");
      }
    }

    String accessKey = toString(itemData.get("accessKey"));
    if ( !isDisabled && (accessKey != null) )
    {
      rw.writeAttribute("accessKey", accessKey, null);
    }

    // In the case of HTML basic browsers, we render an input element. Hence,
    // we cannot render any children, so skip calling _renderCommandChildren
    if (nonJavaScriptSubmit)
    {
      rw.endElement("input");
    }
    else
    {
      _renderText(rw, itemData);
      _renderCommandChildren(context, commandChild);
      rw.endElement("a"); // linkElement
    }

    if (destination == null)
    {
      rc.setCurrentClientId(null);
      rc.getProperties().remove(_EXTRA_SUBMIT_PARAMS_KEY);
    }
  }

  private void _writeCommandChildProperty(
    ResponseWriter rw,
    UIXCommand     commandChild,
    String         propertyName
    ) throws IOException
  {
    rw.writeAttribute(
      propertyName,
      _getCommandChildProperty(commandChild, propertyName),
      propertyName);
  }

  private void _writeOnclickProperty(
    RenderingContext rc,
    ResponseWriter   rw,
    UIXCommand       commandChild,
    boolean          actionSpecialCase,
    boolean          immediate,
    boolean          partialSubmit
    ) throws IOException
  {
    if (actionSpecialCase)
    {
      String onclick = (String)_getCommandChildProperty(commandChild, "onclick");
      String script = _getAutoSubmitScript(rc, immediate, partialSubmit);
      rw.writeAttribute(
        "onclick", XhtmlUtils.getChainedJS(onclick, script, true), "onclick");
    }
    else // simple case, e.g. (destination != null)
    {
      _writeCommandChildProperty(rw, commandChild, "onclick");
    }
  }

  private String _getAutoSubmitScript(
    RenderingContext rc,
    boolean          immediate,
    boolean          partialSubmit
    )
  {
    String id = rc.getCurrentClientId();

    String extraParams = (String)
      rc.getProperties().get(_EXTRA_SUBMIT_PARAMS_KEY);

    String script;
    if (partialSubmit)
    {
      script = AutoSubmitUtils.getSubmitScript(
        rc, id, immediate, false,
        null/* no event*/,
        extraParams,
        false);
    }
    else
    {
      script = AutoSubmitUtils.getFullPageSubmitScript(
        rc, id, immediate,
        null/*no event*/,
        extraParams,
        false/* return false*/);
    }
    return script;
  }

  protected void renderNonOverlappingItem(
    FacesContext        context,
    RenderingContext    rc,
    ResponseWriter      rw,
    Map<String, Object> itemData,
    boolean             isRtl,
    boolean             isBar,
    boolean             isList
    ) throws IOException
  {
    rw.startElement("table", null);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
    String appendedStyle = null;
    if (!isList)
    {
      appendedStyle = "display: inline;"; // style to make the table inline

      // In Safari and webkit browsers display:inline doesn't work as expected, and
      // display:inline-block need to be used to make the table inline.
      // NokiaS60 has a webkit based browser
      if (rc.getAgent().getAgentName() == Agent.AGENT_WEBKIT || isNokiaS60(rc))
      {
        appendedStyle = "display: inline-block;";
      }
    }
    writeInlineStyles(rw, toString(itemData.get("inlineStyle")),
      appendedStyle); // user's style + what we must have on top of it
    rw.writeAttribute("title", itemData.get("shortDesc"), null);
    StringBuilder itemStyleClass = new StringBuilder();
    String userStyleClass = toString(itemData.get("styleClass"));
    if (userStyleClass != null)
    {
      itemStyleClass.append(userStyleClass);
      itemStyleClass.append(" "); // more style classes are appended below
    }

    // Assign the event handlers:
    boolean isDisabled = getBooleanFromProperty(itemData.get("isDisabled"));
    boolean isActive = getBooleanFromProperty(itemData.get("isActive"));
    if (isActive)
    {
      if (isDisabled)
      {
        if (isList)
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_LIST_ACTIVE_DISABLED_STYLE_CLASS);
        }
        else if (isBar)
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_BAR_ACTIVE_DISABLED_STYLE_CLASS);
        }
        else
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_ACTIVE_DISABLED_STYLE_CLASS);
        }
      }
      else
      {
        if (isList)
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_LIST_ACTIVE_ENABLED_STYLE_CLASS);
        }
        else if (isBar)
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_BAR_ACTIVE_ENABLED_STYLE_CLASS);
        }
        else
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_ACTIVE_ENABLED_STYLE_CLASS);
        }
      }
    }
    else
    {
      if (isDisabled)
      {
        if (isList)
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_LIST_INACTIVE_DISABLED_STYLE_CLASS);
        }
        else if (isBar)
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_BAR_INACTIVE_DISABLED_STYLE_CLASS);
        }
        else
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_INACTIVE_DISABLED_STYLE_CLASS);
        }
      }
      else
      {
        if (isList)
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_LIST_INACTIVE_ENABLED_STYLE_CLASS);
        }
        else if (isBar)
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_BAR_INACTIVE_ENABLED_STYLE_CLASS);
        }
        else
        {
          itemStyleClass.append(
            SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_INACTIVE_ENABLED_STYLE_CLASS);
        }
      }
    }
    renderStyleClass(context, rc, itemStyleClass.toString());
    rw.startElement("tbody", null);
    if (!isList)
    {
      _writeInlineTbodyStyles(rc, rw);
    }
    rw.startElement("tr", null);

    if (isList)
    {
      rw.startElement("td", null); // bulletCell
      renderStyleClass(
        context,
        rc,
        SkinSelectors.AF_NAVIGATION_LEVEL_LIST_BULLET_STYLE_CLASS);
      rw.startElement("div", null); // bulletContent
      rw.write(" ");
      rw.endElement("div"); // bulletContent
      rw.endElement("td"); // bulletCell
    }

    rw.startElement("td", null); // centerCell
    rw.startElement("div", null); // centerContent
    if (isList)
    {
      renderStyleClass(context, rc,
        SkinSelectors.AF_NAVIGATION_LEVEL_LIST_CONTENT_STYLE_CLASS);
    }
    else if (isBar)
    {
      renderStyleClass(context, rc,
        SkinSelectors.AF_NAVIGATION_LEVEL_BAR_CONTENT_STYLE_CLASS);
    }
    else
    {
      renderStyleClass(context, rc,
        SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_CONTENT_STYLE_CLASS);
    }
    appendIconAndText(
      context,
      rc,
      rw,
      toString(itemData.get("icon")),
      itemData,
      isDisabled,
      isRtl);
    rw.endElement("div"); // centerContent
    rw.endElement("td"); // centerCell

    if ( !isList && !getBooleanFromProperty(itemData.get("isLast")) )
    {
      rw.startElement("td", null); // rightCell
      rw.startElement("div", null); // rightContent
      if (isBar)
      {
        renderStyleClass(context, rc,
          SkinSelectors.AF_NAVIGATION_LEVEL_BAR_SEPARATOR_STYLE_CLASS);
      }
      else
      {
        renderStyleClass(context, rc,
          SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_SEPARATOR_STYLE_CLASS);
      }
      rw.write("|");
      rw.endElement("div"); // rightContent
      rw.endElement("td"); // rightCell
    }

    rw.endElement("tr");
    rw.endElement("tbody");
    rw.endElement("table");
  }

  /**
   * encodes non command children of navigationPane.
   * This is used only for hint="bar" and hint="buttons"
   * @param index
   * @param context
   * @param rc
   * @param child
   * @param isLastItem
   * @param hint
   * @throws IOException
   */
  protected void renderNonCommandChild(
    int              index,
    FacesContext     context,
    RenderingContext rc,
    UIComponent      child,
    boolean          isLastItem,
    String           hint
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("table", null);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
    String appendedStyle = null;
    appendedStyle = "display: inline;"; // style to make the table inline

    // In Safari and webkit browsers display:inline doesn't work as expected, and
    // display:inline-block need to be used to make the table inline.
    // NokiaS60 has a webkit based browser
    if (rc.getAgent().getAgentName() == Agent.AGENT_WEBKIT || isNokiaS60(rc))
    {
      appendedStyle = "display: inline-block;";
    }
    writeInlineStyles(rw, null, appendedStyle);
    rw.startElement("tbody", null);
    _writeInlineTbodyStyles(rc, rw);
    rw.startElement("tr", null);
    rw.startElement("td", null);
    rw.startElement("div", null);
    encodeChild(context, child);
    rw.endElement("div");
    rw.endElement("td");
    if(!isLastItem)
    {
      rw.startElement("td", null); // rightCell
      rw.startElement("div", null); // rightContent
      if (hint == _HINT_BAR)
      {
        renderStyleClass(context, rc,
          SkinSelectors.AF_NAVIGATION_LEVEL_BAR_SEPARATOR_STYLE_CLASS);
      }
      else
      {
        renderStyleClass(context, rc,
          SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_SEPARATOR_STYLE_CLASS);
      }

      rw.write("|");
      rw.endElement("div"); // rightContent
      rw.endElement("td"); // rightCell
    }
    rw.endElement("tr");
    rw.endElement("tbody");
    rw.endElement("table");
  }

  private void _renderChoiceItem(
    FacesContext        context,
    RenderingContext    rc,
    ResponseWriter      rw,
    Map<String, Object> itemData
    ) throws IOException
  {
    // Choice items do not support icons at this time.
    boolean isDisabled = getBooleanFromProperty(itemData.get("isDisabled"));
    // If the agent, doesn't support disabled options, don't render anything
    // for such options
    if ( !isDisabled ||
         Boolean.TRUE.equals(rc.getAgent().getCapabilities().get(
                TrinidadAgent.CAP_SUPPORTS_DISABLED_OPTIONS)))
    {
      boolean isActive = getBooleanFromProperty(itemData.get("isActive"));
      UIXCommand commandChild = null;
      String destination = null;
      boolean partialSubmit = false;
      if (!isDisabled)
      {
         // Write the script to evaluate once the item is selected
         commandChild = (UIXCommand)itemData.get("component");
         destination = toString(itemData.get("destination"));
         if (destination == null)
         {
           partialSubmit = getBooleanFromProperty(itemData.get("partialSubmit"));
           if (partialSubmit)
           {
             AutoSubmitUtils.writeDependencies(context, rc);
           }
         }
      }
      rw.startElement("option", null);
      if (isActive)
      {
        rw.writeAttribute("selected", Boolean.TRUE, null);
      }
      rw.writeAttribute("disabled", isDisabled, null);
      rw.writeAttribute("title", itemData.get("shortDesc"), null);

      if (!isDisabled)
      {
        boolean immediate = false;
        if (destination == null)
        {
          immediate = getBooleanFromProperty(itemData.get("immediate"));
          String clientId = commandChild.getClientId(context);
          // Make sure we don't have anything to save
          assert(rc.getCurrentClientId() == null);
          rc.setCurrentClientId(clientId);

          // Find the params up front, and save them off -
          // getOnClick() doesn't have access to the UIComponent
          String extraParams = AutoSubmitUtils.getParameters(commandChild);
          rc.getProperties().put(_EXTRA_SUBMIT_PARAMS_KEY, extraParams);
        }
        _renderCommandChildId(context, commandChild);
        String selectionScript;

        // For Non-javaScript browsers, set the value attribute to the id of
        // the element instead of a javascript code

        if (!supportsScripting(rc))
        {
          selectionScript = rc.getCurrentClientId();
        }
        else if (destination == null)
        {
          selectionScript = _getAutoSubmitScript(rc, immediate, partialSubmit);

          // Trim off the "return false;" because we will be performing an
          // "eval" on the script and a "return" will yield a runtime error:
          selectionScript =
            selectionScript.substring(0, selectionScript.length() - 13);
        }
        else
        {
          String encodedDestination =
            context.getExternalContext().encodeActionURL(destination);
          String targetFrame = toString(itemData.get("targetFrame"));
          StringBuilder sb = new StringBuilder();
          if ( (targetFrame != null) && !Boolean.FALSE.equals(
            rc.getAgent().getCapabilities().get(TrinidadAgent.CAP_TARGET)) )
          {
            sb.append("window.open('");
            sb.append(encodedDestination);
            sb.append("','");
            sb.append(targetFrame);
            sb.append("');");
          }
          else
          {
            sb.append("self.location='");
            sb.append(encodedDestination);
            sb.append("';");
          }
          selectionScript = sb.toString();
        }
        rw.writeAttribute("value", selectionScript, null);

        if (destination == null)
        {
          rc.setCurrentClientId(null);
          rc.getProperties().remove(_EXTRA_SUBMIT_PARAMS_KEY);
        }
      }

      _renderText(rw, itemData);
      rw.endElement("option");
    }
  }

  private void _renderChoiceLabel(
    FacesContext     context,
    RenderingContext rc,
    ResponseWriter   rw,
    boolean          isRtl,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String chooseText = getTitle(component, bean);
    if ( (chooseText == null) || (chooseText.length() == 0) )
    {
      chooseText = getShortDesc(component, bean);
    }

    if ( (chooseText != null) && (chooseText.length() != 0) )
    {
      if (isRtl)
      {
        _renderSpace(rw);
      }
      rw.startElement("span", null);
      renderStyleClass(context, rc,
        SkinSelectors.AF_NAVIGATION_LEVEL_CHOICE_LABEL_STYLE_CLASS);
      rw.write(chooseText);
      rw.endElement("span");
      if (!isRtl)
      {
        _renderSpace(rw);
      }
    }
  }

  private void _renderChoiceButton(
    FacesContext     context,
    RenderingContext rc,
    ResponseWriter   rw,
    boolean          isRtl,
    String           choiceSelectId
    ) throws IOException
  {
    if (!isRtl)
    {
      _renderSpace(rw);
    }
    //The button html element is not supported on all browsers;  use "input"
    //if it is not
    boolean useButtonTag = supportsAdvancedForms(rc);
    String element = useButtonTag ? "button" : "input";

    rw.startElement(element, null);
    renderStyleClass(context, rc,
      SkinSelectors.AF_NAVIGATION_LEVEL_CHOICE_BUTTON_STYLE_CLASS);
    String goText = rc.getSkin().getTranslatedString(
      rc.getLocaleContext(),
      _GO_BUTTON_LABEL_KEY);

    rw.writeAttribute("type", useButtonTag ? "button"  : "submit", null);

    // For Non-JavaScript browsers, encode the name attribute with the
    // parameter name and value thus it would enable the browsers to
    // include the name of this element in its payLoad if it submits
    // the page.

    if (!supportsScripting(rc) )
    {

      String nameAttri = XhtmlUtils.getEncodedParameter
                                     (XhtmlConstants.MULTIPLE_VALUE_PARAM)
                                     + choiceSelectId;

      rw.writeAttribute("name", nameAttri, null);

    }
    else
    {
      // The onclick handler will evaluate the value of the selected option:
      rw.writeAttribute(
        "onclick",
        "var navLevelSelect = document.getElementById('" +
          choiceSelectId +
          "'); eval(navLevelSelect.options[navLevelSelect.selectedIndex].value); return false;",
        null);
    }

    if (useButtonTag)
    {
      rw.write(goText);
    }
    else
    {
      rw.writeAttribute("value", goText, "text");
    }

    rw.endElement(element);

    if (isRtl)
    {
      _renderSpace(rw);
    }
  }

  private void _renderSpace(
    ResponseWriter rw
    ) throws IOException
  {
    rw.startElement("span", null);
    rw.writeAttribute("style", "width: 5px;", null);
    rw.write(" ");
    rw.endElement("span");
  }

  protected void renderTabItem(
    FacesContext        context,
    RenderingContext    rc,
    ResponseWriter      rw,
    Map<String, Object> itemData,
    boolean             isRtl
    ) throws IOException
  {
    rw.startElement("table", null);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);

    String appendedStyle = "display: inline;";

    // In Safari and webkit browsers display:inline doesn't work as expected, and
    // display:inline-block need to be used to make the table inline.
    // NokiaS60 has a webkit based browser
    if (rc.getAgent().getAgentName() == Agent.AGENT_WEBKIT || isNokiaS60(rc))
    {
      appendedStyle = "display: inline-block;";
    }
    writeInlineStyles(rw, toString(itemData.get("inlineStyle")),
      appendedStyle); // user's style + what we must have on top of it
    rw.writeAttribute("title", itemData.get("shortDesc"), null);
    StringBuilder itemStyleClass = new StringBuilder();
    String userStyleClass = toString(itemData.get("styleClass"));
    if (userStyleClass != null)
    {
      itemStyleClass.append(userStyleClass);
      itemStyleClass.append(" "); // more style classes are appended below
    }

    // Assign the event handlers:
    boolean isDisabled = getBooleanFromProperty(itemData.get("isDisabled"));
    boolean isActive = getBooleanFromProperty(itemData.get("isActive"));
    String sectionStyleClass1;
    String sectionStyleClass2 = null;
    if (isActive)
    {
      if (isDisabled)
      {
        sectionStyleClass2 = SkinSelectors.P_AF_DISABLED;
      }
      sectionStyleClass1 =
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_ACTIVE_STYLE_CLASS;
    }
    else
    {
      if (isDisabled)
      {
        sectionStyleClass2 = SkinSelectors.P_AF_DISABLED;
      }
      sectionStyleClass1 =
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_INACTIVE_STYLE_CLASS;
    }
    renderStyleClass(context, rc, itemStyleClass.toString());
    rw.startElement("tbody", null);
    _writeInlineTbodyStyles(rc, rw);
    rw.startElement("tr", null);

    boolean isFirst = getBooleanFromProperty(itemData.get("isFirst"));
    boolean isLast = getBooleanFromProperty(itemData.get("isLast"));
    boolean previousActive = getBooleanFromProperty(itemData.get("previousActive"));

    // start portion of tab:
    if (isFirst)
    {
      // first start
      rw.startElement("td", null);
      _renderTabSection(
        context,
        rc,
        rw,
        sectionStyleClass1,
        sectionStyleClass2,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_START_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_START_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_START_CONTENT_STYLE_CLASS,
        null,
        isDisabled,
        isRtl);
      rw.endElement("td");
    }
    else if (previousActive)
    {
      // start-join-selected-to-deselected
      rw.startElement("td", null);
      _renderTabSection(
        context,
        rc,
        rw,
        sectionStyleClass1,
        sectionStyleClass2,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_START_JOIN_FROM_ACTIVE_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_MID_CONTENT_STYLE_CLASS,
        null,
        isDisabled,
        isRtl);
      rw.endElement("td");
    }
    else if (isActive)
    {
      // start-join-deselected-to-selected
      rw.startElement("td", null);
      _renderTabSection(
        context,
        rc,
        rw,
        sectionStyleClass1,
        sectionStyleClass2,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_START_JOIN_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_START_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_START_CONTENT_STYLE_CLASS,
        null,
        isDisabled,
        isRtl);
      rw.endElement("td");
    }
    else
    {
      // start-join-deselected-to-deselected
      rw.startElement("td", null);
      _renderTabSection(
        context,
        rc,
        rw,
        sectionStyleClass1,
        sectionStyleClass2,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_START_JOIN_FROM_INACTIVE_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_MID_CONTENT_STYLE_CLASS,
        null,
        isDisabled,
        isRtl);
      rw.endElement("td");
    }

    // mid portion of tab:
    rw.startElement("td", null);
    _renderTabSection(
      context,
      rc,
      rw,
      sectionStyleClass1,
      sectionStyleClass2,
      SkinSelectors.AF_NAVIGATION_LEVEL_TABS_MID_STYLE_CLASS,
      SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_MID_STYLE_CLASS,
      SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_MID_CONTENT_STYLE_CLASS,
      itemData,
      isDisabled,
      isRtl);
    rw.endElement("td");

    if (isLast)
    {
      // last end
      rw.startElement("td", null);
      _renderTabSection(
        context,
        rc,
        rw,
        sectionStyleClass1,
        sectionStyleClass2,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_END_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_CONTENT_STYLE_CLASS,
        null,
        isDisabled,
        isRtl);
      rw.endElement("td");
    }
    else if ( isActive ||  (!getBooleanFromProperty(itemData.get("nextActive"))) )
    {
      // end-join-selected-to-deselected or end-join-deselected-to-deselected
      rw.startElement("td", null);
      _renderTabSection(
        context,
        rc,
        rw,
        sectionStyleClass1,
        sectionStyleClass2,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_END_JOIN_TO_INACTIVE_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_JOIN_STYLE_CLASS,
        SkinSelectors.AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_CONTENT_STYLE_CLASS,
        null,
        isDisabled,
        isRtl);
      rw.endElement("td");
    }

    rw.endElement("tr");
    rw.endElement("tbody");
    rw.endElement("table");
  }

  private void _renderTabSection(
    FacesContext        context,
    RenderingContext    rc,
    ResponseWriter      rw,
    String              sectionStyleClass1,
    String              sectionStyleClass2,
    String              topStyleClass,
    String              bottomStyleClass,
    String              bottomContentStyleClass,
    Map<String, Object> itemData,
    boolean             isDisabled,
    boolean             isRtl
    ) throws IOException
  {
    rw.startElement("table", null);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
    if (sectionStyleClass2 == null)
    {
      renderStyleClass(context, rc, sectionStyleClass1);
    }
    else
    {
      String[] sectionStyleClasses = { sectionStyleClass1, sectionStyleClass2 };
      renderStyleClasses(context, rc, sectionStyleClasses);
    }
    rw.startElement("tbody", null);
    rw.startElement("tr", null);
    rw.startElement("td", null);
    renderStyleClass(context, rc, topStyleClass);
    if (itemData != null)
    {
      appendIconAndText(
        context,
        rc,
        rw,
        toString(itemData.get("icon")),
        itemData,
        isDisabled,
        isRtl);
    }
    rw.endElement("td");
    rw.endElement("tr");
    rw.startElement("tr", null);
    rw.startElement("td", null);
    renderStyleClass(context, rc, bottomStyleClass);
    if (bottomContentStyleClass != null)
    {
      rw.startElement("div", null);
      renderStyleClass(context, rc, bottomContentStyleClass);
      rw.endElement("div");
    }
    rw.endElement("td");
    rw.endElement("tr");
    rw.endElement("tbody");
    rw.endElement("table");
  }


  // sets the currency to the row or container that we want to start rendering
  // from.  (COPIED FROM HierarchyUtils, which is package-private)
  static private boolean _setStartDepthPath(
    UIXHierarchy component,
    int          startDepth
  )
  {
    boolean isNewPath = false;
    Object focusKey = component.getFocusRowKey();

    if (focusKey != null )
    {
      List<Object> focusPath = component.getAllAncestorContainerRowKeys(focusKey);
      focusPath = new ArrayList<Object>(focusPath);
      focusPath.add(focusKey);
      int focusSize =  focusPath.size();
      if ( focusSize > startDepth )
      {
        isNewPath = true;
        component.setRowKey(focusPath.get(startDepth));
      }
      else if ( focusSize == startDepth )
      {
        isNewPath = true;
        component.setRowKey(focusKey);
        component.enterContainer();
      }
    }
    else
    {
      if (startDepth  == 0)
      {
        isNewPath = true;
        component.setRowKey(null);
      }
    }

    return isNewPath;
  }

  /**
   * Check if a component is on a focus path
   */
  private static boolean _isOnFocusPath(
    UIXHierarchy component)
  {
    boolean isOnFocusPath = false;
    Object focusKey = component.getFocusRowKey();
    Object currentRowKey = component.getRowKey();
    if (focusKey != null)
    {
      List<Object> focusPath =
                component.getAllAncestorContainerRowKeys(focusKey);
      focusPath = new ArrayList<Object>(focusPath);
      focusPath.add(focusKey);
      int focusSize = focusPath.size();
      for (int i = 0; i < focusSize; i++)
      {
        Object rowKey = focusPath.get(i);
        if (rowKey.equals(currentRowKey))
        {
          isOnFocusPath = true;
          break;
        }
      }
    }

    return isOnFocusPath;
  }

  static private class NavItemData
  {
    NavItemData()
    {
      _list = new ArrayList<Map<String, Object>>();
      _effectiveActiveIndex = -1;
    }

    int getEffectiveActiveIndex()
    {
      return _effectiveActiveIndex;
    }

    void setEffectiveActiveIndex(int effectiveActiveIndex)
    {
      _effectiveActiveIndex = effectiveActiveIndex;
    }

    int getItemCount()
    {
      return _list.size();
    }

    void addItemData(Map<String, Object> itemData)
    {
      _list.add(itemData);
    }

    Map<String, Object> getItemData(int index)
    {
      return _list.get(index);
    }

    private List<Map<String, Object>> _list;
    private int _effectiveActiveIndex;
  }

  private PropertyKey _hintKey;
  private PropertyKey _titleKey;
  private PropertyKey _disabledKey;

  static private final String _HINT_TABS = "tabs";
  static private final String _HINT_BAR = "bar";
  static private final String _HINT_LIST = "list";
  static private final String _HINT_BUTTONS = "buttons";
  static private final String _HINT_CHOICE = "choice";
  static private final String _HINT_DEFAULT = NavigationPaneRenderer._HINT_TABS;

  private static final String _CHOICE_SELECT_ID_SUFFIX =
    "_af_choice_select";
  private static final String _GO_BUTTON_LABEL_KEY =
    "af_menuChoice.GO";

  static private final Object _EXTRA_SUBMIT_PARAMS_KEY = new Object();

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(NavigationPaneRenderer.class);
}
