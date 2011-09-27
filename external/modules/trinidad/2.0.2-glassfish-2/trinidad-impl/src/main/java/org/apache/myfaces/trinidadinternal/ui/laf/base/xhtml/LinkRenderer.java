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

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;
import org.apache.myfaces.trinidadinternal.ui.action.FireAction;
import org.apache.myfaces.trinidadinternal.ui.collection.Parameter;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/LinkRenderer.java#1 $) $Date: 11-nov-2005.14:59:39 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class LinkRenderer extends XhtmlLafRenderer
{
  @Override
  public void render(UIXRenderingContext context,
                     UINode node) throws IOException
  {

    if (LinkDataObject.__isDataObjectUsedMode(context))
    {
      _buildDataObject(context, node);
    }
    else
    {
      super.render(context, node);
    }
  }

  private String _addParams(
    UIXRenderingContext context,
    UINode           node,
    ClientAction     action,
    String           dest
    ) throws IOException
  {
    String rv = dest;
    if (action != null)
    {
      Parameter[] params = action.getParameters(context, node);
      if (params != null)
      {
        rv = ClientActionUtils.appendURLParameters(context, dest, params);
      }
    }
    return rv;
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    if (!isDisabled(context, node) &&
        supportsNavigation(context))
    {
      String destination = getDestination(context, node);

      if (destination != null)
      {
        ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                       node);
        if ((action != null) && (!action.renderAsEvent(context, node)))
        {
          // The action says it can be rendered as a simple href, so we build
          // up a full destination by appending the parameters to the current
          // destination.

          // If we've got a ClientAction and destination is "#", then
          // we are using the action's script as our onClick handler.
          // No point in tacking on parameters to the "#" destination.
          if (!"#".equals(destination))
            destination = _addParams(context, node, action, destination);
        }
      }

      Object id = getID(context, node);

      if (_isAnchor(destination, id))
      {
        // render the destination of the link
        renderDestination(context, node, destination);

        if (supportsAccessKeys(context))
        {
          renderAttribute(context, node, "accesskey", ACCESS_KEY_ATTR);
        }
      }
    }
  }

  /**
   * @see #setDisabled(UIXRenderingContext,boolean)
   * @todo gcrawford - for now readonly maps to disabled
   */
  @Override
  protected boolean isDisabled(
    UIXRenderingContext context,
    UINode           node
    )
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    boolean isDisabled = ((CoreRenderingContext) arc).isLinkDisabled();
    boolean disabledAttr = XhtmlLafUtils.getLocalBooleanAttribute(context,
                                               node,
                                               DISABLED_ATTR,
                                               false);
    boolean readOnlyAttr = XhtmlLafUtils.getLocalBooleanAttribute(context,
                                               node,
                                               READ_ONLY_ATTR,
                                               false);
    return (isDisabled )
      ? true
      : (disabledAttr || readOnlyAttr);
  }


  /**
   * links can be disabled in two ways. You can set the disabled attribute to
   * true, or you can call this method.
   * @param isDisabled if true, disables all subsequent links, regardless
   * of their disabled attributes.
   * @see #isDisabled(UIXRenderingContext,UINode)
   */
  public static void setDisabled(UIXRenderingContext context, boolean isDisabled)
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    ((CoreRenderingContext) arc).setLinkDisabled(isDisabled);
  }

  public static void setSaveModelDisabled(UIXRenderingContext context, boolean isDisabled)
  {
    /* =-=AEW Save model not available in Trinidad
    if (BodyRenderer.__isSaveModelActive(context))
    {
      setRenderingProperty(context, _SAVE_MODEL_DISABLED_KEY,
                           isDisabled ? Boolean.TRUE : null);
    }
    */
  }


  protected boolean isSaveModelDisabled(UIXRenderingContext context)
  {
    return getRenderingProperty(context, _SAVE_MODEL_DISABLED_KEY) != null;
  }

  /**
   * Renders the destination of the link
   */
  protected void renderDestination(
    UIXRenderingContext context,
    UINode           node,
    String           destination
    ) throws IOException
  {
    if (destination != null)
    {
      boolean isJavascript = destination.startsWith("javascript:");
      if (!isJavascript &&
          !destination.startsWith("#") &&
          isSaveModelDisabled(context))
      {
        destination = appendURLArgument(destination, "_noSv", "M");
      }

      if (!isJavascript ||
          supportsScripting(context))
      {
        renderEncodedActionURI(context, "href", destination);
      }
    }

    if (supportsTarget(context))
    {
      renderAttribute(context, "target", getTargetFrame(context, node));
    }
  }


  /**
   * Renders event handlers for the node.
   */
  @Override
  protected void renderEventHandlers(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // turn off all events if we are disabled
    if (!isDisabled(context, node))
    {
      super.renderEventHandlers(context, node);

      renderAttribute(context, node, "onblur",  ON_BLUR_ATTR);
      renderAttribute(context, node, "onfocus", ON_FOCUS_ATTR);
    }
  }

  @Override
  protected void renderID(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object id = getID(context, node);

    if (id != null)
    {
      if (supportsID(context))
      {
        // For links, "name" and thus "id" is a URI attribute.
        renderURIID(context, id);
      }

      if (supportsNameIdentification(context) && makeNameAndIDSame(context))
      {
        renderURIAttribute(context, "name", id);
      }
    }
  }


  /**
   * The ID and the naem are the same for links but not some of the
   * link subclasses
   */
  protected boolean makeNameAndIDSame(
    UIXRenderingContext context
    )
  {
    return true;
  }


  /**
   * Override to return the id and then anme, in that order
   */
  @Override
  protected Object getID(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (makeNameAndIDSame(context))
    {
      return getCachedIDOrName(context, node);
    }
    else
    {
      return XhtmlLafUtils.getLocalAttribute(context, node, ID_ATTR);
    }
  }


  /**
   * Returns the destination to use for the ImageRenderer.
   */
  protected String getDestination(
    UIXRenderingContext context,
    UINode           node
    )
  {
    String destination = getDestinationAttr(context, node);

    // If we have an onclick handler, always provide a destination
    if (destination == null)
    {
      if (supportsIntrinsicEvents(context))
      {
        Object onClick = getOnClick(context, node);

        if (onClick != null)
        {
          destination = "#";
        }
      }
    }
    else
    {
      // If we've got partial targets, there is no need to
      // render the actual destination, so let's avoid bloating
      // the HTML any more than necessary
      if (getAncestorPartialTargets(context) != null)
        destination = "#";
    }

    return destination;
  }

  protected final String getDestinationAttr(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return XhtmlLafUtils.getLocalTextAttribute(context,
                                               node,
                                               DESTINATION_ATTR);
  }

  /**
   * Returns the destination to use for the ImageRenderer.
   */
  @Override
  protected Object getText(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return XhtmlLafUtils.getLocalTextAttribute(context, node, TEXT_ATTR);
  }

  protected Object getTargetFrame(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, TARGET_FRAME_ATTR);
  }


  /**
   * Returns the StyleClass to use to render this node.
   */
  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    String nodeStyleClass = 
      (String)node.getAttributeValue(context, STYLE_CLASS_ATTR);
    if (nodeStyleClass != null)
      return nodeStyleClass;
      
      
    String styleClass = null;

    // We provide a default style class for links, as long
    // as our parent hasn't explicitly disabled this.
    if (!LinkUtils.isDefaultStyleClassDisabled(context))
    {
      styleClass = (isDisabled(context, node)) ? 
                      LINK_DISABLED_STYLE_CLASS :
                      LINK_STYLE_CLASS;
    }

    return styleClass;

  }

  /**
   * Called to render the portion before the contents.  Default
   * implementation does nothing.
   * @param context the rendering context
   * @param node the current UINode
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (isEmpty(context, node))
      return;

    // If we've got a ClientAction, let it write its dependencies
    // before we start rendering the link
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    if (action != null)
      action.writeDependencies(context, node);

    super.prerender(context, node);

    Object styleClass = getStyleClass(context, node);

    if (styleClass == null)
    {

      boolean isDisabled = isDisabled(context, node);

      if ( isDisabled )
      {
        styleClass = DISABLED_STYLE_CLASS;
      }
    }

    Object inlineStyle = getInlineStyle(context, node);

    // determine whether we can use style attributes
    boolean supportsStyleAttributes = supportsStyleAttributes(context);

    if (supportsStyleAttributes)
    {
      renderStyleClassAttribute(context, styleClass);
      renderInlineStyleAttribute(context, inlineStyle);
    }
    else if (supportsTextPresentation(context))
    {
      startRenderingStyleElements(context, null/*=-=AEW inlineStyle*/, styleClass);
    }
  }


  /**
   * Called to render the portion after the contents.  Default
   * implementation does nothing.
   * @param context the rendering context
   * @param node the current UINode
   */
  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    if (isEmpty(context, node))
      return;

    // determine whether we can use style attributes
    boolean supportsStyleAttributes = supportsStyleAttributes(context);

    if (!supportsStyleAttributes && supportsTextPresentation(context))
    {
      XhtmlLafUtils.endRenderingStyleElements(context);
    }

    super.postrender(context, node);
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (isEmpty(context, node))
      return;

    if (!isInaccessibleMode(context))
    {
      _renderAccessibleStatus(context);
    }

    //
    // first render any text from a text attribute. then the children
    //
    if (_isAnchor(context, node))
    {

      renderAccessKeyText(context, node, getText(context, node), 
                          SkinSelectors.AF_LINKACCESSKEY_STYLE_CLASS);
    }
    else
    {
      renderText(context, node);
    }

    // render the children
    super.renderContent(context, node);
  }

  @Override
  protected boolean doRenderStyleAttrs(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // never render the style attributes in renderAttributes
    // we will always handle this is prerender, ourselves
    return false;
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object text       = getText(context, node);
    int    childCount = node.getIndexedChildCount(context);
    Object id         = getID(context, node);

    if (_isEmpty(text, childCount, id))
      return null;

    String destination = getDestination(context, node);
    return (_isAnchor(destination, id))
              ? "a"
              : "span";
  }

  @Override
  protected Object getOnClick(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // We build up the actual onclick handler by chaining the value the
    // ONCLICK_ATTR with the script of the PRIMARY_CLIENT_ACTION_ATTR.
    // Since we get the onclick handler multiple times (getDestination(),
    // which is called multiple times, and renderEventHandlers()), and
    // since building up the handler can be expensive, we store the
    // script in a local property, so that this work does not need to
    // be repeated.
    Object prop = context.getLocalProperty(0,
                                           _LOCAL_ON_CLICK_KEY,
                                           _NONE);
    if (prop != _NONE)
      return prop;

    Object onClick = super.getOnClick(context, node);
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    String actionScript = null;

    // TODO: This is a temporary way of setting blocking. Once the
    // AutoSubmitUtils makes it into the main branch, dump this and pass a
    // parameter to getSubmitScript(), which will tell it to look for and turn
    // on blocking. Similar code in ButtonRenderer also has to change.
    if (action instanceof FireAction)
    {
      UIComponent component;
      component = node.getUIComponent();
      if ((component != null)
          && Boolean.TRUE.equals(component.getAttributes().get("blocking")))
        ((FireAction) action).setBlocking(true);
    }

    if (action != null)
    {
      if (action.renderAsEvent(context, node) &&
          (getDestinationAttr(context, node) == null))
      {
        // We must ignore actionScript if there is a destination or else the
        // destination will never execute because the onclick will run first.
        actionScript = action.getScript(context, node, Boolean.FALSE);
      }
    }
    else
    {
      // If we don't have a ClientAction, check to see if we've got
      // partial targets provided by an ancestor link container.
      actionScript = getPartialChangeScript(context, node);
    }

    Object chainedScript = null;

    if ((onClick != null) || (actionScript != null))
    {
      chainedScript = XhtmlLafUtils.getChainedJS(onClick,
                                                 actionScript,
                                                 true);
    }

    // Store away the script for next time
    context.setLocalProperty(_LOCAL_ON_CLICK_KEY, chainedScript);
    return chainedScript;
  }

  // Checks for partial targets provided by an ancestor link container
  // and returns a script which updates the partial targets.
  protected String getPartialChangeScript(
    UIXRenderingContext context,
    UINode           node
    )
  {
    String partialTargets = getAncestorPartialTargets(context);
    if (partialTargets == null)
      return null;

    // If we've got partial targets, tack them on to the destination.
    //
    String destination = getDestinationAttr(context, node);
    if (destination == null)
      return null;

    URLEncoder encoder = context.getURLEncoder();
    String partialTargetsKey = encoder.encodeParameter(PARTIAL_TARGETS_PARAM);

    // =-=ags Note: We should perform the following append and the
    //        conversion to the _firePartialChange() call all in one
    //        StringBuffer to avoid multiple buffer allocations.


    destination = XhtmlLafUtils.appendURLArgument(destination,
                                                  partialTargetsKey,
                                                  partialTargets);


    // Return a script which calls _firePartialChange() on
    // the new destination.
    return XhtmlLafUtils.getFirePartialChangeHandler(destination);
  }

  // Returns any partial targets provided by ancestor link containers.
  protected static String getAncestorPartialTargets(
    UIXRenderingContext context
    )
  {
    // First check to see if we've got a local copy
    Object prop = context.getLocalProperty(0,
                                           _LOCAL_PARTIAL_TARGETS_KEY,
                                           _NONE);
    if (prop != _NONE)
      return (String)prop;

    // Get the partial targets from the RenderingContext
    String partialTargets = (String)context.getProperty(MARLIN_NAMESPACE,
                    XhtmlLafConstants.LINK_CONTAINER_PARTIAL_TARGETS_PROPERTY);

    context.setLocalProperty(_LOCAL_PARTIAL_TARGETS_KEY, partialTargets);
    return partialTargets;
  }


  /**
   * Returns true if we should use an anchor to represent this link.
   */
  private boolean _isAnchor(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // NB: Must match logic in renderAttributes()!
    return _isAnchor(getDestination(context, node),
                     getID(context, node));
  }

  /**
   * Returns true if we should use an anchor to represent this link.
   */
  private boolean _isAnchor(
    String destination,
    Object id
    )
  {
    // NB: Must match logic in renderAttributes()!
    return ((destination != null) || (id != null));
  }

  /**
   * Return true if this link is empty ... has no children, text,
   * destination, or node name. We render nothing.
   */
  protected boolean isEmpty(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return _isEmpty(getText(context, node),
                    node.getIndexedChildCount(context),
                    getID(context, node));
  }

  /**
   * Return true if this link is empty ... has no children, text,
   * destination, or node name. We render nothing.
   */
  private boolean _isEmpty(
    Object text,
    int    childCount,
    Object id
    )
  {
    return ((text == null) && (childCount <= 0) && (id == null));
  }

  private void _buildDataObject(UIXRenderingContext context,
                                UINode link) throws IOException
  {
    Object id = getID(context, link);
    Object destination = getDestination(context, link);
    Object text = getText(context, link);
    Object onClick = getOnClick(context, link);
    Object target = getTargetFrame(context, link);
    Object shortDesc = getShortDesc(context, link);
    Object accessKey = link.getAttributeValue(context, ACCESS_KEY_ATTR);
    Integer currentIndex = LinkDataObject.__getCurrentIndex(context);

    boolean isSelected =
        Boolean.TRUE.equals(link.getAttributeValue(context, SELECTED_ATTR));


    boolean isDisabled = isDisabled(context, link);
    LinkDataObject dobj = new LinkDataObject(id, text, shortDesc,
                                             destination, onClick,
                                             target, accessKey,
                                             isSelected, isDisabled,
                                             currentIndex, link);

    LinkDataObject.__setDataObject(context, dobj);
  }

  /**
   * This method addresses the accessibilty issue and renders the selection status
   * of link in recognisable fashion.
   * Used for inserting a SPAN tag with p_OraHiddenLabel style class and appropriate
   * text as content which identifies that it is selected,  with in the link
   * thus enhancing the readability.
   * see bug 2980906
   * @param context The rendering context
   * @throws IOException
   */
  private void _renderAccessibleStatus(
    UIXRenderingContext context
    ) throws IOException
  {
    // Do not attempt to render this label if the underlying
    // platform does not support hidden labels
    if (!HiddenLabelUtils.supportsHiddenLabels(context))
      return;

    // The parent renderers set the selected link by getting the
    // resolved index and cheking it with currVisChildIndex of the
    // rendered child. The selection status is set on Context using the
    // LinkUtils.setSelected(RenderingContext,,) method.
    boolean isSelected =  LinkUtils.isSelected(context);

    if (isSelected)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("span", null);
      renderStyleClassAttribute(context,HIDDEN_LABEL_STYLE_CLASS);
      Object spanText = getTranslatedValue(context,_STATUS_SELECTED_KEY);
      writer.writeText(spanText, null);
      writer.endElement("span");
    }
  }


  // key indicating that the save model needs to be disabled
  private static final Object _SAVE_MODEL_DISABLED_KEY = new Object();

  // object indicating that there is no local property
  private static final Object _NONE = new Object();


  // object used to store the local copy of the onClick handler
  private static final Object _LOCAL_ON_CLICK_KEY = new Object();

  // object used to store the local copy of the ancestor partial targets
  private static final Object _LOCAL_PARTIAL_TARGETS_KEY = new Object();

  /**
   * Key is used to get  translated text 'Selected' which is rendered as text
   * in the SPAN which distinguishes the selection.
   */
  private static final String  _STATUS_SELECTED_KEY = "STATUS_SELECTED";

}
