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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import java.io.IOException;

import javax.faces.component.UIComponent;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.agent.CapabilityKey;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.ElementRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.data.DataObject;
import org.apache.myfaces.trinidadinternal.ui.data.DataObjectList;
import org.apache.myfaces.trinidadinternal.ui.data.bean.BeanAdapterUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelExtension;

/**
 * Base Rendering class
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/BaseLafRenderer.java#0 $) $Date: 10-nov-2005.18:52:56 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BaseLafRenderer extends ElementRenderer
        implements UIConstants, BaseLafConstants
{
  protected Object getShortDesc(
          UIXRenderingContext context,
          UINode           node
          )
  {
    return node.getAttributeValue(context, SHORT_DESC_ATTR);
  }


  protected Object getID(
          UIXRenderingContext context,
          UINode           node
          )
  {
    return node.getAttributeValue(context, ID_ATTR);
  }


  protected boolean isDisabled(
          UIXRenderingContext context,
          UINode           node
          )
  {
    Object isDisabled = node.getAttributeValue(context,
            DISABLED_ATTR);

    return (Boolean.TRUE.equals(isDisabled));
  }


  protected void renderURIAttribute(
          UIXRenderingContext context,
          String           name,
          Object           value
          ) throws IOException
  {
    if (value != null)
    {
      context.getResponseWriter().writeURIAttribute(name, value, null);
    }
  }

  protected void renderURIID(
          UIXRenderingContext context,
          Object           idObject
          ) throws IOException
  {
    if (supportsID(context))
    {
      renderURIAttribute( context, "id", idObject);
    }
  }

  /**
   * Renders the id of the UINode
   */
  protected void renderID(
          UIXRenderingContext context,
          UINode           node
          ) throws IOException
  {
    if (supportsID(context))
    {
      Object id = getID( context, node );
      renderAttribute(context, "id", id);
    }
  }

  /**
   * Renders the id of the UINode
   */
  protected final void renderID(
          UIXRenderingContext context,
          Object           idObject,
          boolean          isSubID
          ) throws IOException
  {
    if (!isSubID && supportsID(context))
    {
      renderAttribute(context, "id", idObject);
    }
  }

  /**
   * Get the name for a node. In rare cases, the renderer must specify
   * the name.
   */
  protected Object getNodeName(
          UIXRenderingContext context,
          UINode           node
          )
  {
    return node.getAttributeValue(context, NAME_ATTR);
  }


  /**
   * Returns the name of the node, transformed for the given context
   */
  protected Object getTransformedName(
          UIXRenderingContext context,
          UINode           node
          )
  {
    Object nameObject = getNodeName(context, node);

    if (nameObject != null)
    {
      return nameObject.toString();
    }
    else
    {
      return null;
    }
  }

  protected UINode getNamedChild(
          UIXRenderingContext context,
          UINode           node,
          String           name
          )
  {
    UINode child = node.getNamedChild(context, name);

    if ((child == null) || skipChild(context, node, child))
      return null;

    return child;
  }


  protected boolean hasNamedChild(
          UIXRenderingContext context,
          UINode           node,
          String           name
          )
  {
    return getNamedChild(context, node, name) != null;
  }

  protected boolean hasRenderedNamedChild(
          UIXRenderingContext context,
          UINode           node,
          String           name
          )
  {
    UINode child = getNamedChild(context, node, name);

    if ( child == null ||
            Boolean.FALSE.equals(child.getAttributeValue(context, RENDERED_ATTR )))
      return false;

    return true;
  }

  /**
   * Gets a property stored on the context, using the BLAF namespace.
   */
  protected static Object getRenderingProperty(
          UIXRenderingContext context,
          Object           key
          )
  {
    return context.getProperty(MARLIN_NAMESPACE, key);
  }


  /**
   * Gets a property stored on the context, using the Marlin namespace.
   */
  protected static Object getRenderingProperty(
          UIXRenderingContext context,
          Object           key,
          Object           defaultValue
          )
  {
    return BaseLafUtils.getRenderingProperty(context, key, defaultValue);
  }

  protected static int getRenderingProperty(
          UIXRenderingContext context,
          Object           key,
          int              defaultValue
          )
  {
    Object value = BaseLafUtils.getRenderingProperty(context, key);

    if (value == null)
    {
      return defaultValue;
    }
    else
    {
      return ((Integer)value).intValue();
    }
  }


  /**
   * Stores a property on the context, using the BLAF namespace.
   */
  protected static void setRenderingProperty(
          UIXRenderingContext context,
          Object           key,
          Object           value
          )
  {
    BaseLafUtils.setRenderingProperty(context, key, value);
  }

  /**
   * Returns UIComponent for a given UINode.
   * For case of composites, UIComponents are not directly available with the
   * supplied UINode. In this case goes up in the containment tree, looks up the
   * first encountered UINode for UIComponent, if present returns the same.
   * Returns null if this lookup was unsuccessful.
   */
  public static UIComponent getUIComponent(
    UIXRenderingContext context,
    UINode node
    )
  {
    //  If we find one in the given node, just return right away.
    UIComponent component = node.getUIComponent();
    if (component != null)
      return component;

    // Else look up its ancestors.
    UIXRenderingContext rContext = context.getParentContext();
    while(rContext != null)
    {
      component = rContext.getAncestorNode(0).getUIComponent();
      if (component != null)
        return component;
      rContext  = rContext.getParentContext();
    }
     return null;
  }

  /**
   * Pushes the attribute value into the rendering context property,
   * storing the old value as a local property to be restored later.
   *
   * @see #pushRenderingProperty
   * @see #popRenderingProperty
   */
  protected static void pushAttributeAsRenderingProperty(
          UIXRenderingContext  context,
          UINode            node,
          AttributeKey      attrKey)
  {
    BaseLafUtils.pushAttributeAsRenderingProperty(context, node, attrKey);
  }

  /**
   * Pushes a new value into the rendering context property,
   * storing the old value as a local property to be restored later.
   *
   * @see #popRenderingProperty
   */
  protected static void pushRenderingProperty(
          UIXRenderingContext  context,
          Object            key,
          Object            value)
  {
    BaseLafUtils.pushRenderingProperty(context, key, value);
  }

  /**
   * Pushes a new value into the rendering context property,
   * storing the old value as a local property to be restored later.
   *
   * @see #popRenderingProperty
   */
  protected static void pushRenderingProperty(
          UIXRenderingContext  context,
          Object            key,
          Object            localKey,
          Object            value)
  {
    BaseLafUtils.pushRenderingProperty(context, key, localKey, value);
  }

  /**
   * Pops a previously pushed local value back into the rendering context
   * property, setting the local property value back to null.
   *
   * @see #pushRenderingProperty
   */
  protected static void popRenderingProperty(
          UIXRenderingContext  context,
          Object            key)
  {
    BaseLafUtils.popRenderingProperty(context, key);
  }

  /**
   * Pops a previously pushed local value back into the rendering context
   * property, setting the local property value back to null.
   *
   * @see #pushRenderingProperty
   */
  protected static void popRenderingProperty(
          UIXRenderingContext  context,
          Object            key,
          Object            localKey)
  {
    BaseLafUtils.popRenderingProperty(context, key, localKey);
  }

  /**
   * Returns true if the specified node has the same Marlin name as the
   * name passed in.
   */
  public static boolean isEqualMarlinName(
          UINode node,
          String localName
          )
  {
    return ((node != null)                               &&
            (node.getNamespaceURI() == MARLIN_NAMESPACE) &&
            (localName.equals(node.getLocalName())));
  }

  /**
   * Render the text stored as a text attribute.
   */
  protected final void renderText(
          UIXRenderingContext context,
          UINode           node
          ) throws IOException
  {
    // render the element text here
    Object textValue = node.getAttributeValue(context, TEXT_ATTR);

    if (textValue != null)
    {
      context.getResponseWriter().writeText(textValue, null);
    }
  }


  /**
   * Returns the value of an attribute for this node, using the default value,
   * if no value exists for the attribute on this node.
   */
  protected static Object getAttributeValue(
          UIXRenderingContext context,
          UINode           node,
          AttributeKey     attributeKey,
          Object           defaultValue
          )
  {
    Object value = node.getAttributeValue(context, attributeKey);

    if (value == null)
    {
      value = defaultValue;
    }

    return value;
  }

  protected static boolean getBooleanAttributeValue(
          UIXRenderingContext context,
          UINode           node,
          AttributeKey     attributeKey,
          Boolean          defaultValue
          )
  {
    return Boolean.TRUE.equals(getAttributeValue(context,
            node,
            attributeKey,
            defaultValue));
  }


  protected static boolean getBooleanAttributeValue(
          UIXRenderingContext context,
          UINode           node,
          AttributeKey     attributeKey,
          boolean          defaultValue
          )
  {
    return getBooleanAttributeValue(context,
            node,
            attributeKey,
            (defaultValue)
            ? Boolean.TRUE
            : Boolean.FALSE);
  }


  protected static int getIntAttributeValue(
          UIXRenderingContext context,
          UINode           node,
          AttributeKey     attributeKey,
          int              defaultValue
          )
  {
    Integer value = (Integer)node.getAttributeValue(context, attributeKey);

    if (value != null)
    {
      return value.intValue();
    }
    else
    {
      return defaultValue;
    }
  }

  protected static DataObject getDataObjectAttributeValue(
          UIXRenderingContext context,
          UINode           node,
          AttributeKey     attributeKey)
  {
    Object o = node.getAttributeValue(context, attributeKey);
    return BeanAdapterUtils.getAdapter(context, o);
  }


  protected static DataObjectList getDataObjectListAttributeValue(
          UIXRenderingContext context,
          UINode           node,
          AttributeKey     attributeKey)
  {
    Object o = node.getAttributeValue(context, attributeKey);
    return BeanAdapterUtils.getAdapterList(context, o);
  }

  protected static Integer getInteger(
          int i
          )
  {
    return i;
  }


  protected static Boolean getBoolean(
          boolean flag
          )
  {
    return Boolean.valueOf(flag);
  }

  protected static boolean isRightToLeft(
          UIXRenderingContext context
          )
  {
    return context.getLocaleContext().isRightToLeft();
  }


  /**
   * Returns a translated String from the skin's resource bundle.
   * Logs a severe message if there is a MissingResourceException.
   */ 
  public static String getTranslatedString(
          UIXRenderingContext context,
          String           key
          )
  {
    return context.getTranslatedString(key);
  }

  /**
   * Returns a translated value from the skin's resource bundle.
   * Logs a severe message if there is a MissingResourceException.
   */ 
  public static Object getTranslatedValue(
          UIXRenderingContext context,
          String           key
          )
  {
    return context.getTranslatedValue(key);
  }

  /**
   * Format a string with the specified pattern and parameters, caching
   * the FastMessageFormat on the RenderingContext.
   */
  protected String formatString(
          UIXRenderingContext context,
          String pattern,
          String[] parameters
          )
  {
    return BaseLafUtils.getFormattedString(context, pattern, parameters);
  }

  /**
   * Returns true if this node is selected.
   */
  protected static boolean isSelected(
          UIXRenderingContext context,
          UINode           node
          )
  {
    Object selectedAttr  = node.getAttributeValue(context,
            SELECTED_ATTR);

    if (selectedAttr == null)
    {
      selectedAttr = Boolean.FALSE;

      UINode parent = context.getAncestorNode(1);

      if (parent != null)
      {
        Integer selectedIndex = (Integer)
                parent.getAttributeValue(context,
                        SELECTED_INDEX_ATTR);

        if ((selectedIndex != null) &&
                !_UNSELECTED_INDEX.equals(selectedIndex))
        {
          if (selectedIndex.intValue() ==
                  context.getPath().getElementIndex(-1))
          {
            selectedAttr = Boolean.TRUE;
          }
        }
      }
    }

    return Boolean.TRUE.equals(selectedAttr);
  }


  public static String getParentFormName(
          UIXRenderingContext context
          )
  {
    return BaseLafUtils.getParentFormName(context);
  }

  /**
   * Convenience method to make it easy for subclasses to create
   * AttributeKeys
   */
  protected static AttributeKey getAttributeKey(
          String attrKeyName
          )
  {
    return AttributeKey.getAttributeKey(attrKeyName);
  }


  public static String getBaseImageURI(UIXRenderingContext context)
  {
    // See if we've cached off the URI
    String baseImageURI = (String)context.getProperty(MARLIN_NAMESPACE,
            _BASE_IMAGE_URI_PROPERTY);
    // Nope, have to compute it
    if (baseImageURI == null)
    {
      // We use getSharedConfiguredURL() instead of getConfiguredURL()
      // because we always want to use shared images if they are available.
      baseImageURI = BaseLafUtils.getConfiguredURL(context,
              Configuration.IMAGES_DIRECTORY);
      context.setProperty(MARLIN_NAMESPACE,
              _BASE_IMAGE_URI_PROPERTY,
              baseImageURI);
    }

    // Configurations are always supposed to serve directories
    // ending with a delimiter
    assert (baseImageURI.charAt(baseImageURI.length() - 1) ==
              URI_DELIMITER);

    return baseImageURI;
  }

  public static String getIconURI(
          UIXRenderingContext context,
          IconKey iconKey
          )
  {
    LafIconProvider iconProvider = getIconProvider(context.getLookAndFeel());
    if (iconProvider != null)
      return iconProvider.getIconURI(context, iconKey);

    return null;
  }

  // Returns the LafIconProvider for the current LookAndFeel
  public static LafIconProvider getIconProvider(LookAndFeel laf)
  {
    if (laf instanceof LafIconProviderProvider)
      return ((LafIconProviderProvider)laf).getLafIconProvider();
    else if (laf instanceof LookAndFeelExtension)
    {
      // Check the base LAF
      LookAndFeel baseLAF = ((LookAndFeelExtension)laf).getBaseLookAndFeel();
      return getIconProvider(baseLAF);
    }

    return null;
  }

  // Returns the absolute URI based on the Configuration
  // IMAGES_DIRECTORY if the specified uri is relative.
  public static String getAbsoluteImageURI(
          UIXRenderingContext context,
          String           uri
          )
  {
    // First, check to see if a protocol is specified - if so, uri is absolute
    if ((uri == null) || (uri.length() == 0) || (uri.indexOf(':') != -1))
      return uri;

    // Check to see if the uri starts with a separator
    if (uri.charAt(0) == URI_DELIMITER)
      return uri;

    String baseImageURI = getBaseImageURI(context);

    if ((baseImageURI == null) || (baseImageURI.length() == 0))
      return uri;

    return baseImageURI + uri;
  }



  protected static String appendURLArgument(
          String baseURL,
          String key,
          String value
          )
  {
    return BaseLafUtils.appendURLArguments(baseURL, new String[] { key, value });
  }

  protected static String appendURLArguments(
          StringBuffer buffer,
          String baseURL,
          String[] keysAndValues
          )
  {
    return BaseLafUtils.appendURLArguments( buffer, baseURL, keysAndValues);
  }


  protected static String appendURLArguments(
          String baseURL,
          String[] keysAndValues
          )
  {
    return BaseLafUtils.appendURLArguments( baseURL, keysAndValues);
  }

  protected String encodeActionURL(
   UIXRenderingContext context,
   Object       value) throws IOException
  {
    if (value != null)
    {
      FacesContext facesContext = context.getFacesContext();
      if(facesContext != null)
      {
        return facesContext.getExternalContext().encodeActionURL(value.toString());
      }
      return value.toString();
    }
    return null;
  }
  
  protected String encodeResourceURL(
   UIXRenderingContext context,
   Object       value) throws IOException
  {
    if (value != null)
    {
      FacesContext facesContext = context.getFacesContext();
      if(facesContext != null)
      {
        return facesContext.getExternalContext().encodeResourceURL(value.toString());
      }
      return value.toString();
    }
    return null;
  }  
  
  protected void renderEncodedActionURI(
   UIXRenderingContext context,
   String       name,
   Object       value) throws IOException
  {
    if (value != null)
    {
      String encodedURL = encodeActionURL(context, value);
      context.getResponseWriter().writeURIAttribute(name, encodedURL, null);
    }
  }

  protected void renderEncodedResourceURI(
   UIXRenderingContext context,
   String       name,
   Object       value) throws IOException
  {
    if (value != null)
    {
      String encodedURL = encodeResourceURL(context, value);
      context.getResponseWriter().writeURIAttribute(name, encodedURL, null);
    }
  }

  /**
   * Returns the Agent capability specified by the key
   */
  protected static Object getAgentCapability(
          UIXRenderingContext   context,
          CapabilityKey key
          )
  {
    return context.getAgent().getCapability(key);
  }


  /**
   * Returns true iff the specified agent capability is true
   */
  protected static boolean getBooleanAgentCapability(
          UIXRenderingContext   context,
          CapabilityKey key
          )
  {
    return Boolean.TRUE.equals(getAgentCapability(context, key));
  }

  /**
   * Returns true if the agent supports a full dom
   */
  public static boolean supportsFullDOM(
          UIXRenderingContext context
          )
  {
    return (TrinidadAgent.DOM_CAP_LEVEL_2 ==
            context.getAgent().getCapability(TrinidadAgent.CAP_DOM));
  }

  /**
   * Returns true if the agent supports the id attribute
   */
  public static boolean supportsID(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_ID);
  }

  /**
   * Returns true if the agent supports the xmldom
   */
  public static boolean supportsXMLDOM(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_XMLDOM);
  }


  /**
   * Returns true if the agent supports access keys
   */
  public static boolean supportsAccessKeys(
          UIXRenderingContext context
          )
  {
    if (isScreenReaderMode(context))
      return false;
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_ACCESS_KEYS);
  }


  /**
   * Returns true if the agent supports the text presentation module.
   * <p>
   * See section 5.4.1 of xhtml modularization.
   */
  public static boolean supportsTextPresentation(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_TEXT_PRESENTATION);
  }

  /**
   * Returns true if the agent supports the (advanced) forms module
   * <p>
   * See section 5.5.2 of xhtml modularization.
   */
  public static boolean supportsAdvancedForms(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_ADVANCED_FORMS);
  }


  /**
   * Returns true if the agent supports the (advanced) tables module
   * <p>
   * See section 5.6.2 of xhtml modularization.
   */
  public static boolean supportsAdvancedTables(
          UIXRenderingContext context
          )
  {
    return (TrinidadAgent.TABLES_CAP_ADVANCED ==
            getAgentCapability(context, TrinidadAgent.CAP_TABLES));
  }


  /**
   * Returns true if the agent supports the Frames module
   * <p>
   * See section 5.11 of xhtml modularization.
   */
  public static boolean supportsFrames(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_FRAMES);
  }


  /**
   * Returns true if the agent supports setting the target
   * attribute of other elements.
   * <p>
   * See section 5.12 of xhtml modularization.
   */
  public static boolean supportsTarget(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_TARGET);
  }


  /**
   * Returns true if the agent supports the Iframes module
   * <p>
   * See section 5.13 of xhtml modularization.
   */
  public static boolean supportsIframes(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_IFRAMES);
  }


  /**
   * Returns true if the event attributes should be rendered for this node.
   * <p>
   * Clients should override this method if the the user agent
   * doesn't support event attributes.
   * <p>
   * See section 5.14 of xhtml modularization.
   */
  public static boolean supportsIntrinsicEvents(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_INTRINSIC_EVENTS);
  }


  /**
   * Returns true if the agent supports the Script module.
   * <p>
   * See section 5.16 of xhtml modularization
   */
  public static boolean supportsScripting(
          UIXRenderingContext context
          )
  {

    Object scriptingSpeed = getAgentCapability(context,
            TrinidadAgent.CAP_SCRIPTING_SPEED);

    return ((scriptingSpeed != null) &&
            (TrinidadAgent.SCRIPTING_SPEED_CAP_NONE != scriptingSpeed));

  }

  /**
   * Returns true if the agent supports opening multiple windows
   */
  public static boolean supportsMultipleWindows(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_MULTIPLE_WINDOWS);
  }


  /**
   * Returns true if the style attributes should be rendered for this node.
   * <p>
   * Clients should override this method if the the user agent
   * doesn't support style attributes.
   * <p>
   * See section 5.18 of xhtml modularization
   */
  public static boolean supportsStyleAttributes(
          UIXRenderingContext context
          )
  {
    return (getAgentCapability(context, TrinidadAgent.CAP_STYLE_ATTRIBUTES) !=
            TrinidadAgent.STYLES_NONE);
  }


  /**
   * Returns true if the CSS class attribute is supported.
   */
  public static boolean supportsClassAttribute(
          UIXRenderingContext context
          )
  {
    // STYLES_INTERNAL means that internal styles are supported, like the
    // <style> element. This means that class attributes may be supported.
    return ((getAgentCapability(context, TrinidadAgent.CAP_STYLE_ATTRIBUTES) ==
            TrinidadAgent.STYLES_EXTERNAL) || 
           (getAgentCapability(context, TrinidadAgent.CAP_STYLE_ATTRIBUTES) ==
                TrinidadAgent.STYLES_INTERNAL));
  }


  /**
   * Returns true if the value of the CSS class attribute can
   * take a space separated list of style selectors
   */
  public static boolean supportsMultipleCssSelectors(
          UIXRenderingContext context
          )
  {
    return (getAgentCapability(context, TrinidadAgent.CAP_CSS_SELECTORS) ==
            TrinidadAgent.SELECTORS_MULTIPLE);
  }


  /**
   * Returns true if navigation is supported.
   */
  public static boolean supportsNavigation(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_NAVIGATION);
  }


  /**
   * Returns true if editing is supported.
   */
  public static boolean supportsEditing(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_EDITING);
  }

  /**
   * Returns true if the agent supports the Name identification module.
   * <p>
   * See section 5.21 of xhtml modularization
   */
  public static boolean supportsNameIdentification(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_NAME_IDENTIFICATION);
  }

  /**
   * Returns true if the agent supports rendering disabled form elements
   */
  public static boolean supportsDisabledFormElements(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context,
            TrinidadAgent.CAP_DISABLED_FORM_ELEMENTS);
  }

  /*
   * Returns true if the agent supports rendering autocomplete form elements
   */
  public static boolean supportsAutoCompleteFormElements(
    UIXRenderingContext context
    )
  {
    return getBooleanAgentCapability(context,
            TrinidadAgent.CAP_AUTO_COMPLETE_FORM_ELEMENTS);
  }

  /**
   * Returns true if the agent supports rendering readonly form elements
   */
  public static boolean supportsReadOnlyFormElements(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context,
            TrinidadAgent.CAP_READONLY_FORM_ELEMENTS);
  }

  /**
   * Returns true if the browser supports rendering of fieldset element.
   */
  public static boolean supportsFieldSetElement(
          UIXRenderingContext context
          )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_FIELDSET);
  }

  /**
   * @return true if we are in inaccessible mode
   */
  public static boolean isInaccessibleMode(UIXRenderingContext context)
  {
    RenderingContext rc = RenderingContext.getCurrentInstance();
    return rc.getAccessibilityMode() ==
       RequestContext.Accessibility.INACCESSIBLE;
  }

  /**
   * @return true if we are in screen reader mode
   */
  public static boolean isScreenReaderMode(UIXRenderingContext context)
  {
    RenderingContext rc = RenderingContext.getCurrentInstance();
    return rc.getAccessibilityMode() ==
       RequestContext.Accessibility.SCREEN_READER;
  }

  /** 
   * Returns true if a user agent is a narrow-screen PDA
   * @param context a <code>UIXRenderingContext</code>
   * @return a <code>boolean</code>
   */
  public static boolean supportsNarrowScreen(UIXRenderingContext context)
  {
    return getBooleanAgentCapability(context,
                                         TrinidadAgent.CAP_NARROW_SCREEN);
  }

  private static final String _BASE_IMAGE_URI_PROPERTY = "baseImageURI";

  private static final Integer _UNSELECTED_INDEX = Integer.valueOf(-1);
}
