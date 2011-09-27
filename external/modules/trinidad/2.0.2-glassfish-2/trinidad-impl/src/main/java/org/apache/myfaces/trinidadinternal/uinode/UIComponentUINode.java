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
package org.apache.myfaces.trinidadinternal.uinode;

import java.io.IOException;
import java.io.InputStream;

import java.net.URL;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.FacesBeanWrapper;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.component.UIXComponentBase;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ArrayMap;
import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;


/**
 * Implements UINode and setAttributeValue().
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/UIComponentUINode.java#1 $) $Date: 11-nov-2005.14:59:38 $
 * @todo Thoroughly investigate performance of this class.
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class UIComponentUINode implements UINode
{
  protected UIComponentUINode(
   UIComponent component,
   String      namespace)
  {
    if (component == null)
      throw new NullPointerException();

    _component = component;
    _namespace = namespace;
  }

  public UIComponent getUIComponent()
  {
    return _component;
  }

  /**
   * @todo should this be getClientId()?
   */
  public String getID()
  {
    return _component.getId();
  }

  public String getNamespaceURI()
  {
    return _namespace;
  }

  public String getLocalName()
  {
    String family = _component.getFamily();
    String rendererType = _component.getRendererType();
    return _get(family, rendererType);
  }

  public int getIndexedChildCount(UIXRenderingContext context)
  {
    return _component.getChildCount();
  }


  /**
   * @todo can the children UIComponentUINode instances be cached anywhere?
   * @todo come up with a real namespace/local name for these UINodes
   */
  public UINode getIndexedChild(UIXRenderingContext context, int childIndex)
  {
    UIComponent child = (UIComponent) _component.getChildren().get(childIndex);
    return __getUINode(child);
  }

  /**
   * @todo can the children UIComponentUINode instances be cached anywhere?
   * @todo come up with a real namespace/local name for these UINodes
   */
  public UINode getNamedChild(UIXRenderingContext context, String childName)
  {
    UIComponent child = _component.getFacet(childName);
    if (child == null)
      return null;

    return __getUINode(child);
  }


  @SuppressWarnings("unchecked")
  public Iterator<String> getChildNames(UIXRenderingContext context)
  {
    if (_component instanceof UIXComponentBase)
      return ((UIXComponentBase) _component).getFacetNames();

    return _component.getFacets().keySet().iterator();
  }

  /**
   * @todo implement this (if necessary)
   */
  public Iterator<AttributeKey> getAttributeNames(UIXRenderingContext context)
  {
    throw new UnsupportedOperationException();
  }


  public void setAttributeValue(
    AttributeKey attrKey,
    Object       value
    )
  {
    throw new UnsupportedOperationException();
  }


  public Object getAttributeValue(UIXRenderingContext context, AttributeKey attrKey)
  {
    // Note that we do not support BoundValues on generic UIComponents;
    // this is intentional!
    return getRawAttributeValue(context, attrKey);
  }


  /**
   * Returns the value of the attribute with a specified name, without
   * attempting to further resolve that value - as if , for instance,
   * it might be a BoundValue.
   * @todo Any other UIComponent attributes that a parent UINode would care
   *    about?  How about SELECTED_ATTR?
   */
  public Object getRawAttributeValue(UIXRenderingContext context,
                                     AttributeKey attrKey)
  {
    // Here, we're concerned not about a Renderer trying to retrieve
    // lots of attributes on itself, but on a parent Renderer retrieving
    // properties from the child.  RENDERED_ATTR is absolutely necessary;
    // are any others?
    if (attrKey == UIConstants.RENDERED_ATTR)
      return _component.isRendered() ? Boolean.TRUE : Boolean.FALSE;

    return null;
  }

  /**
   * Returns the role that this node occupies.
   */
  public NodeRole getNodeRole(UIXRenderingContext context)
  {
    return UIConstants.UNKNOWN_ROLE;
  }
// =-=jmw test only. test only. translation test...
// compare this to the key. If it doesn't match, flag.
  /***
private void _setTranslationKeyTest(
  RenderingContext context,
  UIComponent      component,
  Stack            componentStack)
{
  // e.g., family = org.apache.myfaces.trinidad.SelectBoolean
  // e.g., rendererType = org.apache.myfaces.trinidad.Checkbox
  String family = component.getFamily();
  String rendererType = component.getRendererType();

  String familySuffix = (family != null) ?
    family.substring(family.lastIndexOf('.')+1) :
    "";
  String rendererTypeSuffix = (rendererType != null) ?
    rendererType.substring(rendererType.lastIndexOf('.')+1) :
    "";

  StringBuffer translationKeyPrefix =
    new StringBuffer(3 + familySuffix.length() + rendererTypeSuffix.length());
  translationKeyPrefix.append("af_");
  translationKeyPrefix.append(familySuffix);
  if (!family.equals(rendererType))
      translationKeyPrefix.append(rendererTypeSuffix);

  if (componentStack == null)
      componentStack = new Stack();
  componentStack.push(translationKeyPrefix.toString());

  context.getFacesContext().getExternalContext().
    getRequestMap().put("TRANSLATION_KEY", componentStack);

}
***/

  /**
   * Renders this UINode.  Clients can implement this
   * method in any way desired.
   */
   /**** jmw for testing translation key map
  public void render(RenderingContext context)
    throws IOException
  {
    // get stack from request map
    Stack componentStack =
      (Stack)context.getFacesContext().getExternalContext().
                     getRequestMap().get("TRANSLATION_KEY");

     // if it is null, create it
    if (componentStack == null)
      componentStack = new Stack();

    // _setTranslationKeyTest pushes the key onto the stack
    _setTranslationKeyTest(context, getUIComponent(), componentStack); //jmw test

    render(context, this);

    // pop one entry from the stack
    // jmw for testing
    if (componentStack != null)
      componentStack.pop();
  }
  ****/

  public void render(UIXRenderingContext context)
    throws IOException
  {
    render(context, this);
  }

  /**
   *
   * @todo Investigate if flush() is still necessary now that we've
   * integrated our OutputMethod with ResponseWriter (we needed
   * to get our OutputMethod to close any pending elements.
   */
  public void render(UIXRenderingContext context, UINode dataNode)
    throws IOException
  {
    // We currently have no acceptable way to deal with "dataNode"
    assert(dataNode == this);

    context.getResponseWriter().flush();

    // We need to keep pushing the RenderingContext so that
    // we properly handle composite rendering.
    FacesContext fContext = context.getFacesContext();
    UIXRenderingContext oldContext =
        UINodeRendererBase.__setRenderingContext(fContext, context);

    _renderComponent(fContext, _component);

    UINodeRendererBase.__restoreRenderingContext(fContext, oldContext);
  }

  @Override
  public String toString()
  {
    StringBuffer buffer = new StringBuffer(40);
    String className = getClass().getName();
    int periodIndex = className.lastIndexOf('.');
    if (periodIndex >= 0)
      className = className.substring(periodIndex + 1);
    buffer.append(className);

    buffer.append(", localName='");
    buffer.append(getLocalName());
    buffer.append("'");

    UIComponent component = getUIComponent();
    if (component != null)
    {
      buffer.append('[');
      buffer.append(component.toString());
      buffer.append(",rendererType=");
      buffer.append(component.getRendererType());
      buffer.append(']');
    }

    return buffer.toString();
  }



  @SuppressWarnings("unchecked")
  private void _renderComponent(FacesContext context, UIComponent component)
    throws IOException
  {
    component.encodeBegin(context);
    if (component.getRendersChildren())
    {
      component.encodeChildren(context);
    }
    else
    {
      int count = component.getChildCount();
      if (count > 0)
      {
        List<UIComponent> children = component.getChildren();
        for (int i = 0; i < count; i++)
        {
          UIComponent child = children.get(i);
          if (child.isRendered())
            _renderComponent(context, child);
        }
      }
    }

    component.encodeEnd(context);
  }


  static UINode __getUINode(UIComponent component)
  {
    if (component instanceof UIXComponent)
    {
      FacesBean bean = ((UIXComponent) component).getFacesBean();

      // Since we are using instanceof, unwrap the bean if using the public bean wrapper from
      // the API project
      for (; bean instanceof FacesBeanWrapper; bean = ((FacesBeanWrapper)bean).getWrappedBean());

      if (bean instanceof UINodeFacesBean)
        return ((UINodeFacesBean) bean).getUINode();
    }

    return new UIComponentUINode(component, _FACES_NAMESPACE);
  }

  //
  // DEPRECATED METHODS: all unsupported
  //
  public int getIndexedChildCount() { throw new UnsupportedOperationException(); }

  public UINode getIndexedChild(int i) { throw new UnsupportedOperationException(); }

  public Iterator<String> getChildNames() { throw new UnsupportedOperationException(); }

  public UINode getNamedChild(String name) { throw new UnsupportedOperationException(); }

  public Object getAttributeValue(UIXRenderingContext context, String name) { throw new UnsupportedOperationException(); }

  private String      _namespace;
  private UIComponent _component;

  static private final String _FACES_NAMESPACE = null;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(UIComponentUINode.class);

  static private Map<String, Map<String, String>> _UIX2_LOCALNAMES;
  static
  {
    _UIX2_LOCALNAMES = new HashMap<String, Map<String, String>>(137);
    _loadRenderertypeToLocalnameMap();
  }

  /**
   * @todo The 3.0 names should be pushed into the 2.2 code base
   * when performance becomes an issue, doing double lookups...
   */
  static void _loadRenderertypeToLocalnameMap()
  {
    try
    {
      ClassLoader loader = Thread.currentThread().getContextClassLoader();
      Enumeration<URL> resources = loader.getResources("META-INF/renderertype-localname.properties");
      while (resources.hasMoreElements())
      {
        InputStream propertyStream = resources.nextElement().openStream();
        Properties properties = new Properties();
        properties.load(propertyStream);
        propertyStream.close();

        Iterator<Map.Entry<Object, Object>> keys =
          properties.entrySet().iterator();
        while (keys.hasNext())
        {
          Map.Entry<Object, Object> entry = keys.next();
          String key = (String) entry.getKey();
          String localName = (String) entry.getValue();
          int indexOfBar = key.indexOf('|');
          if (indexOfBar < 0)
            _LOG.severe("MALFORMED_PROPERTY_ENTRY", new Object[]{key, localName});
          else
          {
            String family = key.substring(0, indexOfBar);
            String rendererType = key.substring(indexOfBar + 1);
            _put(family, rendererType, localName);
          }
        }
      }
    }
    catch (Exception e)
    {
      _LOG.severe("CANNOT_LOAD_RENDERER_TYPE_TO_LOCAL_NAME_MAPPING", e);
    }
  }

  /**
   * @todo use ArrayMap for "sub-maps", since they'll almost
   * always be small?
   */
  synchronized static private void _put(
    String family,
    String rendererType,
    String localName)
  {
    Map<String, String> subMap = _UIX2_LOCALNAMES.get(family);
    if (subMap == null)
    {
      subMap = new ArrayMap<String, String>(7);
      _UIX2_LOCALNAMES.put(family, subMap);
    }

    subMap.put(rendererType, localName);
  }

  private String _get(
    String family,
    String rendererType)
  {
    Map<String, String> subMap = _UIX2_LOCALNAMES.get(family);
    if (subMap == null)
      return null;

    return subMap.get(rendererType);
  }
}
