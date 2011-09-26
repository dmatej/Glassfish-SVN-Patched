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
package org.apache.myfaces.trinidadinternal.menu;

import java.util.List;

import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.model.XMLMenuModel;
import org.apache.myfaces.trinidad.util.ContainerUtils;

/**
 * Code generic to a Menu Nodes of the menu model.
 *
 * IMPORTANT NOTE: even internally, values that support EL expressions
 * should use the "get" methods to obtain values.
 * @deprecated Use org.apache.myfaces.trinidad.menu.MenuNode instead of this one.
 */

@Deprecated
public class MenuNode
{
  /**
    * Constructs a MenuNode
    */
  public MenuNode()
  {
  }

  /**
    * Set the menu item's label.
    *
    * @param label - String name shown in the menu item
    */
  public void setLabel(String label)
  {
    _label = label;
  }

  /**
    * Get the menu item's label
    *
    * This could either be a string value set directly in
    * the metadata or an EL expression string. In the case
    * of an EL expression, we need to get its bound value.
    *
    * @return label as a String
    */
  public String getLabel()
  {
    if (_bundleKey != null && _bundleName != null)
    {
      // Load the resource bundle based on the locale of the
      // current request.  If the locale has not changed, this
      // method just returns.
      MenuUtils.loadBundle(_bundleName, _bundleKey + getHandlerId());
    }

    if (   _label != null
        && ContainerUtils.isValueReference(_label)
       )
    {
      // do not set _label to the evaluated EL.
      // It may change at times in the EL.
      return _evalElStr(_label);
    }
    return _label;
  }

  /**
    * Set the icon used by the menu item.
    *
    * @param icon - the String URI to the icon.
    */
  public void setIcon(String icon)
  {
    _icon = icon;
  }

  /**
    * Get the icon used by the menu item
    * This could either be a string value set directly in
    * the metadata or an EL expression string. In the case
    * of an EL expression, we need to get its bound value.
    *
    * @return icon - the String URI to the icon.
    */
  public String getIcon()
  {
    return MenuUtils.evalString(_icon);
  }

  /**
    * Sets the rendered attribute of the menu item.
    * If false, menu item will not appear.
    *
    * @param rendered - boolean that toggles the visible state of the XMLMenuModel
    * item.
    */
  public void setRendered(boolean rendered)
  {
    _renderedStr = rendered ? "true" : "false";
  }

  /**
    * Gets the rendered attribute of the menu item.
    * If false, menu item will not appear.
    *
    * @return boolean indicating whether or not the menu item is visible.
    */
  public boolean getRendered()
  {
    boolean rendered = MenuUtils.evalBoolean(_renderedStr, true);
    return rendered;
  }

  /**
    * Sets the disabled attribute of the menu item.
    * If true, menu item will not appear greyed-out and clicking
    * on it will have no effect
    *
    * @param disabled - boolean that toggles the enabled/disabled state of the
    * menu item.
    */
  public void setDisabled(boolean disabled)
  {
    _disabledStr = disabled ? "true" : "false";
  }

  /**
    * Gets the disabled attribute of the menu item.
    * If true, menu item will not appear greyed-out and clicking
    * on it will have no effect
    *
    * @return boolean indicating whether or not the menu item is disabled.
    */
  public boolean getDisabled()
  {
    boolean disabled = MenuUtils.evalBoolean(_disabledStr, false);
    return disabled;
  }

  /**
    * Sets the visible attribute of the menu item.
    * If false, menu item will not appear
    *
    * @param visible - boolean that toggles the visible state of the
    * menu item.
    */
  public void setVisible(boolean visible)
  {
    _visibleStr = visible ? "true" : "false";
  }

  /**
    * Gets the visible attribute of the menu item.
    * If false, menu item will not appear
    *
    * @return boolean indicating whether or not the menu item is visible.
    */
  public boolean getVisible()
  {
    boolean visible = MenuUtils.evalBoolean(_visibleStr, true);
    return visible;
  }

  /**
    * Sets the defaultFocusPath attribute of the menu item.
    *
    * @param defaultFocusPath - boolean that tells the XMLMenuModel model that
    * the focus path to this node should be used in cases where the focus path
    * is not determinable by the XMLMenuModel model.
    */
  public void setDefaultFocusPath(boolean defaultFocusPath)
  {
    _defaultFocusPathStr = defaultFocusPath ? "true" : "false";
  }

  /**
    * Gets the defaultFocusPath attribute of the menu item.
    *
    * @return boolean indicating whether or not this is the focus path to use,
    * by default, in cases where there are duplicate paths to this node and
    * the focus path is not determinable by the XMLMenuModel model.
    */
  public boolean getDefaultFocusPath()
  {
    boolean defaultFocusPath = MenuUtils.evalBoolean(_defaultFocusPathStr,
                                                     false);
    return defaultFocusPath;
  }

  /**
    * Get the List of menu item's children.
    *
    * @return List of menu item's children
    */
  public List<MenuNode> getChildren()
  {
    return _children;
  }

  /**
    * Set the List of menu item's children.
    *
    * @param children - List of MenuNode children for this MenuNode
    */
  public void setChildren(List<MenuNode> children)
  {
    _children = children;
  }

  /**
    * Gets the readOnly state of the node.
    *
    * @return the node's readOnly state as a boolean.
    */
  public boolean getReadOnly()
  {
    boolean readOnly = MenuUtils.evalBoolean(_readOnlyStr, false);
    return readOnly;
  }

  /**
    * Sets the the value of the readOnly attribute of the node.
    *
    * @param readOnly - boolean setting readOnly state of the node
    */
  public void setReadOnly(boolean readOnly)
  {
    _readOnlyStr = readOnly ? "true" : "false";
  }

  /**
  * Sets the value of the node's focusViewId property.
  *
  * @param focusViewId - string value of the Node's "focusViewId" property.
  */
  public void setFocusViewId(String focusViewId)
  {
    _focusViewId = focusViewId;
  }

  /**
  * Gets the value of the node's focusViewId property.
  *
  * @return string - the value of the Node's "focusViewId" property.
  */
  public String getFocusViewId()
  {
    return _focusViewId;
  }

  /**
    * Sets the rendered attribute of the menu item.
    * If false, menu item will not appear.
    *
    * Called only from MenuContentHandlerImpl during parsing of metadata.
    *
    * @param renderedStr - string representing a boolean value
    */
  public void setRendered(String renderedStr)
  {
    _renderedStr = renderedStr;
  }

  /**
    * Sets the disabled attribute of the menu item.
    * If false, menu item will appear in a disabled state.
    *
    * @param disabledStr - string representing a boolean value or
    * an EL Expression
    */
  public void setDisabled(String disabledStr)
  {
    _disabledStr = disabledStr;
  }

  /**
    * Sets the readOnly attribute of the menu item.
    * If false, menu item will appear in a readOnly state.
    *
    * @param readOnlyStr - string representing a boolean value or EL
    * expression.
    */
  public void setReadOnly(String readOnlyStr)
  {
    _readOnlyStr = readOnlyStr;
  }

  /**
    * Sets the visible attribute of the menu item.
    * If false, menu item will not appear.
    *
    * @param visibleStr - string representing a boolean value or
    * an EL Expression
    */
  public void setVisible(String visibleStr)
  {
    _visibleStr = visibleStr;
  }

  /**
    * Sets the defaultFocusPath attribute of the menu item.
    *
    * Called only from MenuContentHandlerImpl during parsing of metadata.
    *
    * @param defaultFocusPathStr - string representing a boolean value
    */
  public void setDefaultFocusPath(String defaultFocusPathStr)
  {
    _defaultFocusPathStr = defaultFocusPathStr;
  }

  /**
   * setAccessKey - Takes either a single character String or
   * an EL expression and sets the value of the accessKey attribute
   * of the node.
   *
   * @param accessKey - Single character String or EL expression
   * representing the label's access key.
   */
  public void setAccessKey (String accessKey)
  {
    if (   accessKey != null
        && ContainerUtils.isValueReference(accessKey)
       )
    {
       // EL Expression
      _accessKey = accessKey;
    }
    else
    {
      // accessKey cannot be more than one character
      if (accessKey != null && accessKey.length() > 1)
        return;

      _accessKey = accessKey;
    }
  }

  /**
   * setAccessKey - Takes a single character and sets the value of the
   * accessKey attribute of the node.
   *
   * @param accessKey - Single character label access key.
   */
  public void setAccessKey (char accessKey)
  {
      char[] charArray = {'\0'};

      charArray[0] = accessKey;
      _accessKey = String.copyValueOf(charArray);
  }

  /**
   * getAccessKey - get the label's accessKey as a char.
   *
   * @return the access key of the label as a char.
   */
  public char getAccessKey()
  {
    String accessKeyStr = MenuUtils.evalString(_accessKey);

    if (accessKeyStr == null || accessKeyStr.length() > 1)
      return '\0';

    return accessKeyStr.charAt(0);
  }

  /**
   * setLabelAndAccessKey - Takes either an EL expression or a
   * String representing the label and accessKey together, and
   * sets the label and the accessKey separately.
   *
   * @param labelAndAccessKey - either and EL Expression or
   * a String representing the label and accessKey together.
   */
  public void setLabelAndAccessKey(String labelAndAccessKey)
  {
    int ampIdx = 0;
    _labelAndAccessKeyEL = false;

    // if EL expression, set it and the label to the same thing
    if (   labelAndAccessKey != null
        && ContainerUtils.isValueReference(labelAndAccessKey)
       )
    {
      _labelAndAccessKey   = labelAndAccessKey;
      _labelAndAccessKeyEL = true;
      _accessKey = null;
    }
    else if (   labelAndAccessKey == null
             || (ampIdx = labelAndAccessKey.indexOf('&')) == -1
            )
    {
      // String is null or a label w/o an accesskey
      _label = labelAndAccessKey;
      _accessKey = null;
    }
    else if (ampIdx == (labelAndAccessKey.length() - 1))
    {
      // & is last character, strip it.
      _label = labelAndAccessKey.substring(0, ampIdx);
      _accessKey = null;
    }
    else
    {
       // We have a string with an accessKey somewhere
       _splitLabelAndAccessKey(labelAndAccessKey);
    }
  }

  /**
   * getLabelAndAccessKey - get the label and accessKey together
   * in a single string.
   *
   * @return a String containing (representing) the label and accessKey
   * together.
   */
  public String getLabelAndAccessKey()
  {
    // If labelAndAccessKey is an EL expression
    // we get it and process it, set the label
    // and the accessKey, null out labelAndAccessKey
    // and set labelAndAccessKeyEL to false
    if (_labelAndAccessKeyEL)
    {
      _labelAndAccessKey = _evalElStr(_labelAndAccessKey);
      setLabelAndAccessKey(_labelAndAccessKey);
      _labelAndAccessKey = null;
      _labelAndAccessKeyEL = false;
    }
    // Now it is a simple string, so we have already
    // set the label and accessKey.  We get both the
    // label and accessKey, construct a labelAndAccessKey
    // and return it.
    String label = getLabel(); // This is a simple string

    if (_accessKey == null)
      return label; // String is just the label

    return _joinLabelAndAccessKey(label, _accessKey);
  }

  /**
   * setId - sets the id of the node.
   *
   * @param id - the identifier for the node component
   */
  public void setId (String id)
  {
    _id = id;
  }

  /**
   * getId - gets the metadata id of the node.
   *
   * @return - String identifier for the node component.
   */
  public String getId()
  {
    return _id;
  }

  /*===========================================================================
   * getRefNode(), doAction(), & getDestination() are never called.  They
   * are just here so that the same methods in itemNode.java and GroupNode.java
   * will compile.
   * ==========================================================================
   */
  /**
   * Get the node whose id matches this node's
   * idref attribute value.
   *
   * @return the MenuNode whose id matches this
   *         node's idref attribute value.
   */
  public MenuNode getRefNode()
  {
    return this;
  }

  /**
   * Called by the Default ActionListener
   * when a menu node is clicked/selected.
   *
   * @return String outcome or viewId used
   *         during a POST for navigation.
   */
  public String doAction()
  {
    // Call the doAction method of my idref node
    return getRefNode().doAction();
  }

  /**
   * Get the Destination URL of a page for a
   * GET.
   *
   * @return String URL of a page.
   */
  public String getDestination()
  {
    // Call the doAction method of my idref node
    return getRefNode().getDestination();
  }

  /**
   * Get the top-level, root menu model Request Map Key.
   *
   * @return root, top-level XMLMenuModel's Request Map Key.
   */
  public String getRootModelKey()
  {
    return _rootModelKey;
  }

  /**
   * Sets the root menu Model's Request map key.
   * <p>
   * This is always only the top-level, root model's Request map key.
   * We do this because the MenuContentHandlerImpl and nodes need to be able
   * to call into the root model to:
   * <ul>
   * <li>notify them root menu model of the currently selected node on a POST
   * <li>group node needs to find its referenced item node.
   * </ul>
   *
   * @param rootModelKey - String the root, top-level menu model's Request
   *        map key.
   */
  public void setRootModelKey(String rootModelKey)
  {
    _rootModelKey = rootModelKey;
  }

  /**
   * Gets the local (shared node's) menu Model's Request map key.
   */
  public String getModelId()
  {
    return _modelId;
  }

  /**
   * Sets the local (shared node's) menu Model's Request map key.
   * <p>
   * This is appended to the node's id to create a unique id.
   *
   * @param rootModelKey - String the local (shared node's) menu
   *        Model's Request map key.
   */
  public void setModelId(String modelId)
  {
    _modelId = modelId;
  }

  public int getRootId()
  {
    return _rootId;
  }
  public void setRootId(int id)
  {
    _rootId = id;
  }

  
  public final String getLabelProperty()
  {
    return _label;
  }

  public final String getIconProperty()
  {
    return _icon;
  }

  public final List<MenuNode> getChildrenProperty()
  {
    return _children;
  }

  public final String getFocusViewIdProperty()
  {
    return _focusViewId;
  }

  public final String getRenderedProperty()
  {
    return _renderedStr;
  }

  public final String getDisabledProperty()
  {
    return _disabledStr;
  }

  public final String getVisibleProperty()
  {
    return _visibleStr;
  }

  public final String getReadOnlyProperty()
  {
    return _readOnlyStr;
  }

  public final String getHandlerIdProperty()
  {
    return _handlerId;
  }

  public final String getBundleKeyProperty()
  {
    return _bundleKey;
  }

  public final String getBundleNameProperty()
  {
    return _bundleName;
  }

  public final String getAccessKeyProperty()
  {
    return _accessKey;
  }

  public final String getIdProperty()
  {
    return _id;
  }

  public final String getModelIdProperty()
  {
    return _modelId;
  }

  public final String getUniqueIdProperty()
  {
    return getIdProperty() + getModelIdProperty();
  }

  
  public final String getLabelAndAccessKeyProperty()
  {
    return _labelAndAccessKey;
  }

  public final String getDefaultFocusPathProperty()
  {
    return _defaultFocusPathStr;
  }

  public final String getRootModelKeyProperty()
  {
    return _rootModelKey;
  }

  public final int getRootIdProperty()
  {
    return _rootId;
  }
  
  /**
   * setResBundleKey - sets the name of the resource bundle used in
   * obtaining the node's label text. Used, along with the handerId,
   * to identify and get a string from the proper resource bundle.
   *
   * @param bundleKey - String name of the resource bundle.
   */
  protected void setResBundleKey(String bundleKey)
  {
    _bundleKey = bundleKey;
  }

  /**
   * setResBundleKey - sets the name of the resource bundle used in
   * obtaining the node's label text. Used, along with the handerId,
   * to identify and get a string from the proper resource bundle.
   *
   * @param bundleName - String name of the resource bundle.
   */
  protected void setResBundleName(String bundleName)
  {
    _bundleName = bundleName;
  }

  /**
   * setHandlerId - sets the MenuContentHandlerImpl's handlerId on the node.
   * Used, along with the bundleKey, to identify and get a string from the
   * proper resource bundle.
   *
   * @param handlerId String uniquely identifying the specific
   *        MenuContentHandlerImpl that created this node.
   */
  protected void setHandlerId(String handlerId)
  {
    _handlerId = handlerId;
  }

  /**
   * Notifies the root model that this node has been selected on a POST
   *
   * @param selectedNode - The currently selected menu item.
   */
  protected void postSelectedNode(MenuNode selectedNode)
  {
    getRootModel().setCurrentlyPostedNode(selectedNode);
  }

  /**
   * getUniqueId - gets the unique id of the node.
   *
   * @return - String identifier for the node component.
   */
  protected String getUniqueId()
  {
    // This must be made unique so that we do not have duplicates
    // in the idNodeMap on the menu's tree.
    return _id + _modelId;
  }

  /**
   * Set the MenuContentHandlerImpl's id.
   *
   * @return String object id of the MenuContentHandlerImpl
   */
  protected String getHandlerId()
  {
    return _handlerId;
  }

  /**
   * Get the top-level, root menu model, which contains
   * the entire menu tree.
   *
   * @return root, top-level XMLMenuModel
   */
  @SuppressWarnings("unchecked")
  protected XMLMenuModel getRootModel()
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    Map<String, Object> requestMap =
      facesContext.getExternalContext().getRequestMap();

    Map map =  (Map) requestMap.get(getRootModelKey());
    XMLMenuModel model = (XMLMenuModel) map.get(getRootId());
    return model;
  }

  /**
   * Construct a thread safe version
   * of this object and return it.
   * @return a thread safe copy of this object. 
   */
  protected MenuNode getThreadSafeCopy()
  {
    return null;
  }
  /**
   * _joinLabelAndAccessKey - takes a string label and string accessKey
   * and combines them into a single labelAndAccessKey string.
   *
   * @param label - String with node's label.
   * @param accessKey - One character String which is the label's accessKey.
   * @return
   */
  private String _joinLabelAndAccessKey(String label, String accessKey)
  {
    char[] keyArray  = label.toCharArray();
    int len          = label.length();
    int lentimes2    = len*2;
    char[] keyArray2 = new char[lentimes2];
    int i, j = 0;
    boolean accessKeyFound = false;

    // find the first occurrence of a single Ampersand
    for (i=0, j=0; i < len; i++, j++)
    {
      // AccessKey
      if (   keyArray[i] == accessKey.charAt(0)
          && !accessKeyFound
         )
      {
        keyArray2[j] = '&';
        j++;
        accessKeyFound = true;
      }

      keyArray2[j] = keyArray[i];

      // Ampersand as regular character
      // double it up.
      if (keyArray[i] == '&')
      {
        j++;
        keyArray2[j] = keyArray[i];
      }
    }

    String combinedLabel = new String(keyArray2, 0, j);
    return combinedLabel;
  }

  /**
   * _splitLabelAndAccessKey - takes a string containing a label
   * and an accessKey and breaks separates it and sets the label
   * and the accessKey separately.
   *
   * @param labelAndAccessKey - String holding both a label and
   * accessKey.
   */
  private void _splitLabelAndAccessKey(String labelAndAccessKey)
  {
    char[] keyArray  = labelAndAccessKey.toCharArray();
    int len = labelAndAccessKey.length();
    char[] keyArray2 = new char[len];
    int i, j = 0;
    boolean accessKeyFound = false;

    for (i=0, j=0; i < len ; i++, j++)
    {
      if (keyArray[i] == '&')
      {
         i++;

         if (!accessKeyFound && keyArray[i] != '&')
         {
           // We have our accessKey
           _accessKey = labelAndAccessKey.substring(i, i+1);
           accessKeyFound = true;
         }
      }

      keyArray2[j] = keyArray[i];
    }

    String label = new String(keyArray2, 0, j);
    _label = label;
  }

  /**
   * _evalElStr - Evaluate an EL expression string.
   *
   * @param str - the EL expression
   * @return the bound value of the El expression as a String
   */
  private String _evalElStr(String str)
  {
    if (str == null)
      return null;

    String keystr = MenuUtils.stringReplaceFirst(str.trim(), _bundleKey,
                                                 _bundleKey + getHandlerId());
    String elVal = MenuUtils.getBoundValue(keystr, String.class);
    return elVal;
  }

  private String         _label       = null;
  private String         _icon        = null;
  private List<MenuNode> _children    = null;
  private String         _focusViewId = null;
  private String         _renderedStr = null;
  private String         _disabledStr = null;
  private String         _visibleStr  = null;
  private String         _readOnlyStr = null;
  private String         _handlerId   = null;
  private String         _bundleKey   = null;
  private String         _bundleName  = null;
  private String         _accessKey   = null;
  private String         _id          = null;
  private String         _modelId     = null;
  private boolean        _labelAndAccessKeyEL = false;
  private String         _labelAndAccessKey   = null;
  private String         _defaultFocusPathStr = null;

  // Root Menu model's Request Map Key
  private String _rootModelKey  = null;
  
  private int _rootId;

}
