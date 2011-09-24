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
package org.apache.myfaces.trinidad.menu;

import java.util.Map;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.el.MethodExpression;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.util.ContainerUtils;

/**
 * Code specific to a Menu Model's ItemNode.
 *     
 */

public class ItemNode extends MenuNode
{
  /**
    * Constructs an ItemNode
    */
  public ItemNode()
  {
    super();
  }
  
  /**
    * Sets the action of the node.  This is obtained from the menu
    * metadata file and is the string value of the "action" 
    * property.
    * 
    * @param action - the string value of the ItemNode's "action" property.
    */

  public void setAction(String action)
  {
    _action = action;
  }  
  
  /**
    * Gets the value of the node's action property.  The action attr value
    * could be one of 2 things:
    * 1) An EL expression
    * 2) An outcome referencing a navigation rule in the faces_config file.
    * 
    * Since this method is called only when an ItemNode is clicked, the model 
    * is notified that this node is the currently selected node.
    * 
    * @return String value of the ItemNode's "action" property.
    */
  @Override
  public String doAction()
  {
    String value = _action;

    if (value != null)
    {
      FacesContext facesContext = FacesContext.getCurrentInstance();
      ExpressionFactory expressionFactory =
          facesContext.getApplication().getExpressionFactory();
      ELContext context = facesContext.getELContext();
      MethodExpression methodExpression =
          expressionFactory.createMethodExpression(context, value,
              String.class, new Class<?>[]
              {});
      value = (String) methodExpression.invoke(context, null);
    }

    // Post me as the selected Node for the request
    postSelectedNode(this);

    return value;
  }

  /**
    * setActionListener - sets the value of the Menu Node's actionListener
    * atribute.
    * 
    * @param actionListener - El expression method reference to an 
    * action listener
    */
  public void setActionListener(String actionListener)
  {
    _actionListener = actionListener;
  }  
  
  /**
    * getActionListener - gets the value of the Menu Node's actionListener
    * attribute.
    * 
    * @return String  - method reference to an 
    * action listener
    */
  public String getActionListener()
  {
    String value = _actionListener;

    // Could be EL expression
    if (   value != null
        && ContainerUtils.isValueReference(value)
       )
    {
      // Value of action is EL method binding, so we 
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
      setActionListener(value);
    }

    return value;
  }
  
  public void actionListener(ActionEvent event)
  {
    String value = _actionListener;
    if (value != null)
    {
      FacesContext facesContext = FacesContext.getCurrentInstance();
      ExpressionFactory expressionFactory =
          facesContext.getApplication().getExpressionFactory();
      ELContext context = facesContext.getELContext();

      MethodExpression methodExpression =
          expressionFactory.createMethodExpression(context, value, Void.TYPE,
              new Class<?>[]
              { ActionEvent.class });
      methodExpression.invoke(context, new Object[]{ event });
    }

  }
  
  /**
    * setLaunchListener - sets the value of the Menu Node's launchListener
    * atribute.
    * 
    * @param launchListener - El expression method reference to a 
    * launch listener
    */
  public void setLaunchListener(String launchListener)
  {
    _launchListener = launchListener;
  }  
  
  /**
    * getLaunchListener - gets the value of the Menu Node's launchListener
    * attribute.
    * 
    * @return String  - method reference to an 
    * launch listener
    */
  public String getLaunchListener()
  {
    String value = _launchListener;

    // Could be EL expression
    if (   value != null
        && ContainerUtils.isValueReference(value)
       )
    {
      // Value of action is EL method binding, so we 
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
      setLaunchListener(value);
    }

    return value;
  }
  
  /**
    * setReturnListener - sets the value of the Menu Node's returnListener
    * atribute.
    * 
    * @param returnListener - El expression method reference to a 
    * return listener
    */
  public void setReturnListener(String returnListener)
  {
    _returnListener = returnListener;
  }  
  
  /**
    * getReturnListener - gets the value of the Menu Node's returnListener
    * attribute.
    * 
    * @return String  - method reference to an 
    * return listener
    */
  public String getReturnListener()
  {
    String value = _returnListener;

    // Could be EL expression
    if (   value != null
        && ContainerUtils.isValueReference(value)
       )
    {
      // Value of action is EL method binding, so we 
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
      setReturnListener(value);
    }

    return value;
  }
  
  /**
    * Sets the immediate attribute of the menu item.  
    *  
    * @param immediate - boolean indicating whether or not data validation - 
    * client-side or server-side - should take place when 
    * events are generated by this component. 
    */
  public void setImmediate(boolean immediate)
  {
    _immediateStr = immediate ? "true" : "false";
  }

  /**
    * Sets the immediate attribute of the menu item.  
    * 
    * @param immediateStr - string representing a boolean value
    * or an EL expression
    */
  public void setImmediate(String immediateStr)
  {
    _immediateStr = immediateStr;
  }
      
  /**
    * Gets the immediate attribute of the menu item.  
    *
    * @return boolean - indicating whether or not data validation - 
    * client-side or server-side - should take place when events 
    * are generated by this component. 
    */
  public boolean getImmediate()
  {
    boolean immediate = MenuUtils.evalBoolean(_immediateStr, false);
    return immediate;
  }
      
  /**
    * Sets the useWindow attribute of the menu item.  
    *  
    * @param useWindow - boolean indicating whether
    * or not to use a new window when launching dialogs. 
    */
  public void setUseWindow(boolean useWindow)
  {
    _useWindowStr = useWindow ? "true" : "false";
  }

  /**
    * Sets the useWindow attribute of the menu item.  
    * 
    * @param useWindowStr - string representing a boolean value or
    * an EL Expression
    */
  public void setUseWindow(String useWindowStr)
  {
    _useWindowStr = useWindowStr;
  }
      
  /**
    * Gets the useWindow attribute of the menu item.  
    *
    * @return boolean - indicating whether
    * or not to use a new window when launching dialogs. 
    */
  public boolean getUseWindow()
  {
    boolean useWindow = MenuUtils.evalBoolean(_useWindowStr, false);
    return useWindow;
  }
      
  /**
    * Sets the windowHeight attribute of the menu item.  
    *  
    * @param windowHeight - int height of the window, if 
    * this command is used to launch a window.
    */
  public void setWindowHeight(int windowHeight)
  {
    _windowHeightStr = Integer.toString(windowHeight);
  }

  /**
    * Sets the windowHeight attribute of the menu item.  
    *  
    * @param windowHeightStr - String Height of the window, if 
    * this command is used to launch a window. Could be an
    * EL expression
    */
  public void setWindowHeight(String windowHeightStr)
  {
    _windowHeightStr = windowHeightStr;
  }

  /**
    * Gets the windowHeight attribute of the menu item.  
    *
    * @return int height of the window, if 
    * this command is used to launch a window. 
    */
  public int getWindowHeight()
  {
    int windowHeight = MenuUtils.evalInt(_windowHeightStr);
    return windowHeight;
  }
      
  /**
    * Sets the windowWidth attribute of the menu item.  
    *  
    * @param windowWidth - int width of the window, if 
    * this command is used to launch a window.
    */
  public void setWindowWidth(int windowWidth)
  {
    _windowWidthStr = Integer.toString(windowWidth);
  }

  /**
    * Sets the windowWidth attribute of the menu item.  
    *  
    * @param windowWidthStr - String width of the window, if 
    * this command is used to launch a window. Could be an
    * EL expression
    */
  public void setWindowWidth(String windowWidthStr)
  {
    _windowWidthStr = windowWidthStr;
  }

  /**
    * Gets the windowWidth attribute of the menu item.  
    *
    * @return int width of the window, if 
    * this command is used to launch a window. 
    */
  public int getWindowWidth()
  {
    int windowWidth = MenuUtils.evalInt(_windowWidthStr);
    return windowWidth;
  }

  /**
    * Sets the destination of the node.  
    * 
    * This is obtained from the metadata file and is the string
    * value of the "destination" property.
    *
    * @param destination - either a URI or an EL method binding expression.
    */
  public void setDestination(String destination)
  {
    _destination = destination;
  }  
  
  /**
    * Gets the value of the node's destination property.
    * The destination attr value could be one of 2 things:
    * 1) a uri
    * 2) An EL expression
    * 
    * So that the model can identify this node as the currently selected
    * node, the node's id is appended to the destination as a parameter
    * that is picked up when the getFocusRowKey() method of the model 
    * is called to get the focus path.
    * 
    * @return destination - the String value of the destinationNode's
    *                       "destination" property.
    */
  @Override
  public String getDestination()
  {
    String value = _destination;
      
    // Could be EL expression
    if (   value != null
        && ContainerUtils.isValueReference(value)
       ) 
    {
      // Value of action is EL method binding, so we 
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
    }

    // Appending nodeId to URL so that we can identify the node
    // when getFocusRowKey() is called on the model.
    return value != null ? value + "?nodeId=" + getUniqueId() : value;
  }
  
  /**
   * setTargetFrame - sets the value of the Destination Node's
   * targetFrame attribute
   * 
   * @param targetFrame - the target frame for the goCommandMenuItem.
   */
  public void setTargetFrame(String targetFrame) 
  {
    _targetFrame = targetFrame; 
  }

  /**
   * getTargetFrame - gets the value of the Destination Node's
   * targetFrame attribute
   * 
   * @return the target frame for the goCommandMenuItem.
   */
  public String getTargetFrame()
  {
    String value = _targetFrame;
    
    // Could be EL expression
    if (   value != null
        && ContainerUtils.isValueReference(value)
       )
    {
      // Value of destination is EL value binding, so we 
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
      setTargetFrame(value);
    }
     
    return value;
  }
  
  /**
   * Get the Attributes containing the custom attributes on this node. This 
   * needs to be public so that the menu model can get them.
   * 
   * @return Attributes list containing the custom attributes on this node
   */
  public Map<String, String> getCustomPropList()
  {
    return _customPropList;
  }
  
  
  public final Map<String, String> getCustomPropListProperty()
  {
    return _customPropList;
  }

  public final String getDestinationProperty()
  {
    return _destination;
  }

  public final String getTargetFrameProperty()
  {
    return _targetFrame;
  }

  public final String getActionProperty()
  {
    return _action;
  }

  public final String getActionListenerProperty()
  {
    return _actionListener;
  }

  public final String getLaunchListenerProperty()
  {
    return _launchListener;
  }

  public final String getReturnListenerProperty()
  {
    return _returnListener;
  }

  public final String getImmediateProperty()
  {
    return _immediateStr;
  }

  public final String getUseWindowProperty()
  {
    return _useWindowStr;
  }

  public final String getWindowHeightProperty()
  {
    return _windowHeightStr;
  }

  public final String getWindowWidthProperty()
  {
    return _windowWidthStr;
  }
  /**
   * Set the list of custom attributes.
   * 
   * @param attrMap Map of attibute name/values for this node
   * from MenuContentHandlerImpl
   */
  public void setCustomPropList(Map<String, String> attrMap)
  {
    _customPropList = attrMap;
  }
  
  public MenuNode getThreadSafeCopy()
  {
    return new ImmutableItemNode(this);
  }
  
  // Map for Custom attributes (properties)
  private Map<String, String> _customPropList = null;
  
  private String _destination     = null;
  private String _targetFrame     = null;   
  private String _action          = null;
  private String _actionListener  = null;
  private String _launchListener  = null;
  private String _returnListener  = null;
  private String _immediateStr    = null;
  private String _useWindowStr    = null;
  private String _windowHeightStr = null;
  private String _windowWidthStr  = null;
  
 
}
