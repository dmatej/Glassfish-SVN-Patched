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

import java.util.List;
import java.util.Map;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.el.MethodExpression;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.util.ContainerUtils;

/**
 * This class is a thread safe version of ItemNode class.
 * It replicates most of the code in ItemNode but makes
 * sure it does not modify state of the object.
 * 
 * Therefore multiple request threads can access the
 * properties of the objects of this class,in a thread safe
 * manner.
 * 
 * Please note that setters should not be called on objects
 * of this class.Objects of this class are fully initialized
 * on construction.
 *
 */
public class ImmutableItemNode extends ItemNode
{
  
  public ImmutableItemNode(ItemNode node)
  {
    _icon = node.getIconProperty();
    _focusViewId = node.getFocusViewIdProperty();
    _renderedStr = node.getRenderedProperty();
    _disabledStr = node.getDisabledProperty();
    _visibleStr = node.getVisibleProperty();
    _readOnlyStr = node.getReadOnlyProperty();
    _handlerId = node.getHandlerIdProperty();
    _bundleKey = node.getBundleKeyProperty();
    _bundleName = node.getBundleNameProperty();
    _accessKey = node.getAccessKeyProperty();
    _id = node.getIdProperty();
    _modelId = node.getModelIdProperty();
    _uniqueId = node.getUniqueIdProperty();
    _labelAndAccessKey = node.getLabelAndAccessKeyProperty();
    _defaultFocusPathStr = node.getDefaultFocusPathProperty();

    // Root Menu model's Request Map Key
    _rootModelKey = node.getRootModelKeyProperty();

    _rootId = node.getRootIdProperty();

    _customPropList = node.getCustomPropListProperty();

    _destination = node.getDestinationProperty();
    _targetFrame = node.getTargetFrameProperty();
    _action = node.getActionProperty();
    _actionListener = node.getActionListenerProperty();
    _launchListener = node.getLaunchListenerProperty();
    _returnListener = node.getReturnListenerProperty();
    _immediateStr = node.getImmediateProperty();
    _useWindowStr = node.getUseWindowProperty();
    _windowHeightStr = node.getWindowHeightProperty();
    _windowWidthStr = node.getWindowWidthProperty();

    _label = node.getLabelProperty();
  }

  public final Map<String, String> getCustomPropList()
  {
    return _customPropList;
  }

  public final String getDestination()
  {

    String value = _destination;

    // Could be EL expression
    if (value != null && ContainerUtils.isValueReference(value))
    {
      // Value of action is EL method binding, so we
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
    }

    // Appending nodeId to URL so that we can identify the node
    // when getFocusRowKey() is called on the model.
    return value != null ? value + "?nodeId=" + getUniqueId() : value;

  }

  public final String getTargetFrame()
  {
    String value = _targetFrame;

    // Could be EL expression
    if (value != null && ContainerUtils.isValueReference(value))
    {
      // Value of destination is EL value binding, so we
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
    }

    return value;
  }

  public final String getActionListener()
  {
    String value = _actionListener;

    // Could be EL expression
    if (value != null && ContainerUtils.isValueReference(value))
    {
      // Value of action is EL method binding, so we
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
    }

    return value;
  }

  public final String getLaunchListener()
  {

    String value = _launchListener;

    // Could be EL expression
    if (value != null && ContainerUtils.isValueReference(value))
    {
      // Value of action is EL method binding, so we
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
    }

    return value;
  }

  public final String getReturnListener()
  {
    String value = _returnListener;

    // Could be EL expression
    if (value != null && ContainerUtils.isValueReference(value))
    {
      // Value of action is EL method binding, so we
      // need to evaluate it
      value = MenuUtils.getBoundValue(value, String.class);
    }

    return value;

  }

  public final boolean getImmediate()
  {
    boolean immediate = MenuUtils.evalBoolean(_immediateStr, false);
    return immediate;
  }

  public final boolean getUseWindow()
  {
    boolean useWindow = MenuUtils.evalBoolean(_useWindowStr, false);
    return useWindow;
  }

  public final int getWindowHeight()
  {
    int windowHeight = MenuUtils.evalInt(_windowHeightStr);
    return windowHeight;
  }

  public final int getWindowWidth()
  {
    int windowWidth = MenuUtils.evalInt(_windowWidthStr);
    return windowWidth;
  }

  public final String getLabel()
  {
    if (_bundleKey != null && _bundleName != null)
    {
      // Load the resource bundle based on the locale of the
      // current request. If the locale has not changed, this
      // method just returns.
      MenuUtils.loadBundle(_bundleName, _bundleKey + getHandlerId());
    }

    if (_label != null && ContainerUtils.isValueReference(_label))
    {
      // do not set _label to the evaluated EL.
      // It may change at times in the EL.
      return _evalElStr(_label);
    }
    if (_label == null && _labelAndAccessKey != null)
    {
      int ampIdx = 0;
      String labelAndAccessKeyEval = null;
      String labelAndAccessKey = _labelAndAccessKey;
      String label;
      if (ContainerUtils.isValueReference(labelAndAccessKey))
      {
        labelAndAccessKeyEval = _evalElStr(labelAndAccessKey);
      } else
      {
        labelAndAccessKeyEval = labelAndAccessKey;
      }

      String accessKey;
      if (labelAndAccessKeyEval == null ||
          (ampIdx = labelAndAccessKeyEval.indexOf('&')) == -1)
      {
        // String is null or a label w/o an accesskey
        label = labelAndAccessKeyEval;
      } else if (ampIdx == (labelAndAccessKeyEval.length() - 1))
      {
        // & is last character, strip it.
        label = labelAndAccessKeyEval.substring(0, ampIdx);
      } else
      {
        // We have a string with an accessKey somewhere
        char[] keyArray = labelAndAccessKeyEval.toCharArray();
        int len = labelAndAccessKeyEval.length();
        char[] keyArray2 = new char[len];
        int i, j = 0;
        boolean accessKeyFound = false;

        for (i = 0, j = 0; i < len; i++, j++)
        {
          if (keyArray[i] == '&')
          {
            i++;

            if (!accessKeyFound && keyArray[i] != '&')
            {
              // We have our accessKey
              accessKey = labelAndAccessKeyEval.substring(i, i + 1);
              accessKeyFound = true;
            }
          }

          keyArray2[j] = keyArray[i];
        }

        String label1 = new String(keyArray2, 0, j);
        label = label1;
      }
      return label;

    }
    return _label;
  }

  public final String getIcon()
  {
    return MenuUtils.evalString(_icon);
  }

  public final List<MenuNode> getChildren()
  {
    return _children;
  }

  public void setChildren(List<MenuNode> children)
  {
    _children = children;
  }

  public final String getFocusViewId()
  {
    return _focusViewId;
  }

  public final boolean getRendered()
  {
    boolean rendered = MenuUtils.evalBoolean(_renderedStr, true);
    return rendered;
  }

  public final boolean getDisabled()
  {
    boolean disabled = MenuUtils.evalBoolean(_disabledStr, false);
    return disabled;
  }

  public final boolean getVisible()
  {
    boolean visible = MenuUtils.evalBoolean(_visibleStr, true);
    return visible;
  }

  public final boolean getReadOnly()
  {
    boolean readOnly = MenuUtils.evalBoolean(_readOnlyStr, false);
    return readOnly;
  }

  protected final String getHandlerId()
  {
    return _handlerId;
  }

  public final String getBundleKey()
  {
    return _bundleKey;
  }

  public final String getBundleName()
  {
    return _bundleName;
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
      methodExpression.invoke(context, new Object[]
      { event });
    }

  }

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

  public final char getAccessKey()
  {
    if (_accessKey == null && _labelAndAccessKey != null)
    {
      int ampIdx = 0;
      String labelAndAccessKeyEval = null;
      String labelAndAccessKey = _labelAndAccessKey;
      String label;
      if (ContainerUtils.isValueReference(labelAndAccessKey))
      {
        labelAndAccessKeyEval = _evalElStr(labelAndAccessKey);
      } 
      else
      {
        labelAndAccessKeyEval = labelAndAccessKey;
      }

      String accessKey = null;
      if (labelAndAccessKeyEval == null ||
          (ampIdx = labelAndAccessKeyEval.indexOf('&')) == -1)
      {
        // String is null or a label w/o an accesskey
        label = labelAndAccessKeyEval;
      } else if (ampIdx == (labelAndAccessKeyEval.length() - 1))
      {
        // & is last character, strip it.
        label = labelAndAccessKeyEval.substring(0, ampIdx);
      } else
      {
        // We have a string with an accessKey somewhere
        char[] keyArray = labelAndAccessKeyEval.toCharArray();
        int len = labelAndAccessKeyEval.length();
        char[] keyArray2 = new char[len];
        int i, j = 0;
        boolean accessKeyFound = false;

        for (i = 0, j = 0; i < len; i++, j++)
        {
          if (keyArray[i] == '&')
          {
            i++;

            if (!accessKeyFound && keyArray[i] != '&')
            {
              // We have our accessKey
              accessKey = labelAndAccessKeyEval.substring(i, i + 1);
              accessKeyFound = true;
            }
          }

          keyArray2[j] = keyArray[i];
        }

        String label1 = new String(keyArray2, 0, j);
        label = label1;
      }
      return (accessKey != null)? accessKey.charAt(0):'\0';

    }
    else 
    {
      String accessKeyStr = MenuUtils.evalString(_accessKey);
      if (accessKeyStr == null || accessKeyStr.length() > 1)
        return '\0';
      return accessKeyStr.charAt(0);
    }
    
  }

  public final String getId()
  {
    return _id;
  }

  public final String getModelId()
  {
    return _modelId;
  }

  public final String getUniqueId()
  {
    return _uniqueId;
  }
  
  public final String getLabelAndAccessKey()
  {
    String labelAndAcessKeyEval;
    if (_labelAndAccessKey != null)
    {
      int ampIdx = 0;
      String labelAndAccessKeyEval = null;
      String labelAndAccessKey = _labelAndAccessKey;
      String label;
      if (_bundleKey != null && _bundleName != null)
      {
        // Load the resource bundle based on the locale of the
        // current request. If the locale has not changed, this
        // method just returns.
        MenuUtils.loadBundle(_bundleName, _bundleKey + getHandlerId());
      }
      if (ContainerUtils.isValueReference(labelAndAccessKey))
      {
        labelAndAccessKeyEval = _evalElStr(labelAndAccessKey);
      } else
      {
        labelAndAccessKeyEval = labelAndAccessKey;
      }

      String accessKey = null;
      if (labelAndAccessKeyEval == null ||
          (ampIdx = labelAndAccessKeyEval.indexOf('&')) == -1)
      {
        // String is null or a label w/o an accesskey
        label = labelAndAccessKeyEval;
      } else if (ampIdx == (labelAndAccessKeyEval.length() - 1))
      {
        // & is last character, strip it.
        label = labelAndAccessKeyEval.substring(0, ampIdx);
      } else
      {
        // We have a string with an accessKey somewhere
        char[] keyArray = labelAndAccessKeyEval.toCharArray();
        int len = labelAndAccessKeyEval.length();
        char[] keyArray2 = new char[len];
        int i, j = 0;
        boolean accessKeyFound = false;

        for (i = 0, j = 0; i < len; i++, j++)
        {
          if (keyArray[i] == '&')
          {
            i++;

            if (!accessKeyFound && keyArray[i] != '&')
            {
              // We have our accessKey
              accessKey = labelAndAccessKeyEval.substring(i, i + 1);
              accessKeyFound = true;
            }
          }

          keyArray2[j] = keyArray[i];
        }

        String label1 = new String(keyArray2, 0, j);
        label = label1;
      }
      // https://issues.apache.org/jira/browse/TRINIDAD-1588
      if (accessKey == null) {
          return label;
      }
      return _joinLabelAndAccessKey(label, accessKey);
    }
    return null;
  }

  public final boolean getDefaultFocusPath()
  {
    boolean defaultFocusPath =
        MenuUtils.evalBoolean(_defaultFocusPathStr, false);
    return defaultFocusPath;
  }

  public final String getRootModelKey()
  {
    return _rootModelKey;
  }

  public final int getRootId()
  {
    return _rootId;
  }

  private String _evalElStr(String str)
  {
    if (str == null)
      return null;

    String keystr =
        MenuUtils.stringReplaceFirst(str.trim(), _bundleKey, _bundleKey +
            getHandlerId());
    String elVal = MenuUtils.getBoundValue(keystr, String.class);
    return elVal;
  }

  private String _joinLabelAndAccessKey(String label, String accessKey)
  {
    char[] keyArray = label.toCharArray();
    int len = label.length();
    int lentimes2 = len * 2;
    char[] keyArray2 = new char[lentimes2];
    int i, j = 0;
    boolean accessKeyFound = false;

    // find the first occurrence of a single Ampersand
    for (i = 0, j = 0; i < len; i++, j++)
    {
      // AccessKey
      if (keyArray[i] == accessKey.charAt(0) && !accessKeyFound)
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

  private final String _icon;
  private List<MenuNode> _children = null;
  private final String _focusViewId;
  private final String _renderedStr;
  private final String _disabledStr;
  private final String _visibleStr;
  private final String _readOnlyStr;
  private final String _handlerId;
  private final String _bundleKey;
  private final String _bundleName;
  private final String _accessKey;
  private final String _id;
  private final String _modelId;
  private final String _labelAndAccessKey;
  private final String _defaultFocusPathStr;
  private final String _uniqueId;

  // Root Menu model's Request Map Key
  private final String _rootModelKey;

  private final int _rootId;

  private final Map<String, String> _customPropList;

  private final String _destination;
  private final String _targetFrame;
  private final String _action;
  private final String _actionListener;
  private final String _launchListener;
  private final String _returnListener;
  private final String _immediateStr;
  private final String _useWindowStr;
  private final String _windowHeightStr;
  private final String _windowWidthStr;

  private final String _label;

}
