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

import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.List;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ContainerUtils;

/**
 * This class is a thread safe version of GroupNode class.
 * It replicates most of the code in GroupNode but makes
 * sure it does not modify state of the object.
 * 
 * Therefore multiple request threads can access the
 * properties of the objects of this class,in a thread safe
 * manner.
 * 
 * Please note that setters should not be called on objects
 * of this class.Objects of this class are fully initialized
 * on construction.
 * @deprecated Use org.apache.myfaces.trinidad.menu.ImmutableGroupNode instead of this one.
 */

@Deprecated
public class ImmutableGroupNode extends GroupNode
{
  
  public ImmutableGroupNode(GroupNode node)
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
    _label = node.getLabelProperty();
    _idref = node.getIdRef();
    _idrefList = node.getIdRefListProperty();
  }
  
  
  public String getIdRef()
  {
    return _idref;
  }
  
  public MenuNode getRefNode()
  {
    MenuNode refNode = null;

    
    // Get idrefList
    String[] idrefList = _idrefList;

    // get group node's children
    List<MenuNode> children = getChildren();

    // Traverse the list. Do the following:
    //    o get Node from Model's hashMap of nodes and ids
    //    o check attributes (rendered, disabled, readOnly)
    //    o if they are ok, return the node
    for (int i=0; i < Array.getLength(idrefList); i++)
    {
      Iterator<MenuNode> childIter = children.iterator();

      // All node "id" attribute values had the node's
      // system hashcode id appended to the id when
      // placed in the model's idNodeMap.
      //
      // Each id in the idreflist of a group node does
      // NOT have this node sys id appended it to it
      // and needs to or we won't find the group's
      // ref node.
      //
      // Since group nodes can only point to one of
      // its children, we iterate through them, get
      // their sys id and append it to idref until
      // we find a match (or not).
      while (childIter.hasNext())
      {
        MenuNode childNode = childIter.next();
        String modelId = childNode.getModelId();

        // Need to append mode's sys id here to create a
        // unique id.
        String refNodeId = idrefList[i] + modelId;

        refNode = (MenuNode) getRootModel().getNode(refNodeId);

        // if nothing found, move on to the next child
        if (refNode != null)
         break;
      }

      if (refNode == null)
        continue;

      // Check the attributes of the found node
      if (   !refNode.getRendered()
          ||  refNode.getDisabled()
          ||  refNode.getReadOnly()
          || !refNode.getVisible()
         )
      {
        refNode = null;
        continue;
      }

      // Ok, we have a valid RefNode
      break;
    }

    // If no valid node is found,
    // log an error
    if (refNode == null)
    {
        _LOG.severe("GroupNode " + getLabel() + " refers to no valid node.\n");
        return null;
    }

    return refNode;
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

  public final String getLabelAndAccessKey()
  {
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
      if(accessKey == null)
        return label;
      
      return _joinLabelAndAccessKey(label, accessKey);
    }
    return null;
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
  
  //TODO make this work
//  public final String getLabelAndAccessKey()
//  {
//    String labelAndAcessKeyEval;
//    if ( _labelAndAccessKey != null
//        && ContainerUtils.isValueReference(_labelAndAccessKey)
//       )
//    {
//       labelAndAcessKeyEval= _evalElStr(_labelAndAccessKey);
//    }
//    else
//    {
//      labelAndAcessKeyEval = _labelAndAccessKey;
//    }
//    
//    
//  }

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

  

  private final String _label;
  
  
  private final String   _idref ;
  private final String[] _idrefList ;

  private final static TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ImmutableGroupNode.class);
}
