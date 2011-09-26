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
package org.apache.myfaces.trinidadinternal.ui.collection;

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UINodeProxy;

import org.apache.myfaces.trinidadinternal.ui.data.DataObject;


/**
 * General strategy:  set the current DataObject before
 * any call to getAttributeValue(), getNamedChild(), getIndexedChild(),
 * or getIndexedChildCount().  Wrap any children (indexed or named) to
 * do the same thing.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/DataObjectUINodeProxy.java#1 $) $Date: 11-nov-2005.14:59:40 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
class DataObjectUINodeProxy extends UINodeProxy
{
  static public UINode createWrappedNode(
    UINode     baseNode,
    DataObject data,
    DataObject current)
  {
    if (baseNode == null)
      return null;

    // Never wrap twice - this happens if there's a
    // a DataObjectListNodeList inside of another DataObjectListNodeList.
    // When this happens, we want the inner node to "win"
    if (baseNode instanceof DataObjectUINodeProxy)
    {
      // But - and here's where things get subtle - the "inner node"
      // was set up thinking the current DataObject would be
      // the current DataObject set up by this outer DataObjectListNodeList
      // (since that DOLNL set up the DataObjectUINodeProxy that just
      // called getIndexed/NamedChild()).  But, instead, it'll
      // see the current DataObject that was in use outside of that outer
      // DOLNL.  So, to stop it from getting confused, overwrite
      // the current DataObject stored in that UINodeProxy.
      //
      ((DataObjectUINodeProxy) baseNode)._current = current;

      return baseNode;
    }

    return new DataObjectUINodeProxy(baseNode, data, current);
  }

  private DataObjectUINodeProxy(
    UINode     baseNode,
    DataObject data,
    DataObject current
    )
  {
    if (baseNode == null)
      throw new NullPointerException();

    _baseNode = baseNode;
    _data     = data;
    _current  = current;
  }

  @Override
  public Object getAttributeValue(
    UIXRenderingContext context,
    AttributeKey     attrKey
    )
  {
    Object value;

    if (context == null)
    {
      value = super.getAttributeValue(null, attrKey);
    }
    else
    {
      // If the current data object has changed, don't set it.
      DataObject oldDataObject = context.getCurrentDataObject();
      if (oldDataObject != _current)
      {
        value = super.getAttributeValue(context, attrKey);
      }
      else
      {
        context.setCurrentDataObject(_data);
        value = super.getAttributeValue(context, attrKey);
        context.setCurrentDataObject(oldDataObject);
      }
    }

    return value;
  }

  @Override
  public int getIndexedChildCount(UIXRenderingContext context)
  {
    int count;

    if (context == null)
    {
      count = super.getIndexedChildCount(null);
    }
    else
    {
      // If the current data object has changed, don't set it.
      DataObject oldDataObject = context.getCurrentDataObject();
      if (oldDataObject != _current)
      {
        count = super.getIndexedChildCount(context);
      }
      else
      {
        context.setCurrentDataObject(_data);
        count = super.getIndexedChildCount(context);
        context.setCurrentDataObject(oldDataObject);
      }
    }

    return count;
  }

  @Override
  public void render(UIXRenderingContext context, UINode node)
    throws IOException
  {
    DataObject oldDataObject = context.getCurrentDataObject();
    if (oldDataObject != _current)
    {
      super.render(context, node);
    }
    else
    {
      context.setCurrentDataObject(_data);
      super.render(context, node);
      context.setCurrentDataObject(oldDataObject);
    }
  }

  @Override
  public UINode getIndexedChild(
    UIXRenderingContext context,
    int              childIndex)
  {
    UINode child;

    if (context == null)
    {
      child = createWrappedNode(super.getIndexedChild(null, childIndex),
                                _data,
                                _current);
    }
    else
    {
      // If the current data object has changed, don't set it.
      DataObject oldDataObject = context.getCurrentDataObject();
      if (oldDataObject != _current)
      {
        child = super.getIndexedChild(context, childIndex);
      }
      else
      {
        context.setCurrentDataObject(_data);
        child = createWrappedNode(super.getIndexedChild(context, childIndex),
                                  _data,
                                  _current);
        context.setCurrentDataObject(oldDataObject);
      }
    }

    return child;
  }

  @Override
  public UINode getNamedChild(
    UIXRenderingContext context,
    String           childName)
  {
    UINode child;

    if (context == null)
    {
      child = createWrappedNode(super.getNamedChild(null, childName),
                                _data,
                                _current);
    }
    else
    {
      // If the current data object has changed, don't set it.
      DataObject oldDataObject = context.getCurrentDataObject();
      if (oldDataObject != _current)
      {
        child = super.getNamedChild(context, childName);
      }
      else
      {
        context.setCurrentDataObject(_data);
        child = createWrappedNode(super.getNamedChild(context, childName),
                                  _data,
                                  _current);
        context.setCurrentDataObject(oldDataObject);
      }
    }

    return child;
  }

  @Override
  protected UINode getUINode()
  {
    return _baseNode;
  }


  private UINode     _baseNode;
  private DataObject _data;
  private DataObject _current;
}

