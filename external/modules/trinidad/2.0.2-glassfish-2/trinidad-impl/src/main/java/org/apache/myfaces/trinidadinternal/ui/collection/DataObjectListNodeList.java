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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.DataObject;
import org.apache.myfaces.trinidadinternal.ui.data.DataObjectList;

import org.apache.myfaces.trinidadinternal.ui.data.bean.BeanAdapterUtils;


/**
 * DataObjectListNodeList is one way for developers to
 * databind the <em>number</em> of children of a bean, and the contents
 * of each child.  When set, the bean will render all the children
 * in the UINodeList for each <code>DataObject</code> in the provided
 * DataObjectList.  For each pass, the <code>RenderingContext</code> will
 * automatically return the correct <code>DataObject</code> from
 * <code>RenderingContext.getCurrentDataObject()</code>.
 * <p>
 * A DataObjectListNodeList can either contain a single UINode
 * or an entire UINodeList.  It can pull the DataObjectList either
 * from an explicitly set instance or from a BoundValue.
 * <p>
 * <h4>Example:</h4>
 *
 * <p>A client wants to provide a single <code>BreadCrumbsBean</code>
 * that will display the full series of bread crumbs off a single source
 * of data.  First, set up a <code>DataObjectList</code>.  For real-world
 * code, you might implement the (simple!) <code>DataObjectList</code>
 * interface from scratch to communicate directly with your data store,
 * but here's a hardcoded example:
 *
 * <pre>
 * private DataObjectList _getBreadCrumbsSource()
 * {
 *   String[] titles = {"Home", "Shopping", ... };
 *   String[] urls   = {"http:...", "...", ... };
 *   DataObject[] objects = new DataObject[titles.length];
 *
 *  // Build one DataObject per child
 *  for (int i = 0; i < titles.length; i++)
 *  {
 *    DictionaryData dd = new DictionaryData();
 *    dd.put("title", titles[i]);
 *    dd.put("url", urls[i]);
 *    objects[i] = dd;
 *  }
 *
 *   // Create a DataObjectList for the whole group
 *  return new ArrayDataSet(objects);
 * }
 * </pre>
 *
 * Second, build up the <code>BreadCrumbsBean</code>, using a
 * <code>DataObjectListNodeList</code> pointing it at a
 * single <code>LinkBean</code> and the <code>DataObjectList</code>
 * you just created.
 *
 *<pre>
 *  private UINode _getDataboundBreadCrumbs()
 *  {
 *    // Create the link that will be used to display each
 *    // "crumb".  Bind the attributes of each
 *    LinkBean link = new LinkBean();
 *    link.setTextBinding("title");
 *    link.setDestinationBinding("url");
 *
 *    // In a real application, you'd probably use a BoundValue
 *    // that returns a DataObjectList, instead of setting the
 *    // DataObjectList directly.
 *    UINodeList nodeList =
 *              new DataObjectListNodeList(link,
 *                                         _getBreadCrumbsSource());
 *
 *    BreadCrumbsBean crumbs = new BreadCrumbsBean();
 *    crumbs.setIndexedNodeList(nodeList);
 *
 *    // And done!
 *    return crumbs;
 *  }
 *</pre>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/DataObjectListNodeList.java#0 $) $Date: 15-nov-2005.19:26:47 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class DataObjectListNodeList extends UINodeListProxy
{
  /**
   * Creates an empty DataObjectListNodeList bound
   * to a DataObjectList.
   * @param childData a DataObjectList
   */
  public DataObjectListNodeList(DataObjectList childData)
  {
    this(new ArrayUINodeList(), childData);
  }

  /**
   * Creates an empty DataObjectListNodeList.
   * @param boundData a BoundValue that will be used
   *   to retrieve the DataObjectList at render time
   */
  public DataObjectListNodeList(BoundValue boundData)
  {
    this(new ArrayUINodeList(), boundData);
  }

  /**
   * Creates a DataObjectListNodeList wrapping a single
   * node and pulling its data from an explicitly set DataObjectList.
   * <p>
   * @param baseNode a single node
   * @param childData a DataObjectList
   */
  public DataObjectListNodeList(
    UINode         baseNode,
    DataObjectList childData
    )
  {
    this(_createSingleNodeList(baseNode), childData);
  }


  /**
   * Creates a DataObjectListNodeList wrapping an entire UINodeList
   * and pulling its data from an explicitly set DataObjectList.
   * <p>
   * @param baseNodes a list of UINodes to wrap
   * @param childData a DataObjectList
   */
  public DataObjectListNodeList(
    UINodeList     baseNodes,
    DataObjectList childData
    )
  {
    if ((baseNodes == null) || (childData == null))
      throw new IllegalArgumentException();

    _baseNodes = baseNodes;
    _childData = childData;
  }


  /**
   * Creates a DataObjectListNodeList wrapping an single UINode.
   * <p>
   * @param baseNode a single node
   * @param boundData a BoundValue that will be used
   *   to retrieve the DataObjectList at render time
   */
  public DataObjectListNodeList(
    UINode         baseNode,
    BoundValue     boundData
    )
  {
    this(_createSingleNodeList(baseNode), boundData);
  }


  /**
   * Creates a DataObjectListNodeList wrapping an entire UINodeList.
   * <p>
   * @param baseNodes a list of UINodes to wrap
   * @param boundData a BoundValue that will be used
   *   to retrieve the DataObjectList at render time
   */
  public DataObjectListNodeList(
    UINodeList     baseNodes,
    BoundValue     boundData
    )
  {
    if ((baseNodes == null) || (boundData == null))
      throw new IllegalArgumentException();

    _baseNodes = baseNodes;
    _boundData = boundData;
  }


  /**
   * Returns the size of the DataObjectListNodeList;  returns
   * (number of contained nodes) * (size of the DataObjectList).
   */
  @Override
  public int size(UIXRenderingContext context)
  {
    DataObjectList dol = getDataObjectList(context);
    if (dol == null)
      return 0;
    return dol.getLength() * super.size(context);
  }

  @Override
  public UINode getUINode(
    UIXRenderingContext context,
    int index
    )
  {
    DataObjectList dol = getDataObjectList(context);
    if (dol == null)
      throw new IndexOutOfBoundsException();

    int baseCount   = super.size(context);
    if ((index < 0) || (index >= (baseCount * dol.getLength())))
      throw new IndexOutOfBoundsException();

    DataObject data = dol.getItem(index / baseCount);
    UINode     node = super.getUINode(context, index % baseCount);

    if (data == null)
    {
      return node;
    }
    else
    {
      DataObject current;
      if (context == null)
        current = null;
      else
        current = context.getCurrentDataObject();

      // =-=AEW Because of how UIX iterates, we inevitably get
      // each node twice.  Unfortunately, we get the nodes in what may
      // seem a rather odd order:  0, 1, 0, 2, 1, 3, 2, 4, 3, 5, 4...
      // This means that it isn't possible to simply cache the _last_
      // retrieved proxy and optimize creations here by 50%.
      // The "problem" is in BaseRenderer, which wants to let each
      // indexed child rendering know not just the current index,
      // but also the previous _actually-rendered_ index, and the
      // next _to-be-rendered_ index.
      // A possible solution would be to maintain a short cache (3 elements
      // long?) of created UINodes.  If we ever discover that this method
      // has become a bottleneck, this would be the way to go.
      return DataObjectUINodeProxy.createWrappedNode(node, data, current);
    }
  }

  @Override
  public Object clone()
  {
    DataObjectListNodeList cloned = (DataObjectListNodeList) super.clone();
    cloned._baseNodes = (UINodeList) cloned._baseNodes.clone();
    return cloned;
  }


  /**
   * Returns the DataObjectList to be used.
   */
  protected DataObjectList getDataObjectList(UIXRenderingContext context)
  {
    if (_boundData != null)
    {
      // Get a cached copy of the DataObjectList.  We're using
      // the DataObjectListNodeList itself as the key.
      // Why use a local property instead of a global property?
      // Sometimes, we walk the tree from high up (e.g. looking
      // for headers for quicklinks).  These walks usually don't
      // resolve dataScopes, so they don't necessarily operate
      // with the correct data.  It would be _bad_ if a tree walk
      // that got the wrong data caused the actual rendering
      // to also use the wrong data.  Local properties dodge this.
      if (context != null)
      {
        Object o = context.getLocalProperty(0, this, _sNOT_SET_OBJECT);
        if (o != _sNOT_SET_OBJECT)
          return ((DataObjectList) o);
      }

      DataObjectList list =
        BeanAdapterUtils.getAdapterList(context,
                                        _boundData.getValue(context));
      if (context != null)
      {
        context.setLocalProperty(this, list);
      }

      return list;
    }
    else
    {
      return _childData;
    }
  }

  @Override
  protected UINodeList getUINodeList(UIXRenderingContext context)
  {
    return _baseNodes;
  }

  static private UINodeList _createSingleNodeList(UINode baseNode)
  {
    UINodeList nodeList = new ArrayUINodeList(1);
    nodeList.addUINode(baseNode);
    return nodeList;
  }


  private UINodeList     _baseNodes;
  private DataObjectList _childData;
  private BoundValue     _boundData;

  // Null object, used to discriminate between "not-set" and "set-to-null"
  static private final Object _sNOT_SET_OBJECT = new Object();
}

