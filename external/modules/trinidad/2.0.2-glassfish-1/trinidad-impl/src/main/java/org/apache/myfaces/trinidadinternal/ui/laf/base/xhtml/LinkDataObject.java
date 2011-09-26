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

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.DataObject;
import org.apache.myfaces.trinidadinternal.ui.data.DataObjectList;
import org.apache.myfaces.trinidadinternal.ui.data.ListDataObjectList;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;

/**
 * used by GlobalButtonRenderer and TabBarRenderer
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/LinkDataObject.java#0 $) $Date: 10-nov-2005.18:53:59 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public final class LinkDataObject implements DataObject
{


  static public DataObjectList getLinkDataList(
    UIXRenderingContext context,
    UINode           parent) throws IOException
  {
    int childCount = parent.getIndexedChildCount(context);

    ListDataObjectList list = new ListDataObjectList(childCount);
    BaseLafUtils.setRenderingProperty(context, _DATA_OBJECT_LIST_KEY, list);

    __setDataObjectUsedMode(context, true);
    for (int childIndex = 0; childIndex < childCount; childIndex++)
    {
      UINode child = parent.getIndexedChild(context, childIndex);
      // needed for TrainRenderer. It is important to know what child index
      // this is, not just the index of the visible children.
      BaseLafUtils.setRenderingProperty(context,
                                        _CURRENT_INDEX_KEY,
                                        childIndex);

      if ((child != null) &&
          !Boolean.FALSE.equals(
             child.getAttributeValue(context, UIConstants.RENDERED_ATTR)))
      {
        context.pushChild(child, null, childIndex);
        context.pushRenderedChild(context, child);

        try
        {
          child.render(context);
        }
        finally
        {
          context.popRenderedChild(context);
          context.popChild();
        }
      }
      BaseLafUtils.setRenderingProperty(context, _CURRENT_INDEX_KEY, null);
    }

    BaseLafUtils.setRenderingProperty(context, _DATA_OBJECT_LIST_KEY, null);

    __setDataObjectUsedMode(context, false);

    return list;
  }





 static public DataObjectList getLinkDataList(
    UIXRenderingContext context,
    UINode           parent,
    UIXHierarchy    component,
    UINode           stamp,
    boolean          checkStampType) throws IOException
  {
    int indexedChildCount = parent.getIndexedChildCount(context);
    int modelChildCount = component.getRowCount();
    int totalChildCount = indexedChildCount + modelChildCount;
    int selectedIndex = component.getRowIndex();

    ListDataObjectList list = new ListDataObjectList(totalChildCount);
    BaseLafUtils.setRenderingProperty(context, _DATA_OBJECT_LIST_KEY, list);
    __setDataObjectUsedMode(context, true);

    // need newSelectedIndex and newIndex because the selectedIndex passed in
    // ignores what is and isn't rendered, but the list only has
    // what is rendered, so newIndex and newSelectedIndex deal with that.
    int newSelectedIndex = -1;
    int newIndex = 0;

    for (int i = 0; i < modelChildCount; i++)
    {
      component.setRowIndex(i);

      boolean includeStamp = true;
      /*if (checkStampType)
      {
        Object type = stamp.getAttributeValue(context, UIConstants.TYPE_ATTR);
        if (CoreCommandNavigationItem.TYPE_GLOBAL.equals(type))
          includeStamp = false;
      }*/

      if (includeStamp)
      {
        // needed for TrainRenderer. It is important to know what child index
        // this is, not just the index of the visible children.
        LinkDataObject.__setCurrentIndex(context, i);

        if ((stamp != null) &&
            !Boolean.FALSE.equals(
               stamp.getAttributeValue(context, UIConstants.RENDERED_ATTR)))
        {
          context.pushChild(stamp, null, i);
          context.pushRenderedChild(context, stamp);

          try
          {
            if ( i == selectedIndex)
              newSelectedIndex = newIndex;
            stamp.render(context);
          }
          finally
          {
            context.popRenderedChild(context);
            context.popChild();
          }

          newIndex++;
        }
        LinkDataObject.__setCurrentIndex(context, null);
      }
    }

    if ( newSelectedIndex > -1)
    {
      LinkDataObject ldo = (LinkDataObject)list.getItem(newSelectedIndex);
      ldo.setSelected(true);
    }

    for (int childIndex = 0; childIndex < indexedChildCount; childIndex++)
    {
      UINode child = parent.getIndexedChild(context, childIndex);
      // needed for TrainRenderer. It is important to know what child index
      // this is, not just the index of the visible children.
      BaseLafUtils.setRenderingProperty(context,
                                        _CURRENT_INDEX_KEY,
                                        childIndex);

      if ((child != null) &&
          !Boolean.FALSE.equals(
             child.getAttributeValue(context, UIConstants.RENDERED_ATTR)))
      {
        context.pushChild(child, null, childIndex);
        context.pushRenderedChild(context, child);

        try
        {
          child.render(context);
        }
        finally
        {
          context.popRenderedChild(context);
          context.popChild();
        }
      }
      BaseLafUtils.setRenderingProperty(context, _CURRENT_INDEX_KEY, null);
    }

    BaseLafUtils.setRenderingProperty(context, _DATA_OBJECT_LIST_KEY, null);
    __setDataObjectUsedMode(context, false);

    return list;
  }



  public LinkDataObject(Object id,
                        Object text,
                        Object shortDesc,
                        Object destination,
                        Object onClick,
                        Object targetFrame,
                        Object accessKey,
                        boolean isSelected,
                        boolean isDisabled)
  {
    this(id, text, shortDesc, destination, onClick,
         targetFrame, accessKey, isSelected, isDisabled, Integer.valueOf(0), null);
  }

  public LinkDataObject(Object id,
                        Object text,
                        Object shortDesc,
                        Object destination,
                        Object onClick,
                        Object targetFrame,
                        Object accessKey,
                        boolean isSelected,
                        boolean isDisabled,
                        Integer currentIndex,
                        UINode  node)
  {
    _id = id;
    _text = text;
    _shortDesc = shortDesc;
    _dest = destination;
    _onClick = onClick;
    _targetFrame = targetFrame;
    _accessKey = accessKey;
    setSelected(isSelected);
    _isDisabled = (isDisabled) ? Boolean.TRUE : Boolean.FALSE;
    _currentIndex = currentIndex;
    _node = node;
  }

  public void setSelected(boolean isSelected)
  {
    _isSelected = (isSelected) ? Boolean.TRUE : Boolean.FALSE;
  }

  public Object selectValue(UIXRenderingContext rc, Object key)
  {
    // this is only called by our renderers, so identity is okay here:
    if (UIConstants.TEXT_ATTR==key)         return _text;
    if (UIConstants.SHORT_DESC_ATTR==key)   return _shortDesc;
    if (UIConstants.ID_ATTR==key)           return _id;
    if (UIConstants.DESTINATION_ATTR==key)  return _dest;
    if (UIConstants.SELECTED_ATTR==key)     return _isSelected;
    if (UIConstants.DISABLED_ATTR==key)     return _isDisabled;
    if (UIConstants.TARGET_FRAME_ATTR==key) return _targetFrame;
    if (UIConstants.ON_CLICK_ATTR==key)     return _onClick;
    if (UIConstants.ACCESS_KEY_ATTR==key)   return _accessKey;
    if (UIConstants.CURRENT_INDEX_ATTR==key)   return _currentIndex;
    if (UIConstants.NODE_ATTR==key)   return _node;
    return null;
  }

  private final Object _id;
  private final Object _text;
  private final Object _shortDesc;
  private final Object _dest;
  private final Object _onClick;
  private final Object _targetFrame;
  private final Object _accessKey;
  private       Boolean _isSelected;
  private final Boolean _isDisabled;
  private final Integer _currentIndex;
  private final UINode  _node;

  /**************** static code follows ***********************/

  /**
   * this is used by TabBarRenderer to tell the LinkRenderer to put all its
   * data into LinkDataObjects
   * @param isUsed if true, then LinkRenderers will not render. set to false
   * to clear state
   */
  static void __setDataObjectUsedMode(UIXRenderingContext context,
                                             boolean used)
  {
    BaseLafUtils.setRenderingProperty(context,
                                      _KEY,
                                      used ? Boolean.TRUE : null);
  }

  /**
   * called by LinkRenderer
   */
  static boolean __isDataObjectUsedMode(UIXRenderingContext context)
  {
    return
      BaseLafUtils.getRenderingProperty(context, _KEY) != null;
  }

  /**
   * called by LinkRenderer
   */
  static void __setDataObject(UIXRenderingContext context,
                              LinkDataObject dobj)
  {
    ListDataObjectList dol = (ListDataObjectList)
      BaseLafUtils.getRenderingProperty(context, _DATA_OBJECT_LIST_KEY);
    dol.addItem(dobj);
  }

  /**
   * called by LinkRenderer
   */
  static Integer __getCurrentIndex(UIXRenderingContext context)
  {
    Object currentIndex =
      BaseLafUtils.getRenderingProperty(context, _CURRENT_INDEX_KEY, null);
    if (currentIndex == null)
      return -1;
    else
      return (Integer)currentIndex;

  }

  static void __setDOL(
    UIXRenderingContext context,
    DataObjectList   list)
  {
    BaseLafUtils.setRenderingProperty(context, _DATA_OBJECT_LIST_KEY, list);
  }

  static void __setCurrentIndex(
    UIXRenderingContext context,
    Integer          currIndex)
  {
    BaseLafUtils.setRenderingProperty(context, _CURRENT_INDEX_KEY, currIndex);
  }


  private static final Object _KEY = new Object();
  private static final Object _DATA_OBJECT_LIST_KEY = new Object();

  private static final Object _CURRENT_INDEX_KEY = new Object();
}
