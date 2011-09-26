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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationTree;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ModelRendererUtils;

/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/NavigationTreeRenderer.java#0 $) $Date: 10-nov-2005.18:55:25 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class NavigationTreeRenderer extends TreeRenderer
{
  @Override
  protected RowKeySet getExpandedRowKeys(UIXHierarchy tree)
  {
    return ((UIXNavigationTree)tree).getDisclosedRowKeys();    
  }
  
  @Override
  protected Map<Object, Boolean> getSelectedPaths(Object focusRowKey)
  {
    if ( focusRowKey == null)
      return new HashMap<Object, Boolean>(0);

    // TODO method must be passed the component so that
    // proper APIs can be used instead of casting to List:
    List<?> focusPath = (List<?>) focusRowKey;
    Map<Object, Boolean> selectedPaths = 
      new HashMap<Object, Boolean>(focusPath.size());

    for ( int i = 0; i < focusPath.size(); i++)
    {
      List<?> focusSubPath = focusPath.subList(0,i + 1);
      selectedPaths.put(focusSubPath, Boolean.TRUE);
    }

    return selectedPaths;
  }

  @Override
  protected UINode getIcon()
  {
    return null;
  }


  // return whether to continue with rendering
  @Override
  protected boolean setInitialPath(
    UIXRenderingContext context,
    UINode           node,
    UIXHierarchy     tree)
  {
    int startLevel = getIntAttributeValue(context, node, START_LEVEL_ATTR, 0);
    return ModelRendererUtils.setNewPath(tree, startLevel, tree.getFocusRowKey());
  }


  /** Map one key to another.
   * (We use this method when we subclass a renderer, but we want
   * to have our own resource keys (e.g., translation keys) . 
   * This is easier and quicker than
   * storing the translation map on the RenderingContext since in that
   * case we'd have to save/restore the map if it is already set. Since
   * we are simply subclassing here, we can use this method).
   * @param key
   * @return String , key mapped to a new String.
   */
  @Override
  protected String mapKey(String key)
  {
    return _RESOURCE_KEY_MAP.get(key);
  }

 private static final Map<String, String> _RESOURCE_KEY_MAP;
  static
  {
    _RESOURCE_KEY_MAP  =  new HashMap<String, String> ();
    _RESOURCE_KEY_MAP.put("af_tree.DISABLED_COLLAPSE_TIP",
                              "af_navigationTree.DISABLED_COLLAPSE_TIP");
    _RESOURCE_KEY_MAP.put("af_tree.COLLAPSE_TIP",
                              "af_navigationTree.COLLAPSE_TIP");
    _RESOURCE_KEY_MAP.put("af_tree.EXPAND_TIP",
                              "af_navigationTree.EXPAND_TIP");
    _RESOURCE_KEY_MAP.put("af_tree.FOLDER_TIP",
                              "af_navigationTree.FOLDER_TIP");
    _RESOURCE_KEY_MAP.put("af_tree.NODE_LEVEL",
                              "af_navigationTree.NODE_LEVEL");
  }

}
