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

import java.util.ArrayList;
import java.util.List;

import org.apache.myfaces.trinidad.component.UIXHierarchy;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ModelRendererUtils.java#0 $) $Date: 10-nov-2005.18:54:03 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ModelRendererUtils 
{
  
  /**
   * 
   * @param component 
   * @param startDepth 
   * @return whether or not a path was set.
   */
  public static final boolean setNewPath(
    UIXHierarchy   component,
    int              startDepth,
    Object           focusRowKey
  )
  {
    return setNewPath(component, startDepth, focusRowKey, true);
  }

  /**
   * 
   * @param component 
   * @param startDepth 
   * @return whether or not a path was set.
   */
  public static final boolean setNewPath(
    UIXHierarchy   component,
    int              startDepth,
    Object           focusRowKey,
    boolean          useFocusChildren
  )
  {
    boolean isNewPath = false;
    
    if (focusRowKey != null )  
    {
      List<Object> focusPath = component.getAllAncestorContainerRowKeys(focusRowKey);
      focusPath = new ArrayList<Object>(focusPath);
      focusPath.add(focusRowKey);
      
      int focusSize =  focusPath.size();
      if ( focusSize > startDepth )
      {
        isNewPath = true;
        component.setRowKey(focusPath.get(startDepth));  
      }
      else if ( useFocusChildren && focusSize == startDepth )
      {
        isNewPath = true;
        component.setRowKey(focusRowKey);  
        component.enterContainer();
      }
    }      
    else  
    {      
      if (startDepth  == 0 && useFocusChildren)
      {
        isNewPath = true;
        component.setRowKey(null); 
      }
    }
    
    return isNewPath;
  }


  /**
   * Checks to see whether the level is empty 
   */
  public static final boolean isEmpty(
    UIXHierarchy    component,
    int              startDepth,
    Object           focusPath
    )
  {               
    Object oldPath = component.getRowKey();
    boolean isNewPath = setNewPath(component, startDepth, focusPath); 
    
    if (!isNewPath)
      return true;   
  
    if (component.getRowCount() > 0)
    {
      component.setRowKey(oldPath);
      return false;
    }
    
    component.setRowKey(oldPath);
    return true;
  }
  
}
