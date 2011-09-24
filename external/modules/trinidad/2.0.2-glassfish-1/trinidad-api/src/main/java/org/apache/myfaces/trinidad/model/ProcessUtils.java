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
package org.apache.myfaces.trinidad.model;

import java.util.HashMap;
import java.util.Map;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

/**
  *  <p>
  *         There are two common scenarios for processes,
  *         "Plus One" and "Max Visited" which are explained below.
  *           <ul>
  *             <li>"Plus One" - from the current step the user can
  *             navigate to any previous page and the next page.
  *             If the user is on the 5th step of a process
  *             and goes back to step 2, then the user can only
  *             navigate from step 2 to step 1 and step 3.
  *             </li>
  *             <li>"Max Visited" - the user can navigate to the max
  *             visited page. If the user is currently on the max
  *             visited page then the user can also navigate to
  *             the next page. If the user is on the 5th step of a
  *             process and goes back to step 2, then the user can
  *             navigate from step 2 to steps 1, 2, 3, 4, and 5.
  *             </li>
  *           </ul>
  *
  *           </p>
  *           <p>
  *           A node in a process should be readOnly
  *           if that step of the process is not reachable from the current
  *           step.
  *           </p>
  *           <p>
  *           A node in a process should be immediate if the values
  *           in the current step don't need to be validated.
  *           </p>
  *           <p>
 */
public class ProcessUtils
{

//**********************************
// Plus One Process methods
//**********************************
 /**
   *
  *           <p>
  *           Determines immediate for a "plus one" process.
  *           Immediate will be true for any previous step, and false otherwise.
  *           For example if the user is on step 5
  *           and goes back to step 2, the user will have to come back
  *           to step 5 again, so the fields on page 5 don't need to
  *           be validated when going back to steps 1,2,3,4, but should be
  *           validated when going to step 6.
  *           </p>
  *
  *
   * @param model the model instance to use.
   * When the model is passed in a call to model.getPath should return the
   * path for the "current" node.
   * @param defaultReturnValue if the current and focus nodes aren't
   * siblings in the tree, this value will be returned.
   * @return whether or not the current node should be immediate.
   */
  public static boolean isImmediate(
    MenuModel model,
    boolean   defaultReturnValue
    )
  {
    Object focusPath = model.getFocusRowKey();
    Object currPath = model.getRowKey();
    boolean returnDefault = _hasDifferentAncestors(model,
                                                   focusPath,
                                                   currPath);

    if (returnDefault)
    {
      model.setRowKey(currPath);
      return defaultReturnValue;
    }

    model.setRowKey(focusPath);
    int focusIndex = model.getRowIndex();
    model.setRowKey(currPath);
    int currIndex = model.getRowIndex();

    if (currIndex < focusIndex)
      return true;

    return false;
  }


  /**
  *           <p>
  *           Determines readOnly for a "plus one" process.
  *           ReadOnly will be true for any step past the next available step
  *         </p>
   * @param model the model instance to use.
   * When the model is passed in a call to model.getPath should return the
   * path for the "current" node.
   * @param defaultReturnValue if the current and focus nodes aren't
   * siblings in the tree, this value will be returned.
   * @return whether or not the current node should be immediate.
   */
  public static boolean isReadOnly(
    MenuModel model,
    boolean   defaultReturnValue
    )
  {
    Object focusPath = model.getFocusRowKey();
    Object currPath = model.getRowKey();
    boolean returnDefault = _hasDifferentAncestors(model,
                                                   focusPath,
                                                   currPath);

    if (returnDefault)
    {
      model.setRowKey(currPath);
      return defaultReturnValue;
    }


    if (focusPath.equals(currPath))
    {
      model.setRowKey(currPath);
      return true;
    }

    model.setRowKey(focusPath);
    int focusIndex = model.getRowIndex();
    model.setRowKey(currPath);
    int currIndex = model.getRowIndex();

    if (currIndex > (focusIndex + 1))
      return true;

    return false;
  }

  private static boolean _hasDifferentAncestors(
    MenuModel model,
    Object    key1,
    Object    key2
  )
  {
      // if either key is null return true
    if ( key1 == null || key2 == null)
      return true;

    int depth = model.getDepth(key2);
    if (  depth != model.getDepth(key1))
    {
      return true;
    }
    if (depth > 0)
    {

      model.setRowKey(key1);
      Object key1ParentPath = model.getContainerRowKey();

      model.setRowKey(key2);
      Object key2ParentPath = model.getContainerRowKey();

      if (!key2ParentPath.equals(key1ParentPath))
      {
        return true;
      }
    }

    return false;
  }



//**********************************
// Max process methods
//**********************************
 /**
  *  <p>
  *   Determines immediate for a "max visited" process.
  *   When the current step and the max step are the same
  *   immediate will be true for any previous step, and false otherwise.
  *   If the current step is before the max step,
  *   immediate will be false.
  *  </p>
   * @param model the model instance to use.
   * When the model is passed in a call to model.getRowKey should return the
   * rowKey for the "current" node.
   * @param maxVisitedRowKey the rowKey to use to determine the max node
   * @param defaultReturnValue if the current, max, and focus nodes aren't
   * siblings in the tree, this value will be returned.
   * @return whether or not the current node should be immediate.
   */
  public static boolean isImmediate(
    MenuModel model,
    Object    maxVisitedRowKey,
    boolean   defaultReturnValue
    )
  {
    Object focusPath = model.getFocusRowKey();
    Object currPath = model.getRowKey();

    boolean returnDefault = _hasDifferentAncestors(model,
                                                   focusPath,
                                                   currPath,
                                                   maxVisitedRowKey);

    if (returnDefault)
    {
      model.setRowKey(currPath);
      return defaultReturnValue;
    }


    model.setRowKey(focusPath);
    int focusIndex = model.getRowIndex();
    model.setRowKey(maxVisitedRowKey);
    int maxIndex = model.getRowIndex();
    model.setRowKey(currPath);
    int currIndex = model.getRowIndex();

    if (maxIndex == focusIndex &&
        currIndex < focusIndex)
      return true;

    return false;
  }

 /**
  *  <p>
  *   Determines readOnly for a "max visited" process.
  *   When the current step and the max step are the same,
  *   readOnly will be true for any step past the next available step.
  *   If the current step is before the max step,
  *   then readOnly will be true for any step past the max step.
  *  </p>
   * @param model the model instance to use.
   * When the model is passed in a call to model.getRowKey should return the
   * rowKey for the "current" node.
   * @param maxVisitedRowKey the rowKey to use to determine the max node
   * @param defaultReturnValue if the current, max, and focus nodes aren't
   * siblings in the tree, this value will be returned.
   * @return whether or not the current node should be immediate.
   */
  public static boolean isReadOnly(
    MenuModel model,
    Object    maxVisitedRowKey,
    boolean   defaultReturnValue
    )
  {
    Object focusPath = model.getFocusRowKey();
    Object currPath = model.getRowKey();

    boolean returnDefault = _hasDifferentAncestors(model,
                                                   focusPath,
                                                   currPath,
                                                   maxVisitedRowKey);

    if (returnDefault)
    {
      model.setRowKey(currPath);
      return defaultReturnValue;
    }

    if (focusPath.equals(currPath))
    {
      model.setRowKey(currPath);
      return true;
    }

    model.setRowKey(focusPath);
    int focusIndex = model.getRowIndex();

    model.setRowKey(maxVisitedRowKey);
    int maxIndex = model.getRowIndex();

    if (maxIndex == focusIndex)
      maxIndex = maxIndex + 1;

    model.setRowKey(currPath);
    int currIndex = model.getRowIndex();


    if (currIndex > maxIndex)
      return true;

    return false;
  }
  
  
  // Plus One case

  /**
   * For the Plus One case, a stop is considered visited if, 
   * - it's before the current or,
   * - is the current stop itself
   * 
   * @param model the menuModel instance to use. When the model is passed in, 
   *  a call to model.getRowKey should return the rowKey for the "current" node.
   * @param defaultReturnValue if the current and focus nodes aren't
   *  siblings in the tree, this value will be returned.
   * @return whether or not the current node has been visited.
   */
  public static boolean isVisited(
    MenuModel model,
    boolean   defaultReturnValue
)
  {
    Object focusPath = model.getFocusRowKey();
    Object currPath = model.getRowKey();
  
    boolean returnDefault = _hasDifferentAncestors (model, 
                                                    focusPath,
                                                    currPath);
                                                    
    if (returnDefault)
    {
      model.setRowKey(currPath);
      return defaultReturnValue;
    }
                           
    // current node is active
    if (focusPath.equals(currPath))
    {
      model.setRowKey(currPath);
      return true;
    }
    
    // nodes before the current node are visited
    model.setRowKey(focusPath);
    int focusIndex = model.getRowIndex();
    model.setRowKey(currPath);
    int currIndex = model.getRowIndex();

    if (currIndex <= focusIndex)
      return true;

    return false;
  }

  /**
   * For the Max Visited case, a stop is considered visited if
   *   - a stop is before the active stop
   *   - or is the active stop
   *   
   * @param model the menuModel instance to use. When the model is passed in, a 
   *  call to model.getRowKey should return the rowKey for the "current" node.
   * @param maxVisitedRowKey the rowKey to use to determine the max visited node
   * @param defaultReturnValue if the current, maxVisited and focus nodes aren't
   *  siblings in the tree, this value will be returned.
   * @return whether or not the current node has been visited.
   */
  public static boolean isVisited (
    MenuModel model,
    Object maxVisitedRowKey,
    boolean defaultReturnValue)
  {
    Object focusPath = model.getFocusRowKey();
    Object currPath = model.getRowKey();

    boolean returnDefault = _hasDifferentAncestors(model,
                                                   focusPath,
                                                   currPath,
                                                   maxVisitedRowKey);

    if (returnDefault)
    {
      model.setRowKey(currPath);
      return defaultReturnValue;
    }

    // on active node
    if (focusPath.equals(currPath))
    {
      model.setRowKey(currPath);
      return true;
    }

    model.setRowKey(maxVisitedRowKey);
    int maxIndex = model.getRowIndex();

    model.setRowKey(currPath);
    int currIndex = model.getRowIndex();

    if (currIndex <= maxIndex)
      return true;

    return false;
  }

  /**
   * Get the rowKey of the max visited node in the respective process.
   * If null set maxVisitedRowKey to be the focus rowKey.
   * If set but the focus rowKey is after maxVisitedRowKey,
   * set maxVisitedRowKey to the focus rowKey.
   *
   * @param model  the model instance to use.
   * When the model is passed in a call to model.getRowKey should return the
   * key for the "current" node.
   * @param maxPathKey this key is used to store the maxVisitedRowKey in both the
   * session and request map.
   * @return the rowKey to the "max visited" node.
   */
  @SuppressWarnings("unchecked")
  public static Object getMaxVisitedRowKey(
    MenuModel model,
    String    maxPathKey
  )
  {
    assert(maxPathKey != null);
    
    ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
    Map<String, Object> requestMap = externalContext.getRequestMap();

    Object maxRowKey = requestMap.get(maxPathKey);
    boolean mustCompareWithFocusRow = true;

    //TODO - if I change this to use pageFlowScope it doesn't work.
    // figure out why.
    Map<String,Object> sessionMap  = externalContext.getSessionMap();
    Map<Object,Object> maxPathMap = (Map<Object,Object>)sessionMap.get(maxPathKey);
    if (maxPathMap == null)
      maxPathMap = new HashMap<Object,Object>();

    Object focusRowKey = model.getFocusRowKey();
    Object parentRowKey = model.getContainerRowKey(focusRowKey);

    if (parentRowKey == null)
      parentRowKey = _MAX_PATH_TOP;

    if (maxRowKey == null)
    {
      maxRowKey = maxPathMap.get(parentRowKey);
      if (maxRowKey == null)
      {
        maxRowKey = focusRowKey;
        maxPathMap.put(parentRowKey, maxRowKey);
        sessionMap.put(maxPathKey, maxPathMap);
        
        // for fast access store maxRowKey in the requestMap
        requestMap.put(maxPathKey, maxRowKey);
        mustCompareWithFocusRow = false;
      }
    }
    
    if (mustCompareWithFocusRow)
    {
      // figure out if focusRowKey > maxPath, if so set a new maxPath,
      // the result is set on the request so we do this once per request.
      Object currPath = model.getRowKey();
      boolean hasDifferentAncestors = _hasDifferentAncestors(model,
                                                   focusRowKey,
                                                   maxRowKey);

      //TODO - should I default this to focusPath?
      if (hasDifferentAncestors)
      {
        model.setRowKey(currPath);
        return null;
      }

      model.setRowKey(focusRowKey);
      int focusRowIndex = model.getRowIndex();
      model.setRowKey(maxRowKey);
      int maxRowIndex = model.getRowIndex();

      if (focusRowIndex > maxRowIndex)
      {
        maxPathMap.put(parentRowKey, focusRowKey);
        sessionMap.put(maxPathKey, maxPathMap);
        requestMap.put(maxPathKey, focusRowKey);
      }
      else
      {
        requestMap.put(maxPathKey, maxRowKey);
      }

      model.setRowKey(currPath);
    }

    return maxRowKey;
  }

  @SuppressWarnings("unchecked")
  public static void clearMaxPath(
    String maxPathKey
  )
  {
    if (maxPathKey != null)
    {
      Map<String, Object> sessionMap = FacesContext.getCurrentInstance().getExternalContext().getSessionMap();
      sessionMap.put(maxPathKey, null);
    }
  }


  private static boolean _hasDifferentAncestors(
    MenuModel model,
    Object    key1,
    Object    key2,
    Object    key3
  )
  {
    boolean diff1 = _hasDifferentAncestors(model, key1, key2);

    if (diff1)
      return true;

    boolean diff2 = _hasDifferentAncestors(model, key1, key3);

    if ( diff2)
      return true;

    return false;

  }



  private static final Object _MAX_PATH_TOP = new Object();

}

