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

import java.beans.IntrospectionException;


 /**
  *
  *
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
  *           step. The {@link #isReadOnly} method can be used to
  *           bind the node's readOnly attribute.
  *           </p>
  *           <p>
  *           A node in a process should be immediate if the values
  *           in the current step don't need to be validated.
  *           The {@link #isImmediate} method can be used to
  *           bind the node's immediate attribute.
  *           </p>
  *           <p>
  *
  *
  *
  * @see ProcessUtils
  *
  */
public class ProcessMenuModel extends ViewIdPropertyMenuModel
{

  /**
   * No-arg constructor for use with managed-beans.
   * Must call the {@link #setViewIdProperty} and
   * {@link #setWrappedData} methods after constructing this instance.
   */
  public ProcessMenuModel()
  {
    super();
  }

  /**
   *
   * The "Plus One" behavior will be used with this constructor.
   *
   * @param instance a treeModel. This object will be passed to
   * {@link ModelUtils#toTreeModel}
   * @param viewIdProperty the property to use to retrieve a viewId
   * from a node in the tree
   * @throws IntrospectionException
   */
  public ProcessMenuModel(
    Object instance,
    String viewIdProperty
  )throws IntrospectionException
  {
    super(instance, viewIdProperty);
  }


  /**
   *
   * @param instance a treeModel. This object will be passed to
   * {@link ModelUtils#toTreeModel}
   * @param viewIdProperty the property to use to retrieve a viewId
   * from a node in the tree
   * @param maxPathKey if the "Max Visited" behavior is desired, this
   * is the key that will be used to get and set the maxPath value
   * on the session and request. For the "Plus One" behavior pass in null.
   * @throws IntrospectionException
   */
  public ProcessMenuModel(
    Object instance,
    String viewIdProperty,
    String maxPathKey
  )throws IntrospectionException
  {
    super(instance, viewIdProperty);
    setMaxPathKey(maxPathKey);
  }

 /**
  *           <p>
  *           A node in a process should be immediate if the values
  *           in the current step don't need to be validated.
  *           This method can be used to
  *           bind the node's immediate attribute. If a user will have to
  *           return to the current page then immediate can be set to true.
  *           For example in a "Plus One" process, if the user is on step 5
  *           and goes back to step 2, the user will have to come back
  *           to step 5 again, so the fields on page 5 don't need to
  *           be validated when going back to steps 1,2,3,4, but should be
  *           validated when going to step 6.
  *           </p>
  *           <p>
  *
  *
  *           <ul>
  *             <li>"Plus One"
  *               <ul>
  *                 <li>immediate - immediate will be true for any previous step,
  *                                 and false otherwise.</li>
  *               </ul>
  *             </li>
  *             <li>"Max Visited" - when the current step and the max step
  *                  are the same, "max visited" behaves like "plus one". If
  *                  the current step is before the max step, then:
  *               <ul>
  *                 <li>immediate - immediate is false. </li>
  *               </ul>
  *             </li>
  *           </ul>
  *         </p>
  */
  public boolean isImmediate()
  {
    String maxPathKey = getMaxPathKey();
    if ( maxPathKey == null)
      return ProcessUtils.isImmediate(this, false);
    else
    {
      Object  maxPath = ProcessUtils.getMaxVisitedRowKey(this, maxPathKey);
      return ProcessUtils.isImmediate(this, maxPath, false);
    }
  }

 /**
  *           <p>
  *           A node in a process should be readOnly
  *           if that step of the process is not reachable from the current
  *           step. This method can be used to
  *           bind the node's readOnly attribute.
  *           </p>
  *
  *           <ul>
  *             <li>"Plus One"
  *               <ul>
  *                 <li>readOnly - readOnly will be true for any step past
  *                                the next available step</li>
  *               </ul>
  *             </li>
  *             <li>"Max Visited" - when the current step and the max step
  *                  are the same, "max visited" behaves like "plus one". If
  *                  the current step is before the max step, then:
  *               <ul>
  *                 <li>readOnly - readOnly will be true for any step
  *                                past the max step</li>
  *               </ul>
  *             </li>
  *           </ul>
  *         </p>
  */
  public boolean isReadOnly()
  {
    String maxPathKey = getMaxPathKey();
    if (maxPathKey == null)
      return ProcessUtils.isReadOnly(this, true);
    else
    {
      Object maxPath = ProcessUtils.getMaxVisitedRowKey(this, maxPathKey);
      return ProcessUtils.isReadOnly(this, maxPath, true);
    }
  }
  
  /**
   * For the Max Visited case, a stop is considered visited if
   *   - a stop is before the max visited stop
   *   - or is the max visited stop
   *   
   * For the Plus One case, a stop is considered visited if, 
   *   - it's before the current or,
   *   - is the current stop itself
   */
  public boolean isVisited()
  {
    // Max Visited
    String maxPathKey = getMaxPathKey();
    if ( maxPathKey == null)
    {
      return ProcessUtils.isVisited(this, false);
    }
    else
    {
      Object maxPath = ProcessUtils.getMaxVisitedRowKey(this, maxPathKey);
      return ProcessUtils.isVisited(this, maxPath, false);
    }
  }  

  /**
   * to clear the max visited path out of the session
   */
  public void clearMaxPath()
  {
    String maxPathKey = getMaxPathKey();
    if ( maxPathKey != null)
      ProcessUtils.clearMaxPath(maxPathKey);
  }

  public void setMaxPathKey(String maxPathKey)
  {
    _maxPathKey = maxPathKey;
  }

  public String getMaxPathKey()
  {
    return _maxPathKey;
  }

  private String _maxPathKey;
}
