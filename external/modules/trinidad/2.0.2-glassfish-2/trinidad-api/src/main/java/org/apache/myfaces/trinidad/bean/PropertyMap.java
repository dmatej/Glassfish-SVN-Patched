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
package org.apache.myfaces.trinidad.bean;

import java.util.Map;
import javax.faces.context.FacesContext;


// TODO Make "stateHolder" a generic property of all PropertyMaps?
//     If so, tweak StateUtils accordingly
public interface PropertyMap extends Map<PropertyKey,Object>
{
  public void markInitialState();
  
  /** 
   * @return true if delta state changes are being tracked, otherwise false
   */
  public boolean initialStateMarked();

  /** 
   * Reset to a non-delta tracking state.
   */
  public void clearInitialState();     
  public Object saveState(FacesContext context);
  public void restoreState(FacesContext context, FacesBean.Type type, Object state);
}
