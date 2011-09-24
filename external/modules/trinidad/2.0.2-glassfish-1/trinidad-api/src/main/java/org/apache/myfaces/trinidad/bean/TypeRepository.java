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

import java.util.HashMap;
import java.util.Map;

/**
 * =-=AEW Can this truly be "static public", or do we need a typed
 * API registered by class-loader?
 */
public class TypeRepository
{
  
  // Package-private: called from FacesBean.Type.lockAndRegister()
  static void registerType(
    /*String renderKitId,*/
    String componentFamily,
    String rendererType,
    FacesBean.Type type)
  {
    String renderKitId = "org.apache.myfaces.trinidad.core";

    // =-=AEW GLOBAL SYNCHRONIZATION IS NOT GOOD!  We need
    // to investigate the performance of this approach.  In
    // theory, 99.9% of both registration and retrieval will happen
    // in a single thread at startup, so sync waits should be a
    // non-issue at least given the intended use of this class.
    synchronized (_repos)
    {
      Map<String, Map<String, FacesBean.Type>> rkMap = _repos.get(renderKitId);
      //Map rkMap = (Map) _repos.get(renderKitId);
      if (rkMap == null)
      {
        rkMap = new HashMap<String, Map<String, FacesBean.Type>>();
        //rkMap = new HashMap();
        _repos.put(renderKitId, rkMap);
      }

      //Map familyMap = (Map) rkMap.get(componentFamily);
      Map<String, FacesBean.Type> familyMap = rkMap.get(componentFamily);
      if (familyMap == null)
      {
        //familyMap = new HashMap();
        familyMap = new HashMap<String, FacesBean.Type>();
        rkMap.put(componentFamily, familyMap);
      }
      
      familyMap.put(rendererType, type);
    }
  }

  static public FacesBean.Type getType(
    /* String renderKitId,*/
    String componentFamily,
    String rendererType)
  {
    String renderKitId = "org.apache.myfaces.trinidad.core";

    synchronized (_repos)
    {
      Map<String, Map<String, FacesBean.Type>> rkMap = _repos.get(renderKitId);
      //Map rkMap = (Map) _repos.get(renderKitId);
      if (rkMap == null)
        return null;

      Map<String, FacesBean.Type> familyMap = rkMap.get(componentFamily);
      //Map familyMap = (Map) rkMap.get(componentFamily);
      if (familyMap == null)
        return null;
      
      return familyMap.get(rendererType);
    }
  }

  // "qdox" doesn't support JDK 1.5 syntax like this.  Hence, all
  // the commented-out code...
  // -= Simon Lessard =-
  // Attempted to switch to JDK 5 on 2006-08-11. We'll see if qdox works fine 
  // with it now.
  static private Map<String, Map<String, Map<String, FacesBean.Type>>>
      _repos = new HashMap<String, Map<String, Map<String, FacesBean.Type>>>();
  //static private Map _repos = new HashMap();
}
