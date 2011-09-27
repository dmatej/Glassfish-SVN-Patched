
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
package org.apache.myfaces.trinidad.util;

import java.lang.reflect.Method;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.faces.context.ExternalContext;

import javax.portlet.faces.annotation.ExcludeFromManagedRequestScope;

import javax.servlet.ServletRequest;

/**
 * TODO: get rid of this object
 */
 @ExcludeFromManagedRequestScope
 public class RequestStateMap extends HashMap<String, Object>
 {

   static public RequestStateMap getInstance(ServletRequest req)
   {
     RequestStateMap map = (RequestStateMap)req.getAttribute(_STATE_MAP);
     
     if(map == null)
     {
       map = new RequestStateMap();
       req.setAttribute(_STATE_MAP, map);
     }
     
     return map;
   }
   
   static public RequestStateMap getInstance(ExternalContext ec)
   {
     Map<String, Object> reqMap = ec.getRequestMap();
     RequestStateMap map = (RequestStateMap)reqMap.get(_STATE_MAP);
     
     //For now, always check this on a render so it can be removed from the session.
     //This can be optimized to only save the state when request attributes are NOT preserved
     if(!ExternalContextUtils.isRequestFromClient(ec))
     {
       String uuid = ec.getRequestParameterMap().get(_STATE_MAP);
       if(uuid!= null)
       {
          RequestStateMap myMap= (RequestStateMap)ec.getSessionMap().remove(_STATE_MAP+"."+uuid);
          if(map == null)
          {
            map = myMap;
            reqMap.put(_STATE_MAP, map);
          }
          else
          {
            //TODO: put optimization code here
          }
       }
     }
     
     if(map == null)
     {
       map = new RequestStateMap();
       reqMap.put(_STATE_MAP, map);
     }
     
     return map;
   }
   
   private RequestStateMap(){};
   
   public void saveState(ExternalContext ec)
   {
     RequestType type = ExternalContextUtils.getRequestType(ec);
     if(type.isPortlet() && !type.isResponseWritable())
     {
       try
       {
         //TODO: use reflection here but it can be replaced..
         Object actionResp = ec.getResponse();
         Method m = actionResp.getClass().getMethod("setRenderParameter", String.class, String.class);
         String uuid = UUID.randomUUID().toString();

         ec.getSessionMap().put(_STATE_MAP+"."+uuid, this);
         m.invoke(actionResp, _STATE_MAP, uuid);
       }
       catch(Throwable t)
       {
         //TODO: Log exception
         t.printStackTrace();
       }
     }
   }
   
   private static final String _STATE_MAP = RequestStateMap.class.getName();
   private static final long serialVersionUID = 1L;
 }
