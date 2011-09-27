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
package org.apache.myfaces.trinidadinternal.renderkit.core.skin;


import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;
import org.apache.myfaces.trinidadinternal.skin.SkinExtension;

/**
  * Implementation the Minimal Portlet Skin for desktop
  * browsers.
  */
 public class MinimalPortletSkinExtension extends SkinExtension

 {
   /**
    * Creates an MinimalPortletSkinExtension instance which extends
    * the specified base Skin. (should be SimplePortletSkin)
    */
   public MinimalPortletSkinExtension(Skin baseSkin)
   {
     super(baseSkin,
           "minimal.portlet",
           TrinidadRenderingConstants.MINIMAL_SKIN_FAMILY,
           TrinidadRenderingConstants.OUTPUT_MODE_PORTLET,
           "META-INF/adf/styles/simple-portlet.css");
   }

 }
