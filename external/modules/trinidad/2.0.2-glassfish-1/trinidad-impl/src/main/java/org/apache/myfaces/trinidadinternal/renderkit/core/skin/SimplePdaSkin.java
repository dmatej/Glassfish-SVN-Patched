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

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;


/**
 * Skin implementation for simple pocket pc
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/SimplePdaSkin.java#0 $) $Date: 10-nov-2005.19:02:56 $
 */
public class SimplePdaSkin extends PdaHtmlSkin
{

  /**
   * Returns the name of the XSS style sheet for this Skin.
   */
  @Override
  public String getStyleSheetName()
  {
    return "META-INF/adf/styles/simple-pda.css";
  }

  /**
   * Returns the id for the PDA implementation of the Simple
   * Skin: "simple.pda".
   */
  @Override
  public String getId()
  {
    return "simple.pda";
  }

  /**
   * Returns the family for the PDA implementation of the Simple
   * Skin: "simple.pda".
   */
  @Override
  public String getFamily()
  {
    return "simple";
  }

  /**
   * Returns the renderKitId for the PDA implementation of the Simple
   * Skin: "org.apache.myfaces.trinidad.pda".
   */  
  @Override
  public String getRenderKitId()
  {
    return XhtmlConstants.APACHE_TRINIDAD_PDA;
  }
}
