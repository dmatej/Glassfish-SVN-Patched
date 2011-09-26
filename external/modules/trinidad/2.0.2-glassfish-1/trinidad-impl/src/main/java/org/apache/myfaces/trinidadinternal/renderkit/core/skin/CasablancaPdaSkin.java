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

import org.apache.myfaces.trinidadinternal.skin.SkinExtension;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;
import org.apache.myfaces.trinidad.skin.Skin;

/**
 * Class for the Casablanca PDA skin
 */
public class CasablancaPdaSkin extends SkinExtension {

    /**
   * Constructs a CasablancaPdaSkin instance
   */
  public CasablancaPdaSkin(Skin baseSkin)
  {
    // Create a SkinExtension for Casablanca
    super(baseSkin,
            _CASABLANCA_PDA_ID,
          TrinidadRenderingConstants.CASABLANCA_SKIN_FAMILY,
          TrinidadRenderingConstants.APACHE_TRINIDAD_PDA);

    // Register our style sheet
    setStyleSheetName(_CASABLANCA_STYLE_SHEET_NAME);
  }

  // Casablanca skin id
  private static final String _CASABLANCA_PDA_ID = "casablanca.pda";


  // Casablanca skin style sheet name
  private static final String _CASABLANCA_STYLE_SHEET_NAME =
    "META-INF/adf/styles/casablancaSkin.css";
}
