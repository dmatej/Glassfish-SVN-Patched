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
package org.apache.myfaces.trinidadinternal.style;

/**
 * Constants for org.apache.myfaces.trinidadinternal.style.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/StyleConstants.java#0 $) $Date: 10-nov-2005.18:57:57 $
 */
public interface StyleConstants
{
  /**
   * Namespace used by UIX Styles.
   * This namespace is used for storing style-related properties
   * on the RenderingContext.
   */
  public static final String OCELOT_NAMESPACE =
    "http://myfaces.apache.org/uix/style";

  /**
   * Mime type for CSS
   */
  public static final String CSS_MIME_TYPE = "text/css";

  /**
   * Property key for specifying the UserStyleSheet to use when
   * accessing Styles from a StyleProvider.
   */
  public static final Object USER_STYLE_SHEET_PROPERTY = "userStyleSheet";
}
