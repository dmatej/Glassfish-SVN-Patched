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
package org.apache.myfaces.trinidadinternal.style.xml;

/**
 * Constants for org.apache.myfaces.trinidadinternal.style.xml.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/XMLConstants.java#0 $) $Date: 10-nov-2005.18:58:00 $
 */
public interface XMLConstants
{
  // Element names
  public static final String COLOR_NAME          = "color";
  public static final String IMPORT_NAME         = "import";
  public static final String INCLUDE_STYLE_NAME  = "includeStyle";
  public static final String INCLUDE_PROPERTY_NAME  = "includeProperty";
  public static final String PROPERTY_NAME       = "property";
  public static final String STYLE_NAME          = "style";
  public static final String STYLE_SHEET_NAME    = "styleSheet";
  public static final String STYLE_SHEET_DOCUMENT_NAME = "styleSheetDocument";
  public static final String COMPOUND_PROPERTY_NAME = "compoundProperty";
  public static final String VALUE_NAME          = "value";
  public static final String INCLUDE_VALUE_NAME  = "includeValue";

  // Attribute names
  public static final String NAME_ATTR      = "name";
  public static final String NAMESPACE_ATTR = "namespace";
  public static final String SELECTOR_ATTR  = "selector";
  public static final String LOCALES_ATTR   = "locales";
  public static final String DIRECTION_ATTR = "direction";
  public static final String MODE_ATTR      = "mode";
  public static final String BROWSERS_ATTR  = "browsers";
  public static final String VERSIONS_ATTR  = "versions";
  public static final String PLATFORMS_ATTR = "platforms";
  public static final String ACC_PROFILE_ATTR = "accessibilityProfile";
  public static final String HREF_ATTR      = "href";
  public static final String PROPERTY_NAME_ATTR       = "propertyName";
  public static final String LOCAL_PROPERTY_NAME_ATTR = "localPropertyName";
  public static final String DOCUMENT_VERSION_ATTR = "documentVersion";
  public static final String RESET_PROPERTIES_ATTR = "resetProperties";

  // Some value constants
  public static final String DIRECTION_RIGHTTOLEFT  = "rtl";
  public static final String DIRECTION_LEFTTORIGHT  = "ltr";

  // AccessibilityProfile contstants
  public static final String ACC_HIGH_CONTRAST  = "high-contrast";
  public static final String ACC_LARGE_FONTS    = "large-fonts";
}

