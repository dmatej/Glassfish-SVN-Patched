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
package org.apache.myfaces.trinidad.skin;

import java.util.Collections;

import javax.el.ValueExpression;

import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.context.LocaleContext;

/**
 * <p>
 * SkinAddition objects are used by custom component developers who have created custom
 * components, and they need a way to 'push' in their own stylesheet and
 * resource bundle for these components into some skin of their choosing,
 * most likely the simple skin.</p>
 * <p>
 * A Skin object contains zero or more SkinAdditions. The SkinAdditions' stylesheets
 * are merged into the Skin's own stylesheet. The SkinAdditions' resource
 * bundle is looked at along with the Skin's own resource bundle when Skin's
 * getTranslatedValue is called.
 * </p>
 * <p>
 * If you want to 'push' your styles into a specific skin, then you would create a skin-addition in the trinidad-skins.xml file.
 * You specify a &lt;skin-addition&gt;. The children are: &lt;skin-id&gt;, 
 * &lt;style-sheet-name&gt;, &lt;bundle-name&gt;, and &lt;translation-source&gt;.
 * The &lt;skin-id&gt; is used to specify which skin you want to 'push' your stylesheet/resource bundle into. 
 * Most likely this is the simple.desktop skin.
 * The other elements are used to create a SkinAddition object.
 * </p>
 * 
 *
 */
public class SkinAddition
{


  /**
   * Constructor takes a styleSheet name and a resourceBundle name.
   */
  public SkinAddition (
    String styleSheetName,
    String resourceBundleName
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = resourceBundleName;
    _translationSourceVE = null;
    _translationSourceVB = null;
  }

  /**
   * Constructor takes a styleSheet name and a translationSource ValueExpression.
   */
  public SkinAddition (
    String       styleSheetName,
    ValueExpression translationSourceValueExpression
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = null;
    _translationSourceVE = translationSourceValueExpression;
    _translationSourceVB = null;
  }
  /**
   * Constructor takes a styleSheet name. resource bundle name and 
   * translation source value expression will be null.
   */
  public SkinAddition (
    String styleSheetName
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = null;
    _translationSourceVE = null;
    _translationSourceVB = null;
  }
  
  /**
   * Constructor takes a styleSheet name and a translationSource ValueBinding.
   * @deprecated
   */
  @Deprecated
  public SkinAddition (
    String       styleSheetName,
    ValueBinding translationSourceValueBinding
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = null;
    _translationSourceVE = null;
    _translationSourceVB = translationSourceValueBinding;
  }
  
  /**
   * Gets the SkinAddition's style sheet name.
   */
  public String getStyleSheetName()
  {
    return _styleSheetName;
  } 
  
  /**
   * Gets the SkinAddition's resource bundle. 
   * Note: A skin cannot have both a resourceBundleName and a translation source
   * value expression. If they do, then the resourceBundleName takes precedence.
   */
  public String getResourceBundleName()
  {
    return _resourceBundleName;
  } 
  
  /**
   * Gets the SkinAddition's translation source ValueExpresion. The 
   * ValueExpression can point to a Map or a ResourceBundle.
   * Note: A skin cannot have both a resourceBundleName and a translation source
   * value expression. If they do, then the resourceBundleName takes precedence.
   */
  public ValueExpression getTranslationSourceValueExpression()
  {
    return _translationSourceVE;
  } 
  
  /**
   * Gets the SkinAddition's translation source ValueBinding. The 
   * ValueBinding can point to a Map or a ResourceBundle.
   * Note: A skin cannot have both a resourceBundleName and a translation source
   * value binding. If they do, then the resourceBundleName takes precedence.
   * @deprecated
   */
   @Deprecated
  public ValueBinding getTranslationSourceValueBinding()
  {
    return _translationSourceVB;
  } 
 
  private final String       _styleSheetName;
  private final String       _resourceBundleName;
  private final ValueExpression _translationSourceVE;
  private final ValueBinding _translationSourceVB;
  
}
