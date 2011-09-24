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

import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;

/**
 * Defines the components (icons, styles, etc)
 * which are used to implement a particular skin.
 *
 * @see SkinFactory
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/Skin.java#0 $) $Date: 10-nov-2005.18:58:54 $
 */
abstract public class Skin
{
  /**
   * Returns an string identifier which uniquely identies
   * this Skin implementation.  Skin implementations
   * can be retrieved by id via SkinFactory.getSkin().
   * @see org.apache.myfaces.trinidad.skin.SkinFactory#getSkin
   */
  abstract public String getId();

  /**
   * Returns the name of the skin "family" for this skin.
   * The family name is used when specifying a preferred skin
   * in trinidad-config.xml.
   * This provides a way to refer to a group of
   * related skin implementations while allowing the
   * particular skin instance to be selected based on the
   * current render-kit-id.
   */
  abstract public String getFamily();
  

  /**
   * Returns the (@link SkinVersion} instance of the "version" for this skin.
   * When a Skin instance is created, a SkinVersion instance can be a part of it.
   * In trinidad-skins.xml this is the version element. In the trinidad-config.xml, 
   * the application developer can set the skin-version to a skin version, or to 'default'.
   * This returns SkinVersion.EMPTY_SKIN_VERSION if no version is set.
   */
  public SkinVersion getVersion()
  {
    return SkinVersion.EMPTY_SKIN_VERSION;
  }

  /**
   * Returns the renderKitId for the Skin.
   */
  abstract public String getRenderKitId();
  
  /**
   * Returns the id of the Skin's stylesheet document.
   */
  abstract public String getStyleSheetDocumentId(RenderingContext arc);

  /**
   * Returns the style class map, or null if there is no map.
   * It should be a map that contains the full style class name as 
   * the key, and the value could be a shortened style class name,
   * or a portlet style class name, etc.
   */
  abstract public Map<String, String> getStyleClassMap(RenderingContext arc);

  /**
   * Returns the name of the style sheet for this Skin.
   */
  abstract public String getStyleSheetName();

  /**
   * Returns a translated String in the LocaleContext's translation Locale.
   */
  abstract public String getTranslatedString(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException;

  /**
   * Returns a translated value in the LocaleContext's translation Locale.
   * This value may or may not be a String, and developers should avoid
   * calling toString() unless absolutely necessary.
   * @param lContext The LocaleContext which provides the translation Locale.
   *                 Cannot be null.
   * @param key The key of the translation to retrieve. Cannot be null.
   * @throws NullPointerException if lContext or key is null.
   */
  abstract public Object getTranslatedValue(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException;

  /**
   * Our renderers call this to get the icon. This returns a renderable
   * icon. (ReferenceIcons are resolved -- the real icon they point to is
   * returned)
   */
  abstract public Icon getIcon(String  iconName);

  /**
   * Returns an Icon object; can be a ReferenceIcon.
   * @param iconName  The name of the icon to retrieve. Cannot be null
   * @throws NullPointerException if iconName is null.
   */
  abstract public Icon getIcon(
    String  iconName,
    boolean resolveIcon);

  /**
   * Retrieves a property that was set via a call to setProperty().
   * Some Renderer implementations may store properties on the
   * Skin instance to avoid having to re-compute Skin-specific
   * values on each render.
   */
  abstract public Object getProperty(Object key);

  /**
   * Sets a value for the specified property key.
   * Some Renderer implementations may store properties on the
   * Skin instance to avoid having to re-compute Skin-specific
   * values on each render.
   */
  abstract public void setProperty(
    Object key,
    Object value);

  /**
   * Registers an Icon for the specified icon name.
   * @param iconName  The name of the icon. Cannot be null.
   * @param icon      The Icon to register.
   * @throws NullPointerException if iconName is null.
   */
  abstract public void registerIcon(
    String  iconName,
    Icon    icon);

  /**
   * Registers a style sheet which defines extension-specific
   * styles.  The styles specified by this style sheet will be
   * merged with the Skin's own styles.
   * @param styleSheetName The name of the style sheet which
   *          defines the extension's styles.
   * @throws NullPointerException if styleSheetName is null.
   * @deprecated Use addSkinAddition(SkinAddition) instead.
   * @see #addSkinAddition(SkinAddition)
   */
  @Deprecated
  abstract public void registerStyleSheet(
    String styleSheetName
    );
    
  /**
   * Adds a SkinAddition on this Skin. You can call this method as many times 
   * as you like for the Skin, and it will add the SkinAddition to the list of 
   * SkinAdditions.
   * However, it does not make sense to call this method more than once
   * with the same SkinAddition object.
   * This is meant for the skin-addition use-cases, where a custom component 
   * developer has a style sheet and/or resource bundle for their custom   
   * components, and they want the style sheet and/or resource bundle 
   * to work for this Skin and the children Skins.
   * The stylesheets specified in the SkinAdditions will be merged with the 
   * Skin's own styles.
   * The resource bundles specified in the SkinAdditions will be looked into 
   * if the translated key is not found in the Skin's own resource bundle 
   * during the call to getTranslatedString or getTranslatedValue.
   * 
   * @param skinAddition The SkinAddition object to add to the Skin.
   * @throws NullPointerException if SkinAddition is null.
   */
  abstract public void addSkinAddition (
    SkinAddition skinAddition
    );
    
  /**
   * Gets a List of SkinAdditions that have been added on this Skin.
   * @return List a List of SkinAdditions.
   */
  abstract public List<SkinAddition> getSkinAdditions ();
  
  /**
   * Check to see if this Skin has been marked dirty. 
   * The only way to mark a Skin dirty is to call setDirty(true).
   * @return true if the Skin is marked dirty. 
   */
  abstract public boolean isDirty();

  /**
   * Sets the dirty flag of the Skin. Use this if you want to regenerate the skin. 
   * During rendering, if isDirty is true, 
   * the skin's css file will be reprocessed regardless of whether the css file has been modified 
   * or if the CHECK_FILE_MODIFICATION flag was set. 
   * The Skinning Framework calls setDirty(false) after the skin has been reprocessed.
   */
  abstract public void setDirty(boolean dirty);
  
}
