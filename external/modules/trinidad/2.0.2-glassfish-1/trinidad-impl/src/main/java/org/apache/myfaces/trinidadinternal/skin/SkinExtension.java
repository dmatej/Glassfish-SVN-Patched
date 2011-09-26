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
package org.apache.myfaces.trinidadinternal.skin;

import java.io.IOException;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Stack;

import javax.el.ValueExpression;
import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.SkinVersion;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;

import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.xml.StyleSheetDocumentUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;

/**
 * A Skin which extends another Skin, possibly adding
 * customizations.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinExtension.java#0 $) $Date: 10-nov-2005.18:58:55 $
 */
public class SkinExtension extends SkinImpl
{
  /**
   * Constructs a SkinExtension of id and family and renderKitId. It also
   * sets the styleSheetname and a resourceBundle name.
   * 
   * @param baseSkin The base Skin that this custom
   *        Skin "extends". If it is a Skin designed for "org.apache.myfaces.trinidad.desktop"
   *        render-kit-id, then its base skin should be SimpleDesktopSkin.
   *        If it is a Skin designed for "org.apache.myfaces.trinidad.pda" render-kit-id,
   *        then its base skin should be SimplePdaSkin.
   *        Must be non-null.
   * @param id A string which can be used to uniquely identify the
   *           Skin .
   *           Must be non-null.
   * @param family The Skin family name that this
   *               SkinExtension belongs to. For example, you might have
   *               a Skin that makes your pages look purple for the
   *               desktop renderkit and a Skin that makes your pages
   *               look purple for the pda renderkit.
   *               You can set the skin-family to "purple" in
   *               trinidad-config.xml, and the Skin with skin-family
   *               and render-kit-id match will be chosen.
   *               Must be non-null.
   * @param renderKitId The render-kit-id that this Skin is designed for.
   * @param styleSheetName The name of the stylesheet for this Skin.
   * @param resourceBundleName The name of the resource bundle for this Skin
   *    to be used to translate strings that a renderer renders.

   * @throws NullPointerException if baseSkin, id, or family is null.
   * 
   */
  public SkinExtension(
    Skin baseSkin,
    String id,
    String family,
    String renderKitId,
    String styleSheetName,
    String resourceBundleName
    )
  {
    this(baseSkin, id, family, renderKitId, styleSheetName, 
         resourceBundleName, SkinVersion.EMPTY_SKIN_VERSION);
  }
  
  public SkinExtension(
    Skin baseSkin,
    String id,
    String family,
    String renderKitId,
    String styleSheetName,
    String resourceBundleName,
    SkinVersion version
    )
  {
    if (baseSkin == null)
      throw new NullPointerException("Null baseSkin");
    if (id == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_SKIN_ID"));
    if (family == null)
      throw new NullPointerException("Null family");
    if (renderKitId == null)
      renderKitId = _DEFAULT_RENDERKIT;
    if (version == null)
      version = SkinVersion.EMPTY_SKIN_VERSION;

    _baseSkin = (SkinImpl)baseSkin;
    _id = id;
    _family = family;
    _renderKitId = renderKitId;
    _styleSheetName = styleSheetName;
    _bundleName = resourceBundleName;
    _translationSourceVE = null;
    _version = version;    
  }
  
  /**
   * Constructs a SkinExtension of id and family and renderKitId. It also
   * sets the styleSheetname and a translation-source (instead of a resource
   * bundle name).
   * 
   * @param baseSkin The base Skin that this custom
   *        Skin "extends". If it is a Skin designed for "org.apache.myfaces.trinidad.desktop"
   *        render-kit-id, then its base skin should be SimpleDesktopSkin.
   *        If it is a Skin designed for "org.apache.myfaces.trinidad.pda" render-kit-id,
   *        then its base skin should be SimplePdaSkin.
   *        Must be non-null.
   * @param id A string which can be used to uniquely identify the
   *           Skin .
   *           Must be non-null.
   * @param family The Skin family name that this
   *               SkinExtension belongs to. For example, you might have
   *               a Skin that makes your pages look purple for the
   *               desktop renderkit and a Skin that makes your pages
   *               look purple for the pda renderkit.
   *               You can set the skin-family to "purple" in
   *               trinidad-config.xml, and the Skin with skin-family
   *               and render-kit-id match will be chosen.
   *               Must be non-null.
   * @param renderKitId The render-kit-id that this Skin is designed for.
   * @param styleSheetName The name of the stylesheet for this Skin.
   * @param translationSourceValueExpression
   *          A ValueExpression that points to a translation source of type
   *          Map or ResourceBundle. This can be used instead of a 
   *          resource bundle name.
   * @throws NullPointerException if baseSkin, id, or family is null.
   * 
   */
  public SkinExtension(
    Skin   baseSkin,
    String id,
    String family,
    String renderKitId,
    String styleSheetName,
    ValueExpression translationSourceValueExpression
    )
  {
    this(baseSkin, id, family, renderKitId, styleSheetName, 
         translationSourceValueExpression, SkinVersion.EMPTY_SKIN_VERSION);
  }
  
  public SkinExtension(
    Skin   baseSkin,
    String id,
    String family,
    String renderKitId,
    String styleSheetName,
    ValueExpression translationSourceValueExpression,
    SkinVersion version
    )
  {
    if (baseSkin == null)
      throw new NullPointerException("Null baseSkin");
    if (id == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_SKIN_ID"));
    if (family == null)
      throw new NullPointerException("Null family");
    if (renderKitId == null)
      renderKitId = _DEFAULT_RENDERKIT;
    if (version == null)
      version = SkinVersion.EMPTY_SKIN_VERSION;

    _baseSkin = (SkinImpl)baseSkin;
    _id = id;
    _family = family;
    _renderKitId = renderKitId;
    _styleSheetName = styleSheetName;
    _bundleName = null;
    _translationSourceVE = translationSourceValueExpression;
    _version = version;
  }  

  
  /**
   * Constructs a SkinExtension of id and family and renderKitId. It also
   * sets the styleSheetname, but not the resource bundle name or 
   * translation source. This is the most frequently-used constructor.
   *
   * @param baseSkin The base Skin that this custom
   *        Skin "extends". If it is a Skin designed for "org.apache.myfaces.trinidad.desktop"
   *        render-kit-id, then its base skin should be SimpleDesktopSkin.
   *        If it is a Skin designed for "org.apache.myfaces.trinidad.pda" render-kit-id,
   *        then its base skin should be SimplePdaSkin.
   *        Must be non-null.
   * @param id A string which can be used to uniquely identify the
   *           Skin .
   *           Must be non-null.
   * @param family The Skin family name that this
   *               SkinExtension belongs to. For example, you might have
   *               a Skin that makes your pages look purple for the
   *               desktop renderkit and a Skin that makes your pages
   *               look purple for the pda renderkit.
   *               You can set the skin-family to "purple" in
   *               trinidad-config.xml, and the Skin with skin-family
   *               and render-kit-id match will be chosen.
   *               Must be non-null.
   * @param renderKitId The render-kit-id that this Skin is designed for.
   * @param styleSheetName The name of the stylesheet for this Skin.
   *
   * @throws NullPointerException if baseSkin, id, or family is null.
   * 
   */  
  public SkinExtension(
    Skin baseSkin,
    String id,
    String family,
    String renderKitId,
    String styleSheetName
    )
  {
    this(baseSkin, id, family, renderKitId, styleSheetName, SkinVersion.EMPTY_SKIN_VERSION);
  }
  
  /*
   * SkinExtension without the resource bundle information, but with the version information.
   */
  public SkinExtension(
    Skin baseSkin,
    String id,
    String family,
    String renderKitId,
    String styleSheetName,
    SkinVersion version
    )
  {
    if (baseSkin == null)
      throw new NullPointerException("Null baseSkin");
    if (id == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_SKIN_ID"));
    if (family == null)
      throw new NullPointerException("Null family");
    if (renderKitId == null)
      renderKitId = _DEFAULT_RENDERKIT;
    if (version == null)
      version = SkinVersion.EMPTY_SKIN_VERSION;

    _baseSkin = (SkinImpl)baseSkin;
    _id = id;
    _family = family;
    _renderKitId = renderKitId;
    _styleSheetName = styleSheetName;
    _bundleName = null;
    _translationSourceVE = null;
    _version = version;
  }
  
  /**
   * Creates a Skin which extends the specified base
   * Skin.
   *
   * @param baseSkin The base Skin that this custom
   *        Skin "extends". If it is a Skin designed for "org.apache.myfaces.trinidad.desktop"
   *        render-kit-id, then its base skin should be SimpleDesktopSkin.
   *        If it is a Skin designed for "org.apache.myfaces.trinidad.pda" render-kit-id,
   *        then its base skin should be SimplePdaSkin.
   *        Must be non-null.
   * @param id A string which can be used to uniquely identify the
   *           Skin .
   *           Must be non-null.
   * @param family The Skin family name that this
   *               SkinExtension belongs to. For example, you might have
   *               a Skin that makes your pages look purple for the
   *               desktop renderkit and a Skin that makes your pages
   *               look purple for the pda renderkit.
   *               You can set the skin-family to "purple" in
   *               trinidad-config.xml, and the Skin with skin-family
   *               and render-kit-id match will be chosen.
   *               Must be non-null.
   * @param renderKitId The render-kit-id that this Skin is designed for.
   * @throws NullPointerException if baseSkin, id, or family is null.
   * @deprecated Use the constructor that also contains styleSheetName
   *
   */
  public SkinExtension(
    Skin baseSkin,
    String id,
    String family,
    String renderKitId
    )
  {
    this(baseSkin, id, family, renderKitId, null);
  }



  /**
   * Returns the base Skin which this custom Skin
   * "extends".
   */
  public Skin getBaseSkin()
  {
    return _baseSkin;
  }

  /**
   * Returns the id of this custom Skin.
   */
  @Override
  public String getId()
  {
    return _id;
  }

  /**
   * Returns the name of the Skin family that this
   * SkinExtension belongs to.
   */
  @Override
  public String getFamily()
  {
    return _family;
  }

  /**
   * Returns the SkinVersion object. 
   * If version was not set when creating the SkinExtension, 
   * this returns SkinVersion.EMPTY_SKIN_VERSION.
   */
  @Override
  public SkinVersion getVersion()
  {
    return _version;
  }
  
  /**
   * Returns the name of the style sheet for this Skin if
   * one has been set
   * @see #setStyleSheetName(String)
   */
  @Override
  public String getStyleSheetName()
  {
    return _styleSheetName;
  }

  /**
   * Returns the name of the render-kit-id for this Skin.
   */
  @Override
  public String getRenderKitId()
  {
    return _renderKitId;
  }


  /**
   * Returns the name of the bundle for the SkinExtension.
   * Note: A skin cannot have both a bundleName and a translation source
   * value expression. If they do, then the bundlename takes precedence.
   */
  @Override
  public String getBundleName()
  {
    return _bundleName;
  }

  /**
   * Returns the name of the bundle for this SkinExtension.
   * @deprecated Use the constructor that takes a resourceBundleName instead
   */
  public void setBundleName(String bundleName)
  {
     // TODO take out method once sufficient time has past since deprecation
    // in July, 2007
    _bundleName = bundleName;
  }

  /**
   * Returns the translation source ValueExpression for the SkinExtension.
   * Note: A skin cannot have both a bundleName and a translation source
   * value expression. If they do, then the bundlename takes precedence.
   * A translation source can be a map of keys/values or a ResourceBundle.
   */
  @Override
  public ValueExpression getTranslationSourceValueExpression()
  {
    return _translationSourceVE;
  }
  
  /**
   * Override of Skin.registerIcon().
   */
  @Override
  public void registerIcon(
    String  iconName,
    Icon    icon
    )
  {
    // If a null icon is being registered, use a NullIcon
    // as a placeholder so that we don't look for a non-null
    // Icon in the base LAF.
    if (icon == null)
      icon = _NULL_ICON;

    super.registerIcon(iconName, icon);
  }

  /**
   * Returns the styleClassMap for this extension
   */
    @Override
  public Map<String, String> getStyleClassMap(
       RenderingContext arc
     )
  {
    return _baseSkin.getStyleClassMap(arc);
  }



  /**
    * Override of Skin.getTranslatedValue() which
    * supports pulling translations from Skin and if not found from the base Skin.
  */
  @Override
  public Object getTranslatedValue(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException
  {
    // Look for the skin's translated value 
    // -first Skin's translation source, then SkinAddition translation sources.
    // A translation source is either a bundleName or 
    // a translationSourceValueExpression
    // If that's not found, then look in the base skin's translated value.
    // getCachedTranslatedValue will protect against MissingResourceExceptions

    Object translatedValue = getCachedTranslatedValue(lContext, key);

    if (translatedValue == null)
    {
      translatedValue = getBaseSkin().getTranslatedValue(lContext, key);
      // Cache the non-null translated value with the SkinExtension to avoid looking
      // at the base skin's map.
      if (translatedValue != null)
        putTranslatedValueInLocaleCache(lContext, key, translatedValue);

    }

    return translatedValue;
  }
   
  /**
   * Try to pull a locally set property, if null
   * pull a property from the base skin.
   * This means you cannot set a local property to null and expect it to
   * "null out" a property on the base skin.
   */
  @Override
  public Object getProperty(Object key)
  {
    Object value = super.getProperty(key);

    if ( value == null)
    {
      Skin baseSkin = getBaseSkin();
      value =  baseSkin.getProperty(key);
    }

    return value;

  }

  /**
   * Override of Skin.getIcon() to look in the base skin for the icon
   * if it isn't registered yet with this skin.
   */
  @Override
  public Icon getIcon(
    String  iconName,
    boolean resolve
    )
  {
    // First check to see if we already have an Icon registered.
    // it might be a ReferenceIcon. If so, resolve the reference and
    // register the true icon (if resolve is true)

    Icon icon = super.getIcon(iconName, false);

    if (icon != null)
    {
      if (icon == _NULL_ICON)
        return null;

      if (resolve)
      {
        // if icon is a ReferenceIcon, I need to get the actual icon, and
        // re-register it.
        if (icon instanceof ReferenceIcon)
        {
          // go find the actual icon
          Icon resolvedIcon = _resolveReferenceIcon((ReferenceIcon)icon,
                                                     null);

          // register icon so we don't have to resolve reference next time.
          registerIcon(iconName, resolvedIcon);

          return resolvedIcon;
        }
      }
      return icon;
    }

    // If we don't have the icon locally, check the base skin.

    Skin baseSkin = getBaseSkin();
    // get baseSkin's icon. Don't resolve to a real icon. If it is a
    // ReferenceIcon, return the ReferenceIcon.
    icon = baseSkin.getIcon(iconName, false);

    // we found the icon on the base Skin, but it is a ReferenceIcon.
    // find the actual icon
    if (resolve)
    {
      if (icon instanceof ReferenceIcon)
      {
        Icon resolvedIcon = _resolveReferenceIcon((ReferenceIcon)icon,
                                                   null);
        // register icon so we don't have to resolve reference next time.
        registerIcon(iconName, resolvedIcon);

        return resolvedIcon;
      }
    }

    // If we found the icon in the base Skin, register it on ourselves
    // so that we don't have to repeatedly retrieve the icon from
    // the base Skin.
    registerIcon(iconName, icon);


    return icon;
  }

  /**
   * Sets the name of the style sheet for this Skin.
   * @deprecated Use the SkinExtension constructor that takes a styleSheetName instead.
   */
  public void setStyleSheetName(String styleSheetName)
  {
    _styleSheetName = styleSheetName;
  }

  /**
   * Override of Skin.getStyleSheetDocument() which merges
   * styles from the base Skin's style sheet and the
   * SkinExtension's style sheet.
   */
  @Override
  public StyleSheetDocument getStyleSheetDocument(StyleContext context)
  {
    // The SkinExtension's StyleSheetDocument is produced
    // by merging the styles provided by the base Skin with
    // the styles provided by the SkinExtension's own style
    // sheet.

    // Get the StyleSheetDocument from the base Skin
    SkinImpl baseSkin = (SkinImpl) getBaseSkin();
    StyleSheetDocument baseDocument = baseSkin.getStyleSheetDocument(context);

    // Get the StyleSheetDocument for the SkinExtension - we
    // just use super.getStyleSheetDocument() to get our own styles
    StyleSheetDocument extensionDocument = super.getStyleSheetDocument(
                                                   context);

    // Now, check to see if either of the StyleSheetDocuments have
    // changed. We synchronize here to prevent multiple threads coming
    // through and mucking with our StyleSheetDocument instance variables.
    synchronized (this)
    {
      if ((baseDocument != _baseStyleSheetDocument) ||
          (extensionDocument != _extensionStyleSheetDocument))
      {
        _baseStyleSheetDocument = baseDocument;
        _extensionStyleSheetDocument = extensionDocument;

        // One of the StyleSheetDocuments has changed - rebuild
        // the "full" StyleSheetDocument
        _fullStyleSheetDocument =
          StyleSheetDocumentUtils.mergeStyleSheetDocuments(baseDocument,
                                                           extensionDocument);
      }

      return _fullStyleSheetDocument;
    }
  }
  
  /**
   * Set the skin to be dirty. This will force the skin's css file to
   * be reprocessed regardless of whether the css file has been modified 
   * or if the CHECK_FILE_MODIFICATION flag was set.
   * The Skinning Framework sets the dirty flag back to 
   * false once it has reprocessed the skin.
   */
  @Override
  public void setDirty(boolean dirty)
  {
    super.setDirty(dirty);
    // also, set the base skin's dirty flag
    getBaseSkin().setDirty(dirty);
  }

  /**
   * Find the actual icon
   * @param refIcon a ReferenceIcon instance
   * @param referencedIconStack  The stack of reference icon names which have
   *          already been visited.  Used to detect circular dependencies.
   * @return icon which is resolved. i.e., it is not a ReferenceIcon.
   */
  private Icon _resolveReferenceIcon(
    ReferenceIcon refIcon,
    Stack<String> referencedIconStack)
  {

    String refName = refIcon.getName();

    // make sure we don't have a circular dependency
    if ( _stackContains(referencedIconStack, refName))
    {
      if (_LOG.isWarning())
        _LOG.warning(_CIRCULAR_INCLUDE_ERROR + refName);
      return null;
    }

    if (referencedIconStack == null)
    {
      // -= Simon Lessard =-
      // TODO: Check if something better than Stack can be used
      referencedIconStack = new Stack<String>();
    }

    referencedIconStack.push(refName);
    //

    // see if the referenced icon is registered already registered in this skin.
    Icon icon = super.getIcon(refName, false);

    if (icon != null)
    {
      if (icon instanceof ReferenceIcon)
      {
          Icon resolvedIcon = _resolveReferenceIcon((ReferenceIcon)icon,
                                                     referencedIconStack);
          return resolvedIcon;
      }
      else
      {
        return icon;
      }
    }
    else
    {

      Skin baseSkin = getBaseSkin();
      icon = baseSkin.getIcon(refName, false);

      if (icon instanceof ReferenceIcon)
      {
        Icon resolvedIcon = _resolveReferenceIcon((ReferenceIcon)icon,
                                                   referencedIconStack);
        return resolvedIcon;
      }
      else
      {
        return icon;
      }
    }
  }

    // Tests whether the value is present in the (possibly null) stack.
  private static boolean _stackContains(
      Stack<String> stack,
      Object value)
  {
    if (stack == null)
      return false;

    return stack.contains(value);
  }

  private SkinExtension() {}


  // Icon class that we use as a placeholder for null icons
  private static class NullIcon extends Icon
  {
    @Override
    public void renderIcon(
      FacesContext        context,
      RenderingContext    arc,
      Map<String, ? extends Object> attrs
      ) throws IOException
    {
      // null icons don't render anything
    }
  }
  
  private String          _id;
  private String          _family;
  private String          _renderKitId;
  private SkinImpl        _baseSkin;
  private String          _styleSheetName;
  private ValueExpression _translationSourceVE;
  private String          _bundleName;
  private SkinVersion     _version;

  // The StyleSheetDocument for the base LookAndFeel's style sheet
  private StyleSheetDocument _baseStyleSheetDocument;

  // The StyleSheetDocument for the LookAndFeelExtension's style sheet
  private StyleSheetDocument _extensionStyleSheetDocument;

  // The complete StyleSheetDocument which merges the styles
  // provided by the base LookAndFeel with the styles added by
  // the SkinExtension.
  private StyleSheetDocument _fullStyleSheetDocument;

  // Placeholder for null icons
  private static final Icon _NULL_ICON = new NullIcon();
  
  private static final String _DEFAULT_RENDERKIT = 
    "org.apache.myfaces.trinidad.desktop";

  // Error messages
  private static final String _CIRCULAR_INCLUDE_ERROR =
    "Circular dependency detected in skin reference icon ";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SkinExtension.class);
}
