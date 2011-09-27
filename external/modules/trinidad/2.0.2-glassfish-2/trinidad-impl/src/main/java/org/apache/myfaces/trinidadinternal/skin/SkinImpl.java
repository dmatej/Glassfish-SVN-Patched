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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

import java.util.concurrent.ConcurrentHashMap;

import java.util.concurrent.atomic.AtomicInteger;

import javax.el.ELContext;
import javax.el.ValueExpression;

import javax.faces.application.ProjectStage;
import javax.faces.context.ExternalContext;

import javax.faces.context.FacesContext;

import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;

import org.apache.myfaces.trinidad.skin.SkinAddition;
import org.apache.myfaces.trinidad.skin.SkinVersion;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.xml.StyleSheetDocumentUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;

/**
 * Defines the components (icons, styles, etc)
 * which are used to implement a particular skin.
 *
 * This implementation class adds the details that should
 * not be exposed outside of this API.
 *
 * @see SkinFactory
 * @see org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext#getSkinFactory
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/Skin.java#0 $) $Date: 10-nov-2005.18:58:54 $
 */
abstract public class SkinImpl extends Skin implements DocumentProviderSkin
{

  /**
   * Returns an string identifier which uniquely identies
   * this Skin implementation.  Skin implementations
   * can be retrieved by id via SkinFactory.getSkin().
   * @see org.apache.myfaces.trinidadinternal.skin.SkinFactory#getSkin
   */
  @Override
  public String getId()
  {
    return null;
  }

  /**
   * Returns the name of the skin "family" for this skin.
   * The family name is used when specifying a preferred skin
   * in trinidad-config.xml.
   * This provides a way to refer to a group of
   * related skin implementations while allowing the
   * particular skin instance to be selected based on the
   * current render-kit-id.
   */
  @Override
  public String getFamily()
  {
    return null;
  }
  
  @Override
  public SkinVersion getVersion()
  {
    return SkinVersion.EMPTY_SKIN_VERSION;
  }  

  /**
   * Returns the renderKitId for the Skin.
   */
  @Override
  public String getRenderKitId()
  {
    return null;
  }

  /**
   * Returns the id of the Skin's stylesheet document. This is the StyleSheetDocument's
   * id for the StyleContext.
   */
   @Override
  public String getStyleSheetDocumentId(RenderingContext arc)
  {
    StyleContext sContext = ((CoreRenderingContext)arc).getStyleContext();
    return getStyleSheetDocument(sContext).getDocumentId(sContext);
  }

  /**
   * Returns the name of the style sheet for this Skin.
   */
  @Override
  abstract public String getStyleSheetName();

  /**
   * Returns a translated String in the LocaleContext's translation Locale.
   */
  @Override
  public String getTranslatedString(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException
  {
    Object o  = getTranslatedValue(lContext, key);
    if (o == null)
      return null;

    return o.toString();
  }

  /**
   * Returns a translated value in the LocaleContext's translation Locale.
   * This value may or may not be a String, and developers should avoid
   * calling toString() unless absolutely necessary.
   * @param lContext The LocaleContext which provides the translation Locale.
   *                 Cannot be null.
   * @param key The key of the translation to retrieve. Cannot be null.
   * @throws NullPointerException if lContext or key is null.
   * @throws MissingResourceException if the resource key cannot be found in the skin's bundle
   * or the skin additions' bundles.
   */
  @Override
  public Object getTranslatedValue(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException
  {
    if (lContext == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_LOCALE_CONTEXT"));
    if (key == null)
      throw new NullPointerException("Null key");

    List<TranslationSource> translationSourceList =
      _getTranslationSourceList();

    // if there is nothing to check, return null
    if (translationSourceList.size() == 0)
      return null;

    Object translatedValue = getCachedTranslatedValue(lContext, key);
    if (translatedValue == null)
    {
      throw new MissingResourceException("Can't find resource for bundle",
                                         getBundleName(),
                                         key);
    }

    return translatedValue;


  }


  /**
   * Our renderers call this to get the icon. This returns a renderable
   * icon. (ReferenceIcons are resolved -- the real icon they point to is
   * returned)
   */
  @Override
  public Icon getIcon(
    String  iconName
    )
  {
    return getIcon(iconName, true);
  }

  /**
   * Returns an Icon object; can be a ReferenceIcon.
   * @param iconName  The name of the icon to retrieve. Cannot be null
   * @throws NullPointerException if iconName is null.
   */
  @Override
  public Icon getIcon(
    String  iconName,
    boolean resolveIcon
    )
  {
    if (iconName == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_ICONNAME"));

    Icon icon = _icons.get(iconName);
    if (resolveIcon)
    {
      if (icon instanceof ReferenceIcon)
      {
        // find the true icon, not a ReferenceIcon
        icon = SkinUtils.resolveReferenceIcon(this, (ReferenceIcon)icon);
      }
    }

    return icon;
  }

  /**
   * Registers an Icon for the specified icon name.
   * @param iconName  The name of the icon. Cannot be null.
   * @param icon      The Icon to register.
   * @throws NullPointerException if iconName is null.
   */
  @Override
  synchronized public void registerIcon(
    String  iconName,
    Icon    icon
    )
  {
    if (iconName == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_ICONNAME"));

    _icons.put(iconName, icon);
  }

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
  public void addSkinAddition (
    SkinAddition skinAddition
    )
  {
     if (skinAddition == null)
       throw new NullPointerException("NULL_SKINADDITION");

     if (_skinAdditions == null)
     {
       _skinAdditions = new ArrayList<SkinAddition>();
     }
     _skinAdditions.add(skinAddition);
  }

  /**
   * Gets an unmodifiable List of SkinAdditions that have been added
   * on this Skin. To add to the SkinAdditions List,
   * call addSkinAddition(SkinAddition)
   * @return List an unmodifiable List of SkinAdditions.
   * @see #addSkinAddition(SkinAddition)
   */
  public List<SkinAddition> getSkinAdditions()
  {
    if (_skinAdditions == null)
    {
      return Collections.emptyList();
    }
    else
      return Collections.unmodifiableList(_skinAdditions);
  }

   /**
    * Returns the style class map, or null if there is no map.
    * Some StyleProvider implementations, such as the FileSystemStyleCache,
    * automatically provide compressed versions style class names.  The
    * short style classes can be used instead of the full style class
    * names to reduce the overall size of generated content.
    * @param arc RenderingContext
    * @return Map&lt;String, String&gt; It should be a map that contains the full style class name
    * as the key, and the value could be a shortened style class name,
    * or a portlet style class name, etc.
    */
  @Override
   public Map<String, String> getStyleClassMap(
     RenderingContext arc
     )
   {
     FacesContext context = FacesContext.getCurrentInstance();
     if (!_isDisableContentCompressionParameterTrue(context))
     {
        StyleContext sContext = ((CoreRenderingContext)arc).getStyleContext();
        StyleProvider sProvider = sContext.getStyleProvider();  
        // make sure that we want to disable style compression - there could be other
        // reasons that the styleContext knows about.
        if (!(sContext.isDisableStyleCompression()))
          return sProvider.getShortStyleClasses(sContext);
     }
     return null;
   }
   
  /**
   * Returns the StyleSheetDocument object which defines all of the
   * styles for this Skin, including any styles that are
   * contributed by skin-additions.
   */
  public StyleSheetDocument getStyleSheetDocument(StyleContext context)
  {
    // We synchronize here because technically speaking multiple
    // threads can simultaneously update the underlying style sheet
    // objects.  However, in practice, this should never be an issue -
    // especially in production systems where modification checking
    // should always be disabled.  In this case, the style sheet objects
    // are created when the first request is received and are never
    // updated after that point.  So, if this synchronization turns
    // out to be a bottleneck, it should be safe to remove this
    // synchronization - at least when modification checking is
    // disabled, if not altogether.
    synchronized (this)
    {
      if ((_styleSheetDocument == null) || _checkStylesModified(context))
        _styleSheetDocument = _createStyleSheetDocument(context);

      return _styleSheetDocument;
    }
  }

  /**
   * Retrieves a property that was set via a call to setProperty().
   * Some Renderer implementations may store properties on the
   * Skin instance to avoid having to re-compute Skin-specific
   * values on each render.
   */
  @Override
  public Object getProperty(Object key)
  {
    return _properties.get(key);
  }

  /**
   * Sets a value for the specified property key.
   */
  @Override
  public void setProperty(
    Object key,
    Object value
    )
  {
    _properties.put(key, value);
  }


  /**
   * @param styleSheetName
   * @see #addSkinAddition(SkinAddition)
   * @deprecated Use addSkinAddition instead
   */
  public void registerStyleSheet(String styleSheetName)
  {
    //TODO Take out deprecated after sufficient amount of time has passed
    // deprecated July, 2007
    SkinAddition addition = new SkinAddition(styleSheetName);
    addSkinAddition(addition);
  }
  
  /**
   * Check to see if this Skin has been marked dirty. 
   * The only way to mark a Skin dirty is to call setDirty(true).
   * @return true if the Skin is marked dirty. 
   * 
   */
  @Override
  public boolean isDirty()
  {
    return _dirty;
  }

  /**
   * Sets the dirty flag of the Skin. Use this if you want to regenerate the skin. 
   * During rendering, if isDirty is true, 
   * the skin's css file will be reprocessed regardless of whether the css file has been modified 
   * or if the CHECK_FILE_MODIFICATION flag was set. 
   * The Skinning Framework calls setDirty(false) after the skin has been reprocessed.
   */
   @Override
  public void setDirty(boolean dirty)
  {
    _dirty = dirty;
  }

  /**
   * Returns a translated value in the LocaleContext's translation Locale, or null
   * if the key could not be found.
   * This value may or may not be a String, and developers should avoid
   * calling toString() unless absolutely necessary.
   * This method protects against MissingResourceExceptions by checking that the key exists
   * before calling the bundle's getObject method. It eats any MissingResourceExceptions as
   * a result of not finding the bundle, since there can be multiple bundles per skin, and
   * we could get a lot of MissingResourceExceptions otherwise.
   * Then the method caches the value once it is found in a particular
   * resource bundle.
   * This method is useful for SkinExtensions which will also check their ancestor skins
   * for the resource if it is not found in the SkinExtension. MissingResourceExceptions would
   * be numerous if we didn't protect against them.
   * If you want to throw a MissingResourceException once all the ancestor skins and their
   * bundles and registered bundles are checked, then you should call getTranslatedValue for the
   * most base skin, and it will throw a MissingResourceException if the
   * key was not found in any of the bundles.
   * @see #getTranslatedValue(LocaleContext, String)
   * @param lContext The LocaleContext which provides the translation Locale.
   *                 Cannot be null.
   * @param key The key of the translation to retrieve. Cannot be null.
   * @throws NullPointerException if lContext or key is null.
   * @return Object translated value of the key;
   *         null if bundleName and skin-addition bundleNames are null for this Skin;
   *         null if the key cannot be found in the bundle or registered bundles -or-
   *

   */
  protected Object getCachedTranslatedValue(
    LocaleContext lContext,
    String        key
    )
  {
    if (lContext == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_LOCALE_CONTEXT"));
    if (key == null)
      throw new NullPointerException("Null key");

    List<TranslationSource> translationSourceList =
      _getTranslationSourceList();

    return _getCachedTranslationValueFromLocale(lContext,
                                                translationSourceList, key);

  }

  /**
   * Put the locale/key/value in the cache (i.e., translations map). This is useful for subclasses
   * to call so that they can store/retrieve the key/value locally rather than always
   * having to look in the parent skins' maps.
   * @param lContext
   * @param key
   * @param value
   */
  protected void putTranslatedValueInLocaleCache(
    LocaleContext lContext,
    String        key,
    Object        value)
  {
    Locale locale = lContext.getTranslationLocale();

    KeyValueMapStatus keyValueMapStatus = _translations.get(locale);
    if (keyValueMapStatus != null)
    {
      Map keyValueMap = keyValueMapStatus.getKeyValueMap();
      if (keyValueMap != null)
      {
        keyValueMap.put(key, value);
      }
    }
    else
    {
      // in the usual program flow, this won't get called here because the keyValueMapStatus
      // is created in getCachedTranslatedValue's call of _getCachedTranslationValueFromLocale
      // it is here as a safeguard in case the keyValueMapStatus isn't there.
      _createKeyValueMapStatusInCache(locale, key, value);
    }
  }

  /**
  * Returns the name of the ResourceBundle for this Skin instance.
  * This does not include the SkinAddition resource bundles.
  * We differentiate between the two types of resource bundles so that
  * the Skin's own resource bundle can take precedence.
  * Note: A skin cannot have both a bundleName and a translation source
  * value expression. If they do, then the bundlename takes precedence.
  */
  abstract protected String getBundleName();

  /**
  * Returns the ValueExpression of the translation source for this Skin instance.
  * This does not include the SkinAddition translation source.
  * The Skin's own resource bundle or translation source can take precedence
  * over the SkinAdditions resource bundle or translation source.
  * Note: A skin cannot have both a bundleName and a translation source
  * value expression. If they do, then the bundleName takes precedence.
  */
  abstract protected ValueExpression getTranslationSourceValueExpression();

  // Checks to see whether any of our style sheets have been updated
  // or if the skin has been marked dirty
  private boolean _checkStylesModified(
    StyleContext context
    )
  {    
    boolean modified = false;

    if (_skinStyleSheet != null)
      modified = _skinStyleSheet.checkModified(context);

    // We also check all of the skin-addition style sheets even
    // if we already know that the skin's style sheet has been
    // modified.  We need to do this because we want to call
    // StyleSheetEntry.checkModified() for each entry - otherwise
    // out of date StyleSheetEntries may not get updated.
    if (_skinAdditionStyleSheets != null)
    {
      for (int i = 0; i < _skinAdditionStyleSheets.length; i++)
      {
        StyleSheetEntry entry = _skinAdditionStyleSheets[i];
        if (entry.checkModified(context))
          modified = true;
      }
    }

    return modified;
  }


  // Creates the StyleSheetDocument for this Skin
  // (as a side effect, this also registers icons and skin properties
  // defined in the skin's style-sheet.)
  private StyleSheetDocument _createStyleSheetDocument(
    StyleContext context
    )
  {
    // If we haven't created the StyleSheetDocument yet,
    // then presumably we haven't attempted to load the
    // style sheet files.  Create StyleSheetEntry instances
    // for each style sheet file that we depend on.
    if (_styleSheetDocument == null)
    {
      String styleSheetName = getStyleSheetName();

      if (styleSheetName != null)
      {
        _skinStyleSheet = StyleSheetEntry.createEntry(context, styleSheetName);
      }

      // Now create entries for skin-addition-specific style sheets.
      _skinAdditionStyleSheets = _getSkinAdditionsStyleSheets(context);
    }

    // Now merge all of the documents provided by all of our
    // entries into a single StyleSheetDocument.
    StyleSheetDocument document = null;

    if (_skinStyleSheet != null)
      document = _skinStyleSheet.getDocument();

    // Merge in any skin-addition style sheets on top of
    // the skin's style sheet
    if (_skinAdditionStyleSheets != null)
    {
      for (int i = 0; i < _skinAdditionStyleSheets.length; i++)
      {
        StyleSheetEntry entry = _skinAdditionStyleSheets[i];
        if (entry != null)
        {
          // Merge the skin-addition's StyleSheetDocument on top of
          // the current StyleSheetDocument.
          StyleSheetDocument additionDocument = entry.getDocument();

          if (additionDocument != null)
          {
            // Merge the skin-addition's StyleSheetDocument on top of
            // the current StyleSheetDocument.  Note: This is not
            // exactly efficient - we would be better off creating
            // an array of StyleSheetDocuments and merging them all
            // in one pass.  But since this code should rarely be
            // executed, this shouldn't be a bottleneck...
            document = StyleSheetDocumentUtils.mergeStyleSheetDocuments(
                                                 document,
                                                 additionDocument);

          }
        }
      }
    }

    // We're done!
    if (document != null)
      return document;

    // If we weren't able to produce a StyleSheetDocument for whatever
    // reason (maybe we don't have any style sheet, maybe there were
    // I/O problems), create a empty StyleSheetDocument so that we
    // don't repeatedly try to re-create the document.
    return new StyleSheetDocument(null,
                                  null,
                                  StyleSheetDocument.UNKNOWN_TIMESTAMP);
  }

  // Gets the StyleSheetEntries for skin-additions
  private StyleSheetEntry[] _getSkinAdditionsStyleSheets(StyleContext context)
  {
    List<String> skinAdditionStyleSheetNames = _getSkinAdditionsStyleSheetNames();
    if (skinAdditionStyleSheetNames.size() == 0)
      return null;

    // Create a list to hold our StyleSheetEntries
    int count = skinAdditionStyleSheetNames.size();
    List<StyleSheetEntry> entries = new ArrayList<StyleSheetEntry>(count);

    // Loop through all registered style sheet names and
    // try to create a StyleSheetEntry for each name.
    for(String name : skinAdditionStyleSheetNames)
    {
      StyleSheetEntry entry = StyleSheetEntry.createEntry(context, name);
      if (entry != null)
      {
        entries.add(entry);
      }
    }

    if (!entries.isEmpty())
    {
      _skinAdditionStyleSheets = new StyleSheetEntry[entries.size()];
      return entries.toArray(_skinAdditionStyleSheets);
    }

    return null;
  }

  /*
   * Returns a List of skin-addition style sheets for the Skin.
   * These stylesheets are added with addSkinAddition.
   * This List does not include the skin's own stylesheet.
   * @return List<String> of skin addition stylesheet names. It will
   * return a List of size 0 if no skin addition stylesheets exist.
   * @see #addSkinAddition(SkinAddition)
   * @see #getStyleSheetName()
   */
  private List<String> _getSkinAdditionsStyleSheetNames()
  {
    // Get all the SkinAdditions's style sheet names.
    // Get the style sheet names and create a List
    // Cache this list in an instance variable

    if (_skinAdditionStyleSheetNames != null)
      return _skinAdditionStyleSheetNames;

    // loop through all the SkinAdditions and get the resource bundles
    List<SkinAddition> additions = getSkinAdditions();

    List<String> styleSheetNames = new ArrayList<String>(additions.size());

    for (SkinAddition addition : additions)
    {
      String name = addition.getStyleSheetName();
      if (name != null)
      {
        styleSheetNames.add(name);
      }
    }

    // cache in instance variable
    _skinAdditionStyleSheetNames = styleSheetNames;

    return _skinAdditionStyleSheetNames;
  }

  /*
   * Returns the List of TranslationSource. A TranslationSource can be
   * a resource bundle name or a translation-source ValueExpression that
   * resolves to a Map or a ResourceBundle.
   * The List indlues TranslationSources from the Skin and the SkinAdditions.
   * @see #getBundleName()
   * @see #getTranslationSourceValueExpression()
   */
  private List<TranslationSource> _getTranslationSourceList()
  {
    // Get the list of translation sources for this Skin.
    // It checks the Skin and the SkinAdditions.
    // Cache this list in instance variable

    // return if already cached
    if (_translationSourceList != null)
      return _translationSourceList;

    // first figure out how many translation sources we have by checking
    // the Skin and the SkinAdditions.
    int translationSourceCount = 0;

    String bName = getBundleName();
    ValueExpression ve = null;
    if (bName == null)
      ve = getTranslationSourceValueExpression();
    if (bName != null || ve != null)
      translationSourceCount++;

    List<SkinAddition> additions = getSkinAdditions();
    // quick assumption is that the skin additions have translation information.
    translationSourceCount += additions.size();

    List<TranslationSource> translationSourceList =
      new ArrayList<TranslationSource>(translationSourceCount);

    // First put in the Skin's translation information, then the SkinAdditions'
    // translation information.
    // Note: bundleName takes precedence over translationSourceValueExpression.
    if (bName != null)
      translationSourceList.add(new ResourceBundleNameTranslationSource(bName));
    else if (ve != null)
      translationSourceList.add(new ValueExprTranslationSource(ve));

    for (SkinAddition add : additions)
    {
      String name = add.getResourceBundleName();
      if (name != null)
        translationSourceList.add(new ResourceBundleNameTranslationSource(name));
      else
      {
        ValueExpression additionVe = add.getTranslationSourceValueExpression();
        if (additionVe != null)
          translationSourceList.add(new ValueExprTranslationSource(additionVe));
        else
        {
          // try the deprecated ValueBinding last.
          // This code can be deleted when we delete the deprecated api
          // SkinAddition's getTranslationSourceValueBinding
        ValueBinding additionVb = add.getTranslationSourceValueBinding();
        if (additionVb != null)
          translationSourceList.add(new ValueBindingTranslationSource(additionVb));
        }
      }
    }

    // cache in instance variable
    _translationSourceList = translationSourceList;

    return _translationSourceList;
  }

  // get the cached value for the locale and key from the _translations map.
  // If the value does not exist, then find it in the resource bundles,
  // searching the Skin's bundle first, then each skin addition resource
  // bundle until it is found. This method fills in the cached key/value map
  // as we look for the key/value. It keeps track of which bundles we looked
  // in so that we don't have to look in them any more for this session.
  private Object _getCachedTranslationValueFromLocale(
    LocaleContext lContext,
    List<TranslationSource> translationSourceList,
    String        key
    )
  {
    Locale locale = lContext.getTranslationLocale();

    KeyValueMapStatus keyValueMapStatus = _translations.get(locale);
    Map keyValueMap = null;

    if (keyValueMapStatus != null)
    {
      keyValueMap = keyValueMapStatus.getKeyValueMap();
      if (keyValueMap != null)
      {
        Object value = keyValueMap.get(key);
        if (value != null)
        {
          return value;
        }
      }
    }
    else
    {
      // create the keyValueMapStatus object and put it on the locale

      keyValueMapStatus = _createKeyValueMapStatusInCache(locale, key, null);
      keyValueMap = keyValueMapStatus.getKeyValueMap();

    }


    // at this point the keyValueMapStatus is set on the locale,
    // and we know we have to fill it in.
    // getProcessedBundlesIndex will tell us which resource bundles
    // we have already processed (locale bundle + skin-addition bundles)
    // we increment this number after we look in each bundle and update
    // the keyValueMap.

    int numberOfTranslationSources = translationSourceList.size();
    // if there is nothing to check, return null
    if (numberOfTranslationSources == 0)
      return null;

    // in theory, multiple threads could get the same processedBundleIndex
    // here, so we could get all these threads updating the same map, but
    // it will eventually update the index, so I won't worry about this now.
    int startIndex = keyValueMapStatus.getProcessedBundlesIndex();
    for (int i=startIndex; i < numberOfTranslationSources;)
    {
      TranslationSource translationSource = translationSourceList.get(i);
      // 'true' means to check if the key already exists in the keyValueMap and
      // if so do not override. The first time true we don't bother checking.
      translationSource.fillInKeyValueMap(lContext, keyValueMap, (i != 0));

      i = keyValueMapStatus.incrementAndGetProcessedBundlesIndex();
      Object value = keyValueMap.get(key);
      if (value != null)
      {
        return value;
      }
    }

    // nothing was found
    return null;

  }

  // this method provides a single point of entry for creating KeyValueMapStatus
  // object and putting the locale/keyValueMapStatus in the _translations map.
  // If value != null, it adds the key/value to the keyValueMap.
  // It synchronizes on the _translations parameter
  // It returns the newly created KeyValueMapStatus object.
  private KeyValueMapStatus _createKeyValueMapStatusInCache(
    Locale locale,
    String key,
    Object value
  )
  {
    KeyValueMapStatus keyValueMapStatus = null;

    // create the keyValueMapStatus object and put it on the locale
    synchronized (_translations)
    {
      // check to see if another thread has put locale in the map
      if (!_translations.contains(locale))
      {
        keyValueMapStatus = new KeyValueMapStatus();
        if (value != null)
        {
          Map keyValueMap = keyValueMapStatus.getKeyValueMap();
          keyValueMap.put(key, value);
        }
        _translations.put(locale, keyValueMapStatus);
      }
      else
      {
        keyValueMapStatus = _translations.get(locale);
      }
    }

    return keyValueMapStatus;

  }

  // fill in the keyValueMap from a ResourceBundle
  // If checkForKey is true, it will not overwrite if the key exists already.
  private static void _fillInKeyValueMapFromResourceBundle (
    ResourceBundle bundle,
    Map            keyValueMap,
    boolean        checkForKey)
  {
    if (bundle != null)
    {
      Enumeration<String> en = bundle.getKeys();

      if (en != null)
      {
        while (en.hasMoreElements())
        {
          String bundleKey = en.nextElement();
          // if checkForKey is true, don't override an existing key/value
          if (checkForKey)
          {
            if (!keyValueMap.containsKey(bundleKey))
            {
              Object value = bundle.getObject(bundleKey);
              if (value != null)
                keyValueMap.put(bundleKey, value);
            }
          }
          else
          {
            Object value = bundle.getObject(bundleKey);
            if (value != null)
              keyValueMap.put(bundleKey, value);
          }
        }
      }
    }
  }


  // fill in the keyValueMap from a Map.
  // If checkForKey is true, it will not overwrite if the key exists already.
  private static void _fillInKeyValueMapFromMap(
    Map<String, String> translationSourceMap,
    Map                 keyValueMap,
    boolean             checkForKey)
  {

    if (translationSourceMap != null)
    {
      if (!checkForKey)
        keyValueMap.putAll(translationSourceMap);
      else
      {
        // go through each key and put the key/value in the map if the key
        // isn't already in the map.
        Set<Map.Entry<String, String>> keys = translationSourceMap.entrySet();
        if (keys != null)
        {
          for(Map.Entry<String, String> entry : translationSourceMap.entrySet())
          {
            String translationKey = entry.getKey();

            if (!keyValueMap.containsKey(translationKey))
            {
              Object translationValue = entry.getValue();
              if (translationValue != null)
                keyValueMap.put(translationKey, translationValue);
            }
          }
        }
      }
    }
  }
  
  // returns true if the web.xml explicitly has DISABLE_CONTENT_COMPRESSION set to true.
  // else return false.
  private boolean _isDisableContentCompressionParameterTrue(FacesContext context)
  {
    // TODO: this section needs to be MOVED up, perhaps to API,
    // as the StyleContextIMPL.java has exactly the same code;
    // this will be fixed with the advent of "TRINIDAD-1662".
    
    String disableContentCompression = context.getExternalContext().
      getInitParameter(Configuration.DISABLE_CONTENT_COMPRESSION);

    boolean disableContentCompressionBoolean; 

    // what value has been specified for the DISABLE_CONTENT_COMPRESSION param?
    if (disableContentCompression != null)
    {
      disableContentCompressionBoolean = "true".equals(disableContentCompression);
    }
    else 
    {
      // if the DISABLE_CONTENT_COMPRESSION parameter has NOT been specified, let us
      // apply the DEFAULT values for the certain Project Stages:
      // -PRODUCTION we want this value to be FALSE;
      // -other stages we use TRUE
      disableContentCompressionBoolean = !(context.isProjectStage(ProjectStage.Production));
    }

    // if Apache MyFaces Trinidad is running in production stage and 
    // running with content compression disabled we generate a WARNING
    // message
    if (disableContentCompressionBoolean && context.isProjectStage(ProjectStage.Production))
    {
      _LOG.warning("DISABLE_CONTENT_COMPRESSION_IN_PRODUCTION_STAGE");
    }
    return disableContentCompressionBoolean;
  }

  // a TranslationSource fills in the keyValueMap differently depending upon
  // if it is a map or a ResourceBundle.
  private static interface TranslationSource
  {
    public abstract void fillInKeyValueMap(
      LocaleContext         lContext,
      Map                   keyValueMap,
      boolean               checkForKey);
  }

  private static class ResourceBundleNameTranslationSource
    implements TranslationSource
  {
    public ResourceBundleNameTranslationSource(String bundleName)
    {
      _bundleName = bundleName;
    }

    // fill in the keyValueMap from a bundle name -- finds the
    // ResourceBundle based on locale and then calls
    // _fillInKeyValueMapFromResourceBundle
    // If checkForKey is true, it will not overwrite if the key exists already.
    public void fillInKeyValueMap(
      LocaleContext lContext,
      Map           keyValueMap,
      boolean       checkForKey)
    {
      ResourceBundle bundle = null;

      try
      {
        bundle = lContext.getBundle(_bundleName);
      }
      catch (MissingResourceException e)
      {
         // It is possible that the call to getBundle() might
         // fail with a MissingResourceException if the customer
         // has only provided a custom bundle for certain languages.
         // This is okay, so we just eat these exceptions.
         ;
      }

      _fillInKeyValueMapFromResourceBundle(bundle, keyValueMap, checkForKey);

    }

    public final String        _bundleName;
  }

  private static class ValueExprTranslationSource implements TranslationSource
  {

    public ValueExprTranslationSource(
      ValueExpression ve)
    {
      _translationSourceVE = ve;
    }

    // fill in the keyValueMap from a ValueExpression. The ValueExpression
    // types that we support are Map and ResourceBundle.
    // If checkForKey is true, it will not overwrite if the key exists already.
    public void fillInKeyValueMap(
      LocaleContext  lContext,
      Map            keyValueMap,
      boolean        checkForKey)
    {
      ELContext elContext = FacesContext.getCurrentInstance().getELContext();

      Object veValue = _translationSourceVE.getValue(elContext);

      if (veValue instanceof Map)
      {
        Map<String, String> translationSourceMap =
          (Map<String, String>)_translationSourceVE.getValue(elContext);
        _fillInKeyValueMapFromMap(translationSourceMap, keyValueMap, checkForKey);
      }
      else if (veValue instanceof ResourceBundle)
      {
        ResourceBundle bundle =
          (ResourceBundle)_translationSourceVE.getValue(elContext);
        _fillInKeyValueMapFromResourceBundle(bundle, keyValueMap, checkForKey);
      }
      else
      {
        _LOG.warning("INVALID_TRANSLATION_SOURCE_VE_TYPE");
      }
    }

    public final ValueExpression _translationSourceVE;
  }

  private static class ValueBindingTranslationSource implements TranslationSource
  {

    public ValueBindingTranslationSource(
      ValueBinding vb)
    {
      _translationSourceVB = vb;
    }

    // fill in the keyValueMap from a ValueBinding. The ValueBinding
    // types that we support are Map and ResourceBundle.
    // If checkForKey is true, it will not overwrite if the key exists already.
    public void fillInKeyValueMap(
      LocaleContext  lContext,
      Map            keyValueMap,
      boolean        checkForKey)
    {
      FacesContext fContext = FacesContext.getCurrentInstance();

      Object veValue = _translationSourceVB.getValue(fContext);

      if (veValue instanceof Map)
      {
        Map<String, String> translationSourceMap =
          (Map<String, String>)_translationSourceVB.getValue(fContext);
        _fillInKeyValueMapFromMap(translationSourceMap, keyValueMap, checkForKey);
      }
      else if (veValue instanceof ResourceBundle)
      {
        ResourceBundle bundle =
          (ResourceBundle)_translationSourceVB.getValue(fContext);
        _fillInKeyValueMapFromResourceBundle(bundle, keyValueMap, checkForKey);
      }
      else
      {
        _LOG.warning("INVALID_TRANSLATION_SOURCE_VE_TYPE");
      }
    }

    public final ValueBinding _translationSourceVB;
  }


  // This is the 'value' of the _translations map.
  // This contains a translation key/value map which contains
  // all the translation keys and values in the resource bundles
  // we have processed thus far for a particular locale.
  // It also contains an index which keeps track of how
  // many of the bundles we have checked so far, so that
  // we don't recheck a bundle.
  // This is to help with performance, since getting
  // values from a resource bundle is expensive.
  private static class KeyValueMapStatus
  {

    KeyValueMapStatus()
    {
      _keyValueMap = new ConcurrentHashMap<String, Object>();
      _processedBundlesIndex = new AtomicInteger(0);
    }

    // get the current key/value Map
    public Map<String, Object> getKeyValueMap()
    {
      return _keyValueMap;
    }

    // Get the current value of processedBundlesIndex.
    public int getProcessedBundlesIndex()
    {
      return _processedBundlesIndex.get();
    }

    // Atomically increment by one the current value of processBundlesIndex.
    // @return the updated value
    public int incrementAndGetProcessedBundlesIndex()
    {
      return _processedBundlesIndex.incrementAndGet();
    }

    Map<String, Object> _keyValueMap;
    // This keeps track of the number of bundles that have been processed.
    // A Skin can have multiple bundles registered on it -- a local resource
    // bundle + any number of skin-addition bundles.
    // When we get a key (getTranslatedValue), we check each bundle
    // and fill in the keyValueMap until we find the key.
    // We update this index after we process each
    // bundle, so that we don't recheck a bundle. We only have to check a bundle
    // once per locale per session, because we cache the keys/values for each
    // bundle we check in the _keyValueMap.
    AtomicInteger       _processedBundlesIndex;
  }

  // Now that we look into possibly multiple ResourceBundles
  // to find a translation (eg. the local bundle + a skin-addition's
  // bundle), translation lookups can become expensive.
  // To get a value, we call: lContext.getBundle(name).getObject(key).
  // We speed things up by caching the translations
  // in this _translations map.
  // As we get a call to getTranslatedValue with a key, we
  // look through our keyValueMap. If it isn't there, we loop
  // through each resource bundle we haven't yet checked,
  // and we get all the keys and values
  // and when we have all the keys/values for the
  // bundle, we return if the key/value is there. Otherwise,
  // we check the next bundle and so on.
  // If we never get a request for a key in bundle X, that bundle X's
  // keys/values will never be put in the keyvalue map. This is a good thing.
  private ConcurrentHashMap<Locale, KeyValueMapStatus> _translations =
    new ConcurrentHashMap<Locale, KeyValueMapStatus>(13);

  // HashMap that maps icon name to Icons
  private ConcurrentHashMap<String, Icon> _icons = new ConcurrentHashMap<String, Icon>();

  // The StyleSheetDocument which contains all of the styles
  // for this Skin - including styles contributed by skin-additions.
  private StyleSheetDocument _styleSheetDocument;

  // A StyleSheetEntry which defines the styles that are
  // provided by this Skin's style sheet only (does
  // not include skin-additions styles).
  private StyleSheetEntry _skinStyleSheet;

  // List of skin-additions style sheet names for this Skin
  private List<String> _skinAdditionStyleSheetNames;

  // Array of skin-additions StyleSheetEntries
  private StyleSheetEntry[] _skinAdditionStyleSheets;

  // List of all the translation sources for this Skin. A translation
  // source is the bundle name or the translation source (Map or ResourceBundle)
  // plus all the SkinAdditions translation sources.
  private List<TranslationSource> _translationSourceList;

  // List of skin-additions for this Skin
  private List<SkinAddition> _skinAdditions;

  // HashMap of Skin properties
  private ConcurrentHashMap<Object, Object> _properties= new ConcurrentHashMap<Object, Object>();

  private boolean _dirty;
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SkinImpl.class);
}
