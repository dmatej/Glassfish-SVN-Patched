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
package org.apache.myfaces.trinidadinternal.style.cache;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.style.Selector;
import org.apache.myfaces.trinidad.style.Style;
import org.apache.myfaces.trinidad.style.Styles;
import org.apache.myfaces.trinidad.util.ArrayMap;
import org.apache.myfaces.trinidad.util.CollectionUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinProperties;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.share.expl.Coercions;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.UnmodifiableStyle;
import org.apache.myfaces.trinidadinternal.style.util.CSSGenerationUtils;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.style.util.StyleWriterFactory;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IconNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;


/**
 * The FileSystemStyleCache is a StyleProvider implementation which
 * caches generated CSS style sheets on the file system. A FileSystemStyleCache object is for one Skin.
 * The FileSystemStyleCache instance contains Entry objects for a Skin: one Entry
 * object per unique generated CSS style sheet (e.g., gecko and ie will most
 * likely have different generated CSS style sheets for the same Skin because these browsers tend to need
 * different css rules).
 *
 * Note that StyleProviders are responsible for providing access
 * both to style information (eg. getStyleSheetURI(), getStyles()) as
 * well as to icons registered via style sheets (see getIcons()).
 *
 * @see org.apache.myfaces.trinidadinternal.style.StyleProvider
 * @see org.apache.myfaces.trinidadinternal.skin.SkinStyleProvider
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/cache/FileSystemStyleCache.java#0 $) $Date: 10-nov-2005.18:58:54 $
 */
public class FileSystemStyleCache implements StyleProvider
{
  /**
   * Returns the mime type for the styles provided by this
   * FileSystemStyleCache - "text/css".
   */
  public String getContentStyleType(StyleContext context)
  {
    return "text/css";
  }

  /**
   * Creates a FileSystemStyleCache.
   *
   * @param target The path of the target directory.  Generated
   *   CSS files are stored in this directory.  If the directory
   *   does not exist and can not be created, an IllegalArgumentException
   *   is thrown.
   *  @see org.apache.myfaces.trinidadinternal.skin.SkinStyleProvider - the subclass
   */
  protected FileSystemStyleCache(String target)
  {
    // If the target directory does not exist, create it now.
    // Note: If we can't create the target directory, we just
    // plug along anyway instead of throwing an IllegalArgumentException.
    // That way, we can still use the Styles for this style sheet
    // even if the style sheet isn't generated.
    File targetDirectory = new File(target);
    if (!targetDirectory.exists())
      targetDirectory.mkdirs();

    _targetPath = target;
  }

  /**
   * Implementation of StyleCache.getStyleSheetURI().
   */
  public List<String> getStyleSheetURIs(StyleContext context)
  {    
    Entry entry = _getEntry(context);

    if (entry == null)
    {
      return Collections.emptyList();
    }
    else
    {
      return entry.uris;
    }
  }

  /**
   * Implementation of StyleProvider.getStyles().
   */
  public Styles getStyles(StyleContext context)
  {

    Entry entry = _getEntry(context);

    if (entry == null)
      return null;
    else
      return entry.styles;
  }

  /**
   * Implementation of StyleProvider.getSkinProperties()
   */
  public ConcurrentMap<Object, Object> getSkinProperties(StyleContext context)
  {

    Entry entry = _getEntry(context);

    if (entry == null)
      return null;
    else
      return entry.skinProperties;
  }

  /**
   * Implementation of StyleProvider.getIcons()
   */
  public ConcurrentMap<String, Icon> getIcons(StyleContext context)
  {

    Entry entry = _getEntry(context);

    if (entry == null)
      return null;
    else
      return entry.icons;
  }

  /**
   * Returns a Map which maps style class names to
   * equivalent shorter names.
   * <p>
   * FileSystemStyleCache automatically generates short versions
   * of every style class that is found the the underlying XSS
   * document.  FileSystemStyleCache clients can reduce the
   * size of generated content by using this method to obtain
   * short versions of any rendered style classes.
   * <p>
   * Note: The returned Map uses String keys to represent
   * the full class names.  However, the short style class values
   * may not necessarily be type java.lang.String.  Clients must
   * avoid explicitly casting the values contained in the Map
   * to type String.  Instead, such values should be passed directly
   * to the ResponseWriter API to be rendered.  Or, if the String
   * representation is required, toString() should be called on
   * the value.
   *
   * @param context The StyleContext
   *
   * @return A Map which maps the full style class names to
   *   the shorter equivalents.
   */
  public Map<String, String> getShortStyleClasses(StyleContext context)
  {
    return _shortStyleClassMap;
  }

  /**
   * Creates the StyleSheetDocument for this StyleProvider.
   * @param context The StyleContext
   *                (not needed here, but is needed in  subclass)
   * @return The StyleSheetDocument which defines the styles
   *         for this StyleProvider.
   * @see org.apache.myfaces.trinidadinternal.skin.SkinStyleProvider#createStyleSheetDocument(context)
   */
  protected StyleSheetDocument createStyleSheetDocument(
    StyleContext context
    )
  {
    return null;
  }

  /**
   * Tests whether the source style sheet files have been modified
   * since the last call to createStyleSheetDocument().
   * @return true if the underlying source style sheets have been
   *              modified, false otherwise.
   */
  protected boolean hasSourceDocumentChanged(StyleContext context)
  {
    // If we haven't parsed yet, don't bother checking the time stamp
    if (_document == null)
      return true;
    return false;
  }

  /**
   * Returns the name to use for the generated style sheet file .
   *
   * @param context The StyleContext
   * @param document The StyleSheetDocument which provides the styles
   */
  protected String getTargetStyleSheetName(
    StyleContext       context,
    StyleSheetDocument document
    )
  {
    StringBuilder buffer = new StringBuilder();

    String contextName = NameUtils.getContextName(context, document);
    if ((contextName != null) && contextName.length() > 0)
    {
      buffer.append(contextName);
    }


    boolean compressedStyles = _isCompressStyles(context);
    if (compressedStyles)
    {
      if (contextName != null)
        buffer.append(_NAME_SEPARATOR);
      buffer.append(_COMPRESSED);
    }

    if (context.isPortletMode())
    {
      if (contextName != null || compressedStyles)
        buffer.append(_NAME_SEPARATOR);

      buffer.append(_PORTLET);
    }
    
    if (context.isRequestSecure())
    {
      buffer.append(_NAME_SEPARATOR);
      buffer.append(_SECURE);
    }

    buffer.append(_CSS_EXTENSION);

    return buffer.toString();
  }

  // Returns the current StyleSheetDocument - used by StyleMapImpl only
  StyleSheetDocument __getStyleSheetDocument()
  {
    return _document;
  }

  // Gets the entry for the specified StyleContext, creating it if
  // necessary. Part of creating an entry is creating the style sheet file itself.
  // And Entry contains the style sheet URI.
  private Entry _getEntry(StyleContext context)
  {
    ConcurrentMap<Key, Entry> cache = null;
    ConcurrentMap<Object, Entry> entryCache = null;
    StyleSheetDocument document = null;
    Map<String, String> shortStyleClassMap = null;
    String[] namespacePrefixes = null;
    boolean isDirty = context.isDirty();
    boolean checkModified  = context.checkStylesModified();

    // Synchronize while set up the _cache, _entryCache, _document, etc...
    synchronized (this)
    {
      // If checking for modified files, then check to see if the XSS or CSS
      // document has been modified.  If so, we dump our in-memory style cache.
      if (isDirty || (checkModified && hasSourceDocumentChanged(context)))
      {
        _cache = null;
        _entryCache = null;
        _document = null;
        _reusableStyleMap = null;
        _reusableSelectorMap = null;
        _shortStyleClassMap = null;
        _namespacePrefixes  = null;
      }

      // We get references to our two caches (the "normal" cache,
      // and the cache of shared Entry objects) up front.  We do
      // this because the actual caches could change at any time.
      // (The caches get reallocated if the source document is
      // modified.)  We need to use a consistent set of caches
      // throughout the entire request, to avoid adding bogus entries
      // to a new re-allocated cache.
      if (_cache == null)
        _cache = new ConcurrentHashMap<Key, Entry>();
      if (_entryCache == null)
        _entryCache = new ConcurrentHashMap<Object, Entry>(19);
      if (_reusableStyleMap == null)
        _reusableStyleMap = new ConcurrentHashMap<Style, Style>();
      if (_reusableSelectorMap == null)
        _reusableSelectorMap = new ConcurrentHashMap<Selector, Selector>();

      cache = _cache;
      entryCache = _entryCache;

      // Get the document up front too.
      // Returns the StyleSheetDocument, parsing the source file if necessary
      // this sets up _shortStyleClassMap and _namespacePrefixes
      document = _getStyleSheetDocument(context);
      if (document == null)
        return null;

      shortStyleClassMap = _shortStyleClassMap;
      namespacePrefixes = _namespacePrefixes;
    }

    // Look up the style sheet
    // The Key class is a private static class that is used for hashing. It implements
    // hashCode and equals which are based on locale, direction, browser, version, platform.
    Key key = new Key(context);
    if (_LOG.isFinest())
    {
      _LOG.finest("FileSystemStyleCache's Key's hashCode is ", key.hashCode());
    }
    Entry entry = _getEntry(cache, key, checkModified);
    if (entry != null)
      return entry;

    // Next see if this is an entry which is compatible with this request
    entry = _getCompatibleEntry(context, document, cache, key, entryCache, checkModified);

    if (entry != null)
      return entry;

    // If we didn't find an entry in the cache, create a new entry
    // This generates the CSS file.
    return _createEntry(context,
                        document,
                        cache,
                        key,
                        entryCache,
                        shortStyleClassMap,
                        namespacePrefixes,
                        checkModified, 
                        isDirty);
  }

  private Entry _getEntry(
    ConcurrentMap<?, Entry> cache,
    Object                  key,
    boolean                 checkModified
    )
  {
    Entry entry = cache.get(key);
    if (entry == null)
    {
      return null;
    }

    if (checkModified)
    {
      List<String> uris = entry.uris;
      assert uris != null && !uris.isEmpty();

      boolean valid = true;
      List<File> existing = new LinkedList<File>();
      // Make sure the entry's file exists.  If it no longer
      // exists, we remove the entry from the cache
      for (String name : uris)
      {
        File file = new File(_targetPath, name);
        if (file.exists())
        {
          existing.add(file);
        }
        else
        {
          valid = false;
        }
      }

      if (!valid)
      {
        _deleteAll(existing);
        
        // atomically remove the key from the cache if it currently points to the entry
        cache.remove(key, entry);

        return null;
      }
    }

    return entry;
  }

  /**
   * Creates and caches an Entry for the specified StyleContext
   * This generates a style sheet for the specific StyleContext
   * (locale, direction, etc), and puts that style sheet's uri in the Entry.
   * It also caches it in the "normal" cache (the one that is based on the StyleContext),
   * and the entry cache (the one that is based on the StyleSheetNodes)
   */
  private Entry _createEntry(
    StyleContext                 context,
    StyleSheetDocument           document,
    ConcurrentMap<Key, Entry>    cache,
    Key                          key,
    ConcurrentMap<Object, Entry> entryCache,
    Map<String, String>          shortStyleClassMap,
    String[]                     namespacePrefixes,
    boolean                      checkModified,
    boolean                      isDirty)
  {
    // Next, get the fully resolved styles for this context. This will be
    // those StyleNodes that match the locale, direction, browser, portlet mode
    // etc -- the info that is in the StyleContext.
    // These styles contain all the StyleNodes, that is, where selector or
    // name (aka alias) are non-null.
    // If a selector has no properties at all (af|foo {}), it will not be returned in the list of
    // StyleNodes. It gets into the shortStyleClassMap which has already happened, but
    // it won't get written to the CSS File.
    StyleNode[] styleNodes = _getStyleContextResolvedStyles(context, document);
    if (styleNodes == null)
      return null;

    /* This code fills in the <Selector, Style> resolvedSelectorStyleMap map. 
     * We use _reusableStyleMap to reuse the Style objects when possible
     * since we have a large number of Style objects. */
    ConcurrentMap<Selector, Style> resolvedSelectorStyleMap = null;
    for (int i=0; i < styleNodes.length; i++)
    {
      String selector = styleNodes[i].getSelector();
      if (selector != null)
      {
        Style style = _convertStyleNodeToStyle(styleNodes[i], _reusableStyleMap);
        if (resolvedSelectorStyleMap == null)
          resolvedSelectorStyleMap = new ConcurrentHashMap<Selector, Style>();

        // To save memory, we reuse Selector objects
        Selector selectorCreated = Selector.createSelector(selector);
        Selector cachedSelector = _reusableSelectorMap.get(selectorCreated);
        if (cachedSelector == null)
        {
          _reusableSelectorMap.put(selectorCreated, selectorCreated);
          resolvedSelectorStyleMap.put(selectorCreated, style);
        }
        else
        {
          resolvedSelectorStyleMap.put(cachedSelector, style);
        }
        
      }
    }
    
    // Generate the style sheet file, if it isn't already generated,
    // and return the uri.
    // Only the StyleNodes with non-null selectors get written to the
    // generated css file.
    // Named styles (StyleNode where name != null) do not get
    // written to the generated css file.
    List<String> uris = _createStyleSheetFiles(context,
                                       document,
                                       styleNodes,
                                       shortStyleClassMap,
                                       namespacePrefixes,
                                       checkModified,
                                       isDirty);

    _LOG.fine("Finished processing stylesheet {0}", uris);


    // Next, get the fully resolved icons and skin properties for this context.
    // This will be those Icons and Skin Properties that match the locale, direction,
    // browser, etc -- the info that is in the StyleContext
    ConcurrentMap<String, Icon> icons =
      _getStyleContextResolvedIcons(context, document);
    ConcurrentMap<Object, Object> skinProperties =
      _getStyleContextResolvedSkinProperties(styleNodes);

    // Create a new entry and cache it in the "normal" cache. The "normal" cache is one
    // where the key is the Key object which is built based on information from the StyleContext,
    // like browser, agent, locale, direction.
    Styles styles = new StylesImpl(namespacePrefixes, _STYLE_KEY_MAP,
                                   shortStyleClassMap,  _isCompressStyles(context), resolvedSelectorStyleMap);
    Entry entry = new Entry(uris, styles, icons, skinProperties);
    cache.put(key, entry);

    // Also, cache the new entry in the entry cache
    DerivationKey derivationKey = _getDerivationKey(context, document);
    entryCache.put(derivationKey, entry);
    
    // just in case, clear the dirty flag.
    RenderingContext arc = RenderingContext.getCurrentInstance();
    Skin skin = arc.getSkin();
    skin.setDirty(false);

    return entry;
  }

  /**
   * Look in the entry cache for a compatible entry.
   * A compatible entry is one with the same DerivationKey, which is essentially the
   * same StyleSheetNodes.
   */
  private Entry _getCompatibleEntry(
    StyleContext                 context,
    StyleSheetDocument           document,
    ConcurrentMap<Key, Entry>    cache,
    Key                          key,
    ConcurrentMap<Object, Entry> entryCache,
    boolean                      checkModified
    )
  {
    DerivationKey derivationKey = _getDerivationKey(context, document);
    Entry entry = _getEntry(entryCache, derivationKey, checkModified);
    if (entry != null)
    {
      // If we've got a compatible entry, cache it
      cache.put(key, entry);
    }
    return entry;
  }

  /**
   * Returns the derivation key that would be used for entries
   * based on the provided context
   */
  private DerivationKey _getDerivationKey(
    StyleContext context,
    StyleSheetDocument document
    )
  {
    // Entries with the same style sheet derivation are compatible.
    // Get the style sheet derivation list.
    Iterator<StyleSheetNode> e = document.getStyleSheets(context);

    StyleSheetNode[] styleSheets;    
    
    if (e.hasNext())
    {
      styleSheets = CollectionUtils.toArray(e, StyleSheetNode.class);
    }
    else
    {
      styleSheets = _EMPTY_STYLE_SHEET_NODE_ARRAY;
    }

    // Create a key out of the style sheet derivation list
    return new DerivationKey(context, styleSheets);
  }

  /**
   * Returns the StyleSheetDocument, parsing the source file if necessary.
   * This does not use the StyleContext
   */
  private StyleSheetDocument _getStyleSheetDocument(StyleContext context)
  {
    StyleSheetDocument document = _document;

    // If we have a StyleSheetDocument already, just return it.
    if (document != null)
      return document;

    // Otherwise, we create the StyleSheetDocument now
    // Note, it does not use the StyleContext. This is the StyleSheetDocument
    // for the entire skin document, so it includes all the specific rules
    // like @agent ie and @agent gecko, etc. It's later that we output
    // the css based on the StyleContext.
    document = createStyleSheetDocument(context);

    // If we weren't able to create the StyleSheetDocument,
    // use a non-null placeholder
    if (document == null)
      document = _EMPTY_DOCUMENT;

    // Save the document
    if (_document == null)
      _document = document;

    // Re-initialize our Array of namespace prefixes that are in the selectors
    // Re-initialize our Map of short style class names
    _namespacePrefixes = _getNamespacePrefixes(context, _document);
    _shortStyleClassMap = _getShortStyleClassMap(_document, _namespacePrefixes);

    return document;
  }

  /**
   * Returns an array of fully resolved StyleNodes for the
   * specified StyleContext  and StyleSheetDocument.
   * This will be those StyleNodes that match the locale, direction, browser, etc -- the
   * info that is in the StyleContext.
   */
  private StyleNode[] _getStyleContextResolvedStyles(
    StyleContext context,
    StyleSheetDocument document
    )
  {
    Iterator<StyleNode> e = document.getStyles(context);
    if ((e == null) || !e.hasNext())
    {
      if (_LOG.isWarning())
        _LOG.warning("NO_STYLES_FOUND_CONTEXT", context);
      return null;
    }

    List<StyleNode> v = new ArrayList<StyleNode>();
    while (e.hasNext())
      v.add(e.next());

    return v.toArray(new StyleNode[v.size()]);
  }

  /**
   * Returns a Map of skin property names to values for the specified
   * styleSheetNodes that have been filtered from the StyleContext and StyleSheetDocument.
   */
  private ConcurrentMap<Object, Object> _getStyleContextResolvedSkinProperties(
    StyleNode[] styleNodes
    )
  {
    // Use the resolved StyleNode[] to get the skinProperties from them.
    ConcurrentMap<Object, Object> skinPropertiesMap = new ConcurrentHashMap<Object, Object>();
    for (StyleNode styleNode : styleNodes)
    {
      Collection<PropertyNode> skinPropertyNodes = styleNode.getSkinProperties();
      for (PropertyNode node : skinPropertyNodes)
      {
        // create SkinProperty key and SkinProperty value
        String selectorName = styleNode.getSelector();
        if (selectorName == null)
        {
          selectorName = "." + styleNode.getName() + ":alias";
        }
        String name = node.getName();

        // Store the property selector + property Name as the Skin Property Key.
        // e.g., use af|breadCrumbs-tr-show-last-item
        StringBuilder keyBuilder = new StringBuilder(selectorName.length() + name.length());
        keyBuilder.append(selectorName);
        keyBuilder.append(name);
        String key = keyBuilder.toString();

        // look up in map to get conversion
        Class<?> type = SkinProperties.PROPERTY_CLASS_TYPE_MAP.get(key);
        Object propValueObj = null;
        String value = node.getValue();
        if (type != null)
        {
          try
          {
            // coerce the value to the type
            propValueObj = Coercions.coerce(null, value, type);
          }
          catch (IllegalArgumentException ex)
          {
            if (_LOG.isWarning())
              _LOG.warning(ex);
          }
        }
        
        skinPropertiesMap.put(key, (propValueObj != null ? propValueObj : value));

      }
    }
    return skinPropertiesMap;
    
  }

  /**
   * Returns a Map of icon names to Icons for the specified
   * styleSheetNodes that have been filtered from the StyleContext and StyleSheetDocument
   * and everything merged together.
   */
  private ConcurrentMap<String, Icon> _getStyleContextResolvedIcons(
    StyleContext       context,
    StyleSheetDocument document
    )
  {

    Iterator<IconNode> iconNodeIterator = document.getIcons(context);
    ConcurrentMap<String, Icon> iconMap = new ConcurrentHashMap<String, Icon>();
    while (iconNodeIterator.hasNext())
    {
      IconNode iconNode = iconNodeIterator.next();
      iconMap.put(iconNode.getIconName(), iconNode.getIcon());
    }

    return iconMap;
  }

  /**
   * Generates the CSS files for the specified context and styles.
   * @return the names of the generated CSS files.
   */
  private List<String> _createStyleSheetFiles(
    StyleContext        context,
    StyleSheetDocument  document,
    StyleNode[]         styles,
    Map<String, String> shortStyleClassMap,
    String[]            namespacePrefixes,
    boolean             checkModified,
    boolean             isDirty)
  {

    // Get the current files
    List<File> outputFiles = _getOutputFiles(context, document);

    // If at least one output file exists, check the last modified time.
    if (!outputFiles.isEmpty())
    {
      // If the skin is marked dirty, we regenerate the css even if the document's timestamp has not
      // changed.
      if (checkModified || isDirty)
      {
        if (!isDirty && (checkModified && !_checkSourceModified(document, outputFiles.get(0))))
        {
          return _getFileNames(outputFiles);
        }
        // If the output file is older than the source file, we
        // need to regenerate the output file.  But first we
        // need to delete the old output file before we attempt to
        // create a new version.
        _deleteAll(outputFiles);
      }
      else
      {
        return _getFileNames(outputFiles);
      }
    }

    // Make sure the output directory exists in case it's been
    // blown away since the creation of the cache
    File outputDir = new File(_targetPath);
    if (!outputDir.exists())
      outputDir.mkdirs();

    // Write out the style sheet
    // First figure out whether or not we need to compress the style classes.
    // We don't compress the style classes if the content compression flag is disabled or
    // if the skin is a portlet skin.
    RenderingContext arc = RenderingContext.getCurrentInstance();
    Skin skin = arc.getSkin();
    boolean compressStyles = _isCompressStyles(context);

    StyleWriterFactoryImpl writerFactory = new StyleWriterFactoryImpl(_targetPath,
      getTargetStyleSheetName(context, document));
    CSSGenerationUtils.writeCSS(context,
                                skin.getStyleSheetName(),
                                styles,
                                writerFactory,
                                compressStyles,
                                shortStyleClassMap,
                                namespacePrefixes,
                                _STYLE_KEY_MAP
                                );

    writerFactory.close();

    // Return the name of the new style sheet
    return _getFileNames(writerFactory.getFiles());
  }

  private File _getOutputFile(String name, int number)
  {
    assert number >= 1;
    if (number == 1)
    {
      return new File(_targetPath, name);
    }
    int index = name.lastIndexOf(".");
    if (index < 0)
    {
      return new File(_targetPath, name + number);
    }
    else
    {
      // file name + number + file extension
      return new File(_targetPath,
        new StringBuilder(name.length() + 2).append(name.substring(0, index)).append(number)
          .append(name.substring(index)).toString());
    }
  }

  private void _deleteAll(Iterable<File> files)
  {
    for (File file : files)
    {
      if (file.exists())
      {
        boolean success = file.delete();
        // add warning if success is false, but continue on.
        // I've seen the delete fail when we try to delete right after the file was created -
        // like if the skin css file is modified and the page refreshed immediately after the
        // app was initially run.
        if (!success && _LOG.isInfo())
        {
          _LOG.info("COULD_NOT_DELETE_FILE", file.getName());
        }
      }
    }
  }

  /**
   * First figure out whether or not we need to compress the style classes.
   * We don't compress the style classes if the content compression flag is disabled or
   * if the skin is a portlet skin.
   */
  private boolean _isCompressStyles(StyleContext sContext)
  {
    // if we are not disabling style compression, then we are compressing the styles
    return !(sContext.isDisableStyleCompression());

  }

  private List<String> _getFileNames(List<File> files)
  {
    List<String> names = new ArrayList<String>(files.size());
    for (File file : files)
    {
      names.add(file.getName());
    }
    return Collections.unmodifiableList(names);
  }

  /**
   * Returns the name of the output files that have been created for the given context and
   * document. If there are no files found, and empty list will be returned.
   * @return The list of list that currently exist, or null if none found
   */
  private List<File> _getOutputFiles(
    StyleContext context,
    StyleSheetDocument document
    )
  {
    // use a linked list as we only iterate and linked lists are faster for iteration & appending
    // than array lists.
    List<File> files = new LinkedList<File>();
    String name = getTargetStyleSheetName(context, document);
    for (int i = 1; true; ++i)
    {
      // we don't know in advance if there are any files, and if there are, how many of them
      // there are. Therefore, we keep incrementing the counter until the file doesn't exist and
      // at that point we know we have all of them
      File f = _getOutputFile(name, i);
      if (f.exists())
      {
        files.add(f);
      }
      else
      {
        break;
      }
    }

    return files;
  }

  /**
   * Returns the PrintWriter to use for the specified file
   */
  private PrintWriter _getWriter(File file)
  {
    PrintWriter out = null;

    try
    {
      File parentFile = file.getParentFile();
      if (parentFile != null)
        parentFile.mkdirs();

      // This throws a FileNotFoundException if it wasn't successfully deleted earlier, most likely
      // due to creating, then deleting too soon after.
      // Since the file has the hashcode in the name, it's not bad that it doesn't rewrite it.
      FileOutputStream fos = new FileOutputStream(file);
      OutputStreamWriter writer = null;

      // Use UTF8 encoding for output, in case font names have non-ascii
      // characters.
      try
      {
        writer = new OutputStreamWriter(fos, _UTF8_ENCODING);
      }
      catch (UnsupportedEncodingException e)
      {
        // UTF-8 should always be supported!
        assert false;

        // Just use default encoding instead
        writer = new OutputStreamWriter(fos);
      }

      out = new PrintWriter(new BufferedWriter(writer));
    }
    catch (IOException e)
    {
      // This might happen if we couldn't delete the css file that was already there, so we 
      // are unable to recreate it.
      if (_LOG.isInfo())
        _LOG.info("IOEXCEPTION_OPENNING_FILE", file);
    }

    return out;
  }

  /**
   * Checks to see whether the source file has been modified
   * since the specified output file was generated.  If so,
   * we need to regenerate the output file.
   */
  private boolean _checkSourceModified(
    StyleSheetDocument document,
    File               outputFile
    )
  {
    assert (outputFile != null);

    return (document.getDocumentTimestamp() > outputFile.lastModified());
  }

  /**
   * Create an array of all the namespace prefixes in the xss/css file. E.g., "af|" or "tr|"
   */
  private static String[] _getNamespacePrefixes(
    StyleContext       context,
    StyleSheetDocument document)
  {

    assert (document != null);
    Iterator<StyleSheetNode> styleSheets = document.getStyleSheets(context);
    assert (styleSheets != null);
    Set<String> namespacePrefixesSet = new HashSet<String>();
    while (styleSheets.hasNext())
    {
      StyleSheetNode styleSheet = styleSheets.next();
      Iterable<StyleNode> styles = styleSheet.getStyles();
      assert (styles != null);
      for (StyleNode style : styles)
      {
        String selector = style.getSelector();

        if (selector != null)
        {
          CSSGenerationUtils.getNamespacePrefixes(namespacePrefixesSet, selector);
        }
      }
    }
    return namespacePrefixesSet.toArray(_EMPTY_STRING_ARRAY);
  }

  /**
   * Create the map of full style classes to short style classes
   * Do not shorten styleclasses that start with SkinSelectors.STATE_PREFIX
   */
  private static Map<String, String> _getShortStyleClassMap(
    StyleSheetDocument document,
    String[]           namespacePrefixes)
  {
    // Use a HashMap to avoid unnecessary synchronization of Hashtable
    Map<String, String> map = new HashMap<String, String>();

    assert (document != null);
    // get all the styleSheets to create the shortened style class map
    // if we only got the ones based on the StyleContext, then we'd have
    // to create a shortened map for each StyleContext we receive and cache it.
    // it's more straightforward, faster, and requires less memory to get
    // all the styleSheets and create the shortened style class map. There might
    // be some styles in the map that have no properties in another StyleContext,
    // but that's ok. It doesn't hurt.
    Iterator<StyleSheetNode> styleSheets = document.getStyleSheets();
    assert (styleSheets != null);

    Set<String> emptySelectors = new HashSet<String>();
    Set<String> nonEmptySelectors = new HashSet<String>(512);

    while (styleSheets.hasNext())
    {
      StyleSheetNode styleSheet = styleSheets.next();
      Iterable<StyleNode> styles = styleSheet.getStyles();
      assert (styles != null);
      for (StyleNode style : styles)
      {
        String selector = style.getSelector();

        if (selector != null)
        {
          // If we've got a single style class selector, add it
          // to the map. Otherwise, we need to search the selector
          // for style classes.
          if (CSSGenerationUtils.isSingleStyleClassSelector(selector))
          {
            String styleClass = selector.substring(1);
            _putStyleClassInShortMap(styleClass, map);
            // don't shorten styleclasses that are states since they are likely to be added
            // and removed on the client.
            if (styleClass != null && !styleClass.startsWith(SkinSelectors.STATE_PREFIX))
              if (!map.containsKey(styleClass))
                map.put(styleClass, _getShortStyleClass(map.size()));

            if (style.isEmpty())
              emptySelectors.add(styleClass);
            else
              nonEmptySelectors.add(styleClass);
          }
          else
          {
            Iterator<String> styleClasses =
              CSSGenerationUtils.getStyleClasses(selector);

            if (styleClasses != null)
            {
              while (styleClasses.hasNext())
              {
                String styleClass = styleClasses.next();
                _putStyleClassInShortMap(styleClass, map);
                // Don't remove any styleclass that is referred to
                nonEmptySelectors.add(styleClass);
              }
            }

            // now search for the selectors that have namespaces and add those to the map
            int length = namespacePrefixes.length;

            for (int i=0; i < length; i++)
            {
              String nsPrefix = namespacePrefixes[i];
              Iterator<String> afSelectors =
                CSSGenerationUtils.getNamespacedSelectors(selector,
                                                          nsPrefix,
                                                          _STYLE_KEY_MAP);
              if (afSelectors != null)
              {
                boolean isFirst = true;
                while (afSelectors.hasNext())
                {
                  String styleClass = afSelectors.next();
                  _putStyleClassInShortMap(styleClass, map);
                  if (isFirst && !afSelectors.hasNext() && style.isEmpty())
                  {
                    emptySelectors.add(styleClass);
                  }
                  else
                  {
                    nonEmptySelectors.add(styleClass);
                  }

                  isFirst = false;
                }
              }
            }
          }
        }
      }
    }

    emptySelectors.removeAll(nonEmptySelectors);

    // Replace all empty keys with an empty string as the selector. 
    // The reason is to keep empty keys from getting written to the HTML.
    // Empty keys get filtered out of the CSS file when we write to that.
    for (String emptyKey : emptySelectors)
      map.put(emptyKey, CoreRenderingContext.EMPTY_STYLE_CLASS);

    // We actually need a Map, since this object is exposed
    // via public APIs.  Also, we need the Map to be immutable,
    // or else we would need to create a new copy of the Map
    // each time it is requested.
    return Collections.unmodifiableMap(map);
  }

  /**
   * Method to put styleclasses in the shortened map.
   * We don't put 'state' styleclasses in the shortened map. Those are styleclasses
   * that start with SkinSelectors.STATE_PREFIX. The reason is that those
   * are likely to be added and removed on the client as the state changes, and
   * we don't want to require the shortened map on the client.
   */
  private static void _putStyleClassInShortMap(String styleClass, Map map)
  {
    if (styleClass != null &&
        !styleClass.startsWith(SkinSelectors.STATE_PREFIX) &&
        !map.containsKey(styleClass))
    {
      map.put(styleClass, _getShortStyleClass(map.size()));
    }
  }

  /**
   * Helper method used by _getShortStyleClassMap().  Returns a new
   * short style class selector.  The count is the number of style
   * classes seen so far.
   */
  private static String _getShortStyleClass(int count)
  {
    // At the moment the short style class is just based on the nubmer
    // of style classes and not the style class itself.
    return _SHORT_CLASS_PREFIX + Integer.toString(count, Character.MAX_RADIX);
  }

  
  /**
   * Given a StyleNode object, which is an internal API that denotes a Style object
   * with additional information like includedSelectors, create a simple public
   * Style object which will be used in the SelectorStyleMap. When this method is called,
   * the StyleNode object is already resolved (included selectors have been merged in)
   * so that all the css properties are there.
   * @param styleNode
   * @param reusableStyleMap A Map<Style, Style>. This is 
   *  used so that we can reuse Style objects in StylesImpl if they have the same list of style property
   *  names and values.
   * @return A Style object created from the information in the styleNode. We reuse
   *  Style objects if the properties are the same.
   */
  public Style _convertStyleNodeToStyle(
    StyleNode          styleNode, 
    Map<Style, Style>  reusableStyleMap)
  {
    // Add in the properties for the style; PropertyNode interns the 'name' and the most common 'value's.
    Collection<PropertyNode> propertyNodeList = styleNode.getProperties();
    Map<String, String> styleProperties = new ArrayMap<String, String>(propertyNodeList.size());

    for (PropertyNode property : propertyNodeList)
    {
      String name = property.getName();
      String value = property.getValue();
      if (name != null && value != null)
      {
        styleProperties.put(name, value);
      }
    }

    // To save memory, we reuse Style objects for each FileSystemStyleCache instance.
    Style style = new UnmodifiableStyle(styleProperties);
    Style cachedStyle = reusableStyleMap.get(style);
    if (cachedStyle == null)
    {
      reusableStyleMap.put(style, style);
      return style;         
    }
    else
    {
      return cachedStyle;
    }
  }
  
  /**
   * Key class used for hashing style sheet URIs. This key for the Entry
   * cache is dependent on the agent, locale, direction, etc.
   */
  private static class Key
  {
    public Key(StyleContext context)
    {
      TrinidadAgent agent = context.getAgent();
      LocaleContext localeContext = context.getLocaleContext();
      AccessibilityProfile accProfile = context.getAccessibilityProfile();

      _init(
       localeContext.getTranslationLocale(),
       LocaleUtils.getReadingDirection(localeContext),
       agent.getAgentApplication(),
       agent.getAgentVersion(),
       agent.getAgentOS(),
       !context.isDisableStyleCompression(),
       accProfile,
       context.isPortletMode(),
       context.isRequestSecure());
    }

    @Override
    public int hashCode()
    {
      if(_noHash)
      {
        //don't worry about synchronizing this
        _hashCode  =   (_direction)                ^
                       (_browser.ordinal() << 2)   ^
                       (_platform << 8)            ^
                       (_short ? 1 : 0)            ^
                       ((_portlet ? 1:0) << 1)     ^
                       ((_secureRequest ? 1: 0) << 3);

        if (_locale != null)     _hashCode ^= _locale.hashCode();
        if (_accProfile != null) _hashCode ^= _accProfile.hashCode();
        if (_version != null)    _hashCode ^= (_version.hashCode());

        _noHash = false;
      }
      return _hashCode;
    }

    @Override
    public boolean equals(Object o)
    {
      if (this == o)
        return true;

      //Improved performance of this check
      if ((o.hashCode() == hashCode()) &&  (o instanceof Key))
      {
        Key key = (Key)o;
        
        // Check the easy stuff first
        if  ((_short == key._short)             &&
             (_portlet == key._portlet)         &&
             (_direction == key._direction)     &&
             (_browser == key._browser)         &&
             (_platform == key._platform)       &&
             (_secureRequest == key._secureRequest))
        {
          // now check the optional objects
          if ((_version == null) || _version.equals(key._version))
            if ((_locale == null) || _locale.equals(key._locale))
              return ((_accProfile == null) || _accProfile.equals(key._accProfile));
        }
      }

      return false;
    }

    private void _init(
      Locale locale,
      int direction,
      TrinidadAgent.Application browser,
      String version,
      int platform,
      boolean useShort,
      AccessibilityProfile accessibilityProfile,
      boolean portlet,
      boolean secure
      )
    {
      // Make sure direction is non-null
      _locale = (locale == null) ? Locale.getDefault() : locale;

      // Make sure direction is not default
      _direction =
        (direction == LocaleUtils.DIRECTION_DEFAULT) ?
          LocaleUtils.getReadingDirectionForLocale(_locale) :
          direction;

      _browser = browser;
      _version = version;
      _platform = platform;
      _short = useShort;
      _accProfile = accessibilityProfile;
      _portlet     = portlet;
      _secureRequest = secure;
    }

    //is immutable, we should cache this, will make things faster in the long run
    private boolean        _noHash = true;
    private int            _hashCode;

    private Locale         _locale;
    private int            _direction;
    private TrinidadAgent.Application _browser;
    private String         _version;
    private int            _platform;
    private boolean        _short;  // Do we use short style classes?
    private AccessibilityProfile _accProfile;
    private boolean        _portlet; //kind of a hack but tells whether this was created in portal mode
    private boolean        _secureRequest;
  }

  /**
   * Cache entry class.
   * The FileSystemStyleCache instance contains Entry objects: one Entry
   * object per unique generated CSS style sheet (e.g., gecko and ie will most
   * likely have different generated CSS style sheets).
   */
  private static class Entry
  {
    public final List<String> uris;
    public final Styles styles;
    public final ConcurrentMap<String, Icon> icons;
    public final ConcurrentMap<Object, Object> skinProperties;

    public Entry(
      List<String> uris,
      Styles styles,
      ConcurrentMap<String, Icon> icons,
      ConcurrentMap<Object, Object> skinProperties)
    {
      this.uris = uris;
      this.styles = styles;
      this.icons = icons;
      this.skinProperties = skinProperties;
    }
  }

  /**
   * A key object which is used to hash Entrys in the entry cache.  This key for the Entry
   * cache is the style sheet derivation list - that is a list of StyleSheetNodes, sorted
   * by specficity. It's not dependent on the agent, locale, direction like the Key object is.
   */
  private static class DerivationKey
  {
    public DerivationKey(StyleContext context, StyleSheetNode[] styleSheets)
    {
      _styleSheets = new StyleSheetNode[styleSheets.length];
      System.arraycopy(styleSheets, 0, _styleSheets, 0, styleSheets.length);
      _short = true;
      _portlet = context.isPortletMode();
      _secureRequest = context.isRequestSecure();
    }

    @Override
    public boolean equals(Object o)
    {
      if (o == this)
        return true;

      if ((o.hashCode() == hashCode()) && (o instanceof DerivationKey))
      {
        DerivationKey key = (DerivationKey)o;

        if ((_short != key._short) ||
            (_portlet != key._portlet) ||
            (_secureRequest != key._secureRequest) ||
            (_styleSheets.length != key._styleSheets.length))
          return false;

        // Test each StyleSheetNode for equality.  We start
        // at the end of the list, as the last style sheet
        // is more likely to be different.  (The first entry
        // is probably the common base style sheet.)
        for (int i = _styleSheets.length - 1; i >= 0; i--)
        {
          if (!_styleSheets[i].equals(key._styleSheets[i]))
            return false;
        }

        return true;
      }

      return false;
    }

    @Override
    public int hashCode()
    {
      if(_noHash)
      {
        _hashCode = Arrays.hashCode(_styleSheets) ^
                    (_short ? 1 : 0)              ^
                    (_portlet ? 1 : 0)            ^
                    ((_secureRequest ? 1: 0) << 3);
        _noHash = false;
      }

      return _hashCode;
    }

    //This object is immutable. So we can cache the hashcode to make it faster
    private boolean _noHash = false;
    private int     _hashCode;

    private StyleSheetNode[] _styleSheets;
    private boolean _portlet;
    private boolean _short;   // Do we use short style classes?
    private boolean _secureRequest;
  }

  /**
   * A Styles implementation that adds the resolved (merged together based on the StyleContext)
   * StyleNodes to a Map. Only the style selectors and not the aliased (aka named) styles
   * are added to this map.
   */
  private static final class StylesImpl extends Styles
  {
    /**
     * This constructor takes an array of StyleNode Objects where each StyleNode has
     * already been resolved based on the StyleContext. Therefore there is no
     * more merging that needs to be done, and the 'included' properties on
     * StyleNode are all null. This way we do not have to resolve the
     * styles based on the StyleContext when someone calls getStyles,
     * etc.
     * @param namespacePrefixArray an array of namespace prefixes that are used in the custom css
     * skinning selectors, like "af" in af|inputText.
     * @param afSelectorMap a map from one selector to another (like af|panelHeader::link maps to
     * af|panelHeader A
     * @param shortStyleClassMap a map from the  non-compressed styleclass
     * to a compressed styleclass.
     * @param resolvedSelectorStyleMap
     */
    public StylesImpl(
        String[]            namespacePrefixArray,
        Map<String, String> afSelectorMap,
        Map<String, String> shortStyleClassMap,
        boolean             compress,
        Map<Selector, Style> resolvedSelectorStyleMap
      )
    {
      // store these local variables to be used in getNativeSelectorString
      _namespacePrefixArray = namespacePrefixArray;
      _afSelectorMap = afSelectorMap;
      _shortStyleClassMap = shortStyleClassMap;
      _compress = compress;

      if (resolvedSelectorStyleMap != null)
        _unmodifiableResolvedSelectorStyleMap =
          Collections.unmodifiableMap(resolvedSelectorStyleMap);
      else
        _unmodifiableResolvedSelectorStyleMap = Collections.emptyMap();

    }

    /**
     * Returns a Map containing the selector String as the key and the Style Object
     * (contains all the css property names/values) as the value. This Map can then be used
     * to get all the selector keys, or to get the Style Object for a Selector, or to
     * get all the selectors that match some criteria, like they contain a certain simple selector.
     * This map does not contain 'alias' (aka named) selectors. It only contains selectors
     * that would be in the generated css file.
     *
     * @return unmodifiableMap of the resolved Selector -> Style map.
     */
    public Map<Selector, Style> getSelectorStyleMap()
    {
      return _unmodifiableResolvedSelectorStyleMap;
    }

    /**
     * Returns the Selector in String form, converted to a format that
     * is suitable to be written to the browse. This is the css-2 format which doesn't have
     * namespaces and our psuedo-elements.
     * @param selector Selector
     * @return String the Selector in a String form that is suitable to be
     * written to the client.
     */
    public String getNativeSelectorString(Selector selector)
    {
      // convert the selector to a valid css2 selector like the ones we write
      // to the generated css file.
      if (selector == null)
        throw new IllegalArgumentException("selector cannot be null");
      // run the selector through a conversion map so the selector is closer to
      // what we write out to the css. e.g., af|inputText:error::content becomes
      // af|inputText.p_AFError af|inputText::content. This way we don't have to
      // do this when we write the css inline. We have the information now.
      String mappedSelector =  CSSGenerationUtils.getMappedSelector(
        _afSelectorMap, _namespacePrefixArray, selector.toString());
      // run through the compressed map if it is compressed.
      if (_compress)
        mappedSelector  =
          CSSGenerationUtils.getShortSelector(_shortStyleClassMap,
                                              _namespacePrefixArray,
                                              mappedSelector);

      return CSSGenerationUtils.getValidFullNameSelector(
        mappedSelector, _namespacePrefixArray);
    }



    


    private final Map<Selector, Style> _unmodifiableResolvedSelectorStyleMap;
    private final Map<String, String>  _afSelectorMap;
    private final String[]             _namespacePrefixArray;
    private final Map<String, String>  _shortStyleClassMap;
    private final boolean              _compress;
  }

  private class StyleWriterFactoryImpl
    implements StyleWriterFactory
  {
    private final String _outputDirectory;
    private final String _baseFilename;
    private PrintWriter _out;
    private List<File> _files = new LinkedList<File>();

    StyleWriterFactoryImpl(String outputDirectory, String baseName)
    {
      _outputDirectory = outputDirectory;
      _baseFilename = baseName;
    }

    List<File> getFiles()
    {
      return _files;
    }

    public PrintWriter createWriter()
    {
      if (_out != null)
      {
        _out.close();
      }

      File outputFile = _getOutputFile(_baseFilename, _files.size() + 1);
      // We never want to do anything other than read it or delete it:
      outputFile.setReadOnly();

      _files.add(outputFile);
      _out = _getWriter(outputFile);

      return _out;
    }

    void close()
    {
      if (_out != null)
      {
        _out.close();
        _out = null;
      }
    }
  }
  
  private final String _targetPath; // The location of the cache

  /** The parsed StyleSheetDocument */
  private StyleSheetDocument _document;
  
  /** Since each FileSystemStyleCache$Entry object holds a FileSystemStyleCache$StylesImpl object
   *  which holds on to many Style objects, it reduces the memory consumption by about half
   *  if we reuse Style objects per FileSystemStyleCache instance rather than per
   *  FileSystemStyleCache$Entry instance. The is the map we use to store unique Style objects. */
  private ConcurrentMap<Style, Style> _reusableStyleMap;
  
  /** Use this to store Selector objects so that they can be reused in all the FileSystemStyleCache$StylesImpl
   * objects. A generated css file can contain 4533 selectors at 16 bytes each. The Selectors will largely
   * be the same between FileSystemStyleCache$StylesImpl instances, so they should be shared. */
  private ConcurrentMap<Selector, Selector> _reusableSelectorMap;
  
  /** The cache of style sheet URIs */
  private ConcurrentMap<Key, Entry> _cache;

  /**
   * We cache Entry objects, hashed by DerivationKey (ie.
   * hashed based on the StyleSheetNode derivation list).
   */
  private ConcurrentMap<Object, Entry> _entryCache;

  /** Map which maps from full style class names to our compressed names. */
  private Map<String, String> _shortStyleClassMap;
  private String[]            _namespacePrefixes;

  // Constants

  // Separator for variants in file names
  private static final char _NAME_SEPARATOR = '-';
  private static final String _COMPRESSED = "cmp";
  private static final String _PORTLET = "prtl";
  private static final String _SECURE = "s";

  /** Extension for CSS files */
  private static final String _CSS_EXTENSION = ".css";

  /** Java name for UTF8 encoding */
  private static final String _UTF8_ENCODING = "UTF8";

  /** Stub StyleSheetDocument instance */
  private static final StyleSheetDocument _EMPTY_DOCUMENT =
    new StyleSheetDocument(null, null, StyleSheetDocument.UNKNOWN_TIMESTAMP);

  /** Prefix to use for short style classes */
  private static final String _SHORT_CLASS_PREFIX = "x";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FileSystemStyleCache.class);


  /**
   * use this map to map from the public style selector names to
   * our internal style selector names. The public style selector
   * names do not contain html, whereas our internal style selector
   * names may. We write out the shortened version of the mapped
   * selector names to the css file.
   * @todo Need to find a better spot for this, like the skin?
   */
  private static final Map<String, String> _STYLE_KEY_MAP;

  static
  {
    _STYLE_KEY_MAP =  new HashMap<String, String>();
    // we don't use a styleClass on tr:body. Instead we use the html element
    // BODY to style it. This makes it easier for users to use an external
    // stylesheet and not have to know our styleClass names.
    _STYLE_KEY_MAP.put("af|body", "BODY");

    _STYLE_KEY_MAP.put("af|panelHeader::level-one",
                 "H1.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-two",
                 "H2.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-three",
                 "H3.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-four",
                 "H4.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-five",
                 "H5.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-six",
                 "H6.af|panelHeader");

    // showDetailHeader
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-one",
                 "H1.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-two",
                 "H2.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-three",
                 "H3.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-four",
                 "H4.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-five",
                 "H5.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-six",
                 "H6.af|showDetailHeader");

    _STYLE_KEY_MAP.put("af|menuTabs::selected-link",
                 "af|menuTabs::selected A");
    _STYLE_KEY_MAP.put("af|menuTabs::enabled-link",
                 "af|menuTabs::enabled A");

    _STYLE_KEY_MAP.put("af|menuBar::enabled-link",
                 "af|menuBar::enabled A");
    _STYLE_KEY_MAP.put("af|menuBar::selected-link",
                 "af|menuBar::selected A");

    _STYLE_KEY_MAP.put("OraLinkEnabledLink",
                 "OraLinkEnabled A:link");
    _STYLE_KEY_MAP.put("OraLinkSelectedLink",
                 "OraLinkSelected A:link");
    _STYLE_KEY_MAP.put("OraLinkEnabledActive",
                 "OraLinkEnabled A:active");
    _STYLE_KEY_MAP.put("OraLinkSelectedActive",
                 "OraLinkSelected A:active");
    _STYLE_KEY_MAP.put("OraLinkEnabledVisited",
                 "OraLinkEnabled A:visited");
    _STYLE_KEY_MAP.put("OraLinkSelectedVisited",
                 "OraLinkSelected A:visited");

    _STYLE_KEY_MAP.put("af|panelPage::about-link",
                 "af|panelPage::about A");
    _STYLE_KEY_MAP.put("af|panelPage::copyright-link",
                 "af|panelPage::copyright A");
    _STYLE_KEY_MAP.put("af|panelPage::privacy-link",
                 "af|panelPage::privacy A");

    _STYLE_KEY_MAP.put("af|panelList::link",
                 "af|panelList A");
    _STYLE_KEY_MAP.put("af|panelList::unordered-list",
                 "af|panelList UL");

    _STYLE_KEY_MAP.put("af|inputDate::nav-link",
                 "af|inputDate::nav A");
    _STYLE_KEY_MAP.put("af|inputDate::content-link",
                 "af|inputDate::content A");
    _STYLE_KEY_MAP.put("af|inputDate::disabled-link",
                 "af|inputDate::disabled A");
    _STYLE_KEY_MAP.put("af|chooseDate::nav-link",
                 "af|chooseDate::nav A");
    _STYLE_KEY_MAP.put("af|chooseDate::content-link",
                 "af|chooseDate::content A");

    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_SHOWMANYACCORDION_TITLE_LINK_STYLE_CLASS,
        "A.af|showManyAccordion::title-link");
    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_SHOWMANYACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS,
        "A.af|showManyAccordion::title-disabled-link");

    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_SHOWONEACCORDION_TITLE_LINK_STYLE_CLASS,
        "A.af|showOneAccordion::title-link");
    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_SHOWONEACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS,
        "A.af|showOneAccordion::title-disabled-link");

    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_PANELACCORDION_TITLE_LINK_STYLE_CLASS,
        "A.af|panelAccordion::title-link");
    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_PANELACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS,
        "A.af|panelAccordion::title-disabled-link");

    _STYLE_KEY_MAP.put("af|panelTabbed::tab-link",
        "af|panelTabbed::tab A");
    _STYLE_KEY_MAP.put("af|panelTabbed::tab-selected-link",
        "af|panelTabbed::tab-selected A");

  }

  private static final StyleSheetNode[] _EMPTY_STYLE_SHEET_NODE_ARRAY = new StyleSheetNode[0];
  private static final String[] _EMPTY_STRING_ARRAY = new String[0];
}
