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

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinAddition;
import org.apache.myfaces.trinidad.skin.SkinVersion;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.skin.icon.NullIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;

/**
 * This is a Skin decorator which is used to store request-specific
 * skin state.  For example, the set of icons to use varies from
 * request to request, eg. based on the browser/platform.  We cannot
 * store such request-specific state on our shared Skin instances.
 * Instead, SkinFactoryImpl wraps shared Skin instances in RequestSkinWrappers
 * so that request-specific state can be stored locally in the wrapper.
 *
 * Currently, the request-specific state for a skin is the icon map and the skin property map.
 * They are retrieved from the StyleProvider one time per request and stored here.
 * @see org.apache.myfaces.trinidadinternal.style.cache.FileSystemStyleCache
 */
public class RequestSkinWrapper extends Skin implements DocumentProviderSkin
{
  // An alternate implementation strategy would be to enhance SkinImpl.getIcon()
  // to serve up the request/context-specific Icons directly, rather than do so
  // via a wrapper Skin class.  There are two issues with the SkinImpl-based
  // solution:
  //
  // 1. SkinImpl.getIcon() needs access to the StyleContext in order to retrieve
  // the request/context-specific Icon map.  However, Skin.getIcon() does not
  // get take a StyleContext.  We could change the Skin.getIcon() API to take
  // a StyleContext, but since this is a public API we would prefer a solution
  // which does not involve an API change.  Alternatively, SkinImpl.getIcon()
  // could get at the StyleContext by retrieving the RenderingContext, casting
  // to CoreRenderingContext, and calling CoreRenderingContext.getStyleContext().
  // However, that adds overhead to the SkinImpl.getIcon() implementation
  // (mainly just a ThreadLocal look up, which isn't the end of the world,
  // but nonetheless would prefer to avoid if possible).
  //
  // 2. Calls to registerIcon() need to behave differently depending on whether
  // the Icon is being registered at Skin initialization time, or at
  // request-time.
  //
  // At Skin initialization time, we simply want to register the Icon on the
  // Skin instance itself, since such Icons are global to the Skin.  However,
  // we also use registerIcon() to register request-specific icon - for example,
  // see CoreRenderingContext.getIcon()'s handling of rtl icons.  We cannot
  // register these request-specific Icons on the Skin as the Skin is reused
  // across requests.  Instead we want to register request-specific Icons on
  // the request-specific Icon map.
  //
  // In order to deal with #2, we could have SkinImpl behave differently
  // depending on whether it is at initialization time or request time.  However,
  // this is awkward and more cleanly solved via the request-specific wrapper
  // approach.

  public RequestSkinWrapper(Skin wrappedSkin)
  {
    _skin = wrappedSkin;
  }

  /**
   * Returns the Skin that is wrapped by this request-specific
   * wrapper skin.
   */
  public Skin getWrappedSkin()
  {
    return _skin;
  }

  @Override
  public String getId()
  {
    return _skin.getId();
  }

  @Override
  public String getFamily()
  {
    return _skin.getFamily();
  }

  @Override
  public SkinVersion getVersion()
  {
    return _skin.getVersion();
  }
  
  @Override
  public String getRenderKitId()
  {
    return _skin.getRenderKitId();
  }

  @Override
  public String getStyleSheetDocumentId(RenderingContext arc)
  {
    return _skin.getStyleSheetDocumentId(arc);
  }

  @Override
  public Map<String, String> getStyleClassMap(RenderingContext arc)
  {
    return _skin.getStyleClassMap(arc);
  }

  @Override
  public String getStyleSheetName()
  {
    return _skin.getStyleSheetName();
  }

  @Override
  public String getTranslatedString(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException
  {
    return _skin.getTranslatedString(lContext, key);
  }

  @Override
  public Object getTranslatedValue(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException
  {
    return _skin.getTranslatedValue(lContext, key);
  }

  @Override
  public Icon getIcon(String  iconName)
  {
    return this.getIcon(iconName, true);
  }

  @Override
  public Icon getIcon(
    String  iconName,
    boolean resolveIcon)
  {
    // We look for the icon in two places:
    //
    // 1. In the icon map provided by the StyleProvider.
    //    The StyleProvider serves up icons that are
    //    specific to this particular request, including
    //    agent-specific icons.
    // 2. In the wrapped Skin instance.  Any Icons that are
    //    manually registered via a call to registerIcon()
    //    will be stored in the wrapped skin.

    // Note: no synchronization needed since we are in a
    // a request-specific object.
    Map<String, Icon> icons = _getRequestIcons();
    assert(icons != null);

    Icon icon = icons.get(iconName);

    if (icon == null)
    {
      icon = _skin.getIcon(iconName, false);

      // Resolve ReferenceIcons if necessary
      if (resolveIcon)
      {
        if (icon instanceof ReferenceIcon)
          icon = SkinUtils.resolveReferenceIcon(this, (ReferenceIcon)icon);

        // We ended up having to do two lookups for icons
        // which are not available in the StyleProvider
        // icon map.  To avoid having to do two lookups
        // the next time this icon is requested, we promote
        // the icon up from the wrapped skin into the
        // StyleProvider icon map.  Note that we only cache
        // resolved icons in this way - we don't want to
        // pollute our cache with unresolved ReferenceIcons.
        registerIcon(iconName, icon);
      }
    }

    return (icon == _NULL_ICON) ? null : icon;
  }

  @Override
  public void registerIcon(
    String  iconName,
    Icon    icon)
  {
    // registerIcon() is called on a RequestSkinWrapper
    // in two cases:
    //
    // 1. CoreRenderingContext.getIcon() contains special handling
    // for icons when the reading direction is right-to-left.  If a
    // rtl variant of the icon is not available, CoreRenderingContext
    // will look up the non-directional version of the icon and register
    // that under the rtl name to avoid repeated lookups.
    //
    // 2. RequestSkinWrapper.getIcon() calls registerIcon() to register
    // icons which were not found in the StyleProvider's icon map.  This
    // also is done as a performance optimization - to repeatedly looking
    // up icons first in the StyleProvider icon map and then in the
    // wrapped skin.
    //
    // In both of these cases storing the missing icon in the StyleProvider
    // icon map is safe/appropriate, since lookups with the same set of
    // StyleContext properties would return the same results.  By storing
    // the result in the StyleProvider icon map, we short-circuit, avoiding
    // redundant work.

    Map<String, Icon> icons = _getRequestIcons();
    if (icons != _NULL_ICONS)
      icons.put(iconName, (icon == null) ? _NULL_ICON : icon);
  }

  @Override
  public Object getProperty(Object key)
  {
    // We look for the skin properties in two places:
    //
    // 1. In the skin property map provided by the StyleProvider (see _getRequestSkinProperties).
    //    The StyleProvider serves up skin properties that are
    //    specific to this particular request, including
    //    agent-specific skin properties.
    // 2. In the wrapped Skin instance.  Any skin properties that are
    //    manually registered via a call to setProperty(Object, Object)
    //    will be stored in the wrapped skin.

    // Note: no synchronization needed since we are in a
    // a request-specific object.
    Map<Object, Object> properties = _getRequestSkinProperties();
    assert(properties != null);
    Object propertyValue = properties.get(key);

    if (propertyValue == null)
    {
      propertyValue = _skin.getProperty(key);
    }

    return propertyValue;
  }

  @Override
  public void setProperty(
    Object key,
    Object value)
  {
    Map<Object, Object> properties = _getRequestSkinProperties();
    if (properties != _NULL_PROPERTIES)
      properties.put(key, value);
  }




  /**
  * @deprecated Use addSkinAddition instead
  * */
  @Override
  public void registerStyleSheet(
    String styleSheetName
    )
  {
    _skin.registerStyleSheet(styleSheetName);
  }

  @Override
  public void addSkinAddition (
    SkinAddition skinAddition
    )
  {
    _skin.addSkinAddition(skinAddition);
  }

  @Override
  public List<SkinAddition> getSkinAdditions ()
  {
    return _skin.getSkinAdditions();
  }

  /**
   * Implementation of DocumentProviderSkin.getStyleSheetDocument().
   */
  public StyleSheetDocument getStyleSheetDocument(StyleContext styleContext)
  {
    // If getStyleSheetDocument() is being called on us, this implies
    // that the wrapped Skin must also be a DocumentProviderSkin.
    assert(_skin instanceof DocumentProviderSkin);

    return ((DocumentProviderSkin)_skin).getStyleSheetDocument(styleContext);
  }
  
  @Override
  public boolean isDirty()
  {
    return _skin.isDirty();
  }
  @Override
  public void setDirty(boolean dirty)
  {
    _skin.setDirty(dirty);
  }

  // Returns request-specific map of icon names to Icons
  private Map<String, Icon> _getRequestIcons()
  {
    if (_icons == null)
    {
      // We get to the request-specific icons via the
      // StyleProvider.  We need a CoreRenderingContext
      // instance to get at the StyleProvider.  This
      // implementation assumes that the RenderingContext
      // is going to be an instanceof CoreRenderingContext.
      // This is a pretty good bet, since the only
      // RenderingContext provided by Trinidad is
      // CoreRenderingContext, and given the complexity of
      // the CoreRenderingContext implementation, it seems
      // unlikely that anyone would attempt to replace the
     // implementation.
      RenderingContext rc = RenderingContext.getCurrentInstance();
      assert(rc instanceof CoreRenderingContext);

      CoreRenderingContext crc = (CoreRenderingContext)rc;
      StyleContext styleContext = crc.getStyleContext();
      StyleProvider styleProvider = styleContext.getStyleProvider();
      _icons = styleProvider.getIcons(styleContext);

      // Under normal circumstances, the StyleProvider will return
      // a non-null, modifiable map.  If the skin/style subsystem
      // has failed to initialize, however, the map may be null.
      // Substitute an empty map so we don't need to check for null
      // later.
      if (_icons == null)
      {
        if (_LOG.isWarning())
          _LOG.warning("_icons is an emptyMap because the skin/style " +
            "subsystem has failed to initialize.");
        _icons = _NULL_ICONS;
      }
    }

    return _icons;
  }

  // Returns request-specific map of skin property key/values
  private Map<Object, Object> _getRequestSkinProperties()
  {
    if (_properties == null)
    {
      // We get to the request-specific properties via the
      // StyleProvider.  We need a CoreRenderingContext
      // instance to get at the StyleProvider.  This
      // implementation assumes that the RenderingContext
      // is going to be an instanceof CoreRenderingContext.
      // This is a pretty good bet, since the only
      // RenderingContext provided by Trinidad is
      // CoreRenderingContext, and given the complexity of
      // the CoreRenderingContext implementation, it seems
      // unlikely that anyone would attempt to replace the
     // implementation.
      RenderingContext rc = RenderingContext.getCurrentInstance();
      assert(rc instanceof CoreRenderingContext);

      CoreRenderingContext crc = (CoreRenderingContext)rc;
      StyleContext styleContext = crc.getStyleContext();
      StyleProvider styleProvider = styleContext.getStyleProvider();
      // skin properties are stored in an Map<Object, Object>
      _properties = styleProvider.getSkinProperties(styleContext);

      // Under normal circumstances, the StyleProvider will return
      // a non-null, modifiable map.  If the skin/style subsystem
      // has failed to initialize, however, the map may be null.
      // Substitute an empty map so we don't need to check for null
      // later.
      if (_properties == null)
      {
        if (_LOG.isWarning())
          _LOG.warning("_properties is an emptyMap because the skin/style " +
            "subsystem has failed to initialize.");
        _properties = _NULL_PROPERTIES;
      }
    }

    return _properties;
  }

  // The wrapped skin
  private final Skin _skin;

  // The icon map specific to this request as served
  // up by the StyleProvider.
  private Map<String, Icon> _icons;

  // The skin properties map specific to this request as served
  // up by the StyleProvider.
  private Map<Object, Object> _properties;

  // Marker used to cache nulls.
  private static final Icon _NULL_ICON = new NullIcon();

  // Empty map used when StyleProvider.getIcons() fails;
  private static final Map<String, Icon> _NULL_ICONS = Collections.emptyMap();

  // Empty map used when StyleProvider.getSkinProperties() fails;
  private static final Map<Object, Object> _NULL_PROPERTIES = Collections.emptyMap();
  static final private TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(RequestSkinWrapper.class);

}
