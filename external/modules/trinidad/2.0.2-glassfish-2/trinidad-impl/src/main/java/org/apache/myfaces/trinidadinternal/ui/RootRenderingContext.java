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
package org.apache.myfaces.trinidadinternal.ui;

import java.util.Map;
import java.util.Hashtable;

import java.util.MissingResourceException;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.Icon;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;

import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.share.config.ContextBasedConfiguration;
import org.apache.myfaces.trinidadinternal.share.url.FacesURLEncoder;

import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.share.url.NullFormEncoder;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;

import org.apache.myfaces.trinidad.context.LocaleContext;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProvider;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.image.ImageProviderResponse;

import org.apache.myfaces.trinidadinternal.image.cache.FileSystemImageCache;

import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;
import org.apache.myfaces.trinidadinternal.ui.data.DataObject;

import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager;

import org.apache.myfaces.trinidadinternal.ui.expl.UIVariableResolver;

/**
 * Default implementation of RenderingContext used as the root for rendering
 * a tree of UINodes.  Developers that need to render UINode content
 * outside of a Servlet or JSP should use this class, but all others
 * should use its subclass,
 * {@link ServletRenderingContext ServletRenderingContext}.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/RootRenderingContext.java#0 $) $Date: 10-nov-2005.18:50:21 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class RootRenderingContext extends RenderedNodeRenderingContext
                                  implements ImageContext
{
  /**
   * Creates a RootRenderingContext using the default RendererManager
   * instance.
   */
  public RootRenderingContext()
  {
    super();

    _facet   = UIConstants.FACET_DEFAULT;
    _manager = null;
    _formEncoder = NullFormEncoder.sharedInstance();
  }

  public void init(FacesContext fContext)
  {
    _facesContext = fContext;
    setURLEncoder(new FacesURLEncoder(fContext));
    setProperty(UIConstants.MARLIN_NAMESPACE,
                CONTEXT_URI_PROPERTY,
                fContext.getExternalContext().getRequestContextPath());
    setProperty(UIConstants.MARLIN_NAMESPACE,
                CONTEXT_PATH_PROPERTY,
                CoreRenderingContext.getTemporaryDirectory(fContext));

  }

  @Override
  public FacesContext getFacesContext()
  {
    return _facesContext;
  }



  /**
   * Sets the configuration for use with this rendering
   * pass.  If set to null, the configuration will return
   * to the default behavior.
   */
  public void setConfiguration(Configuration config)
  {
    _config = config;

    if ((config != null) &&
        !(config instanceof ContextBasedConfiguration))
    {
      throw new IllegalStateException(_LOG.getMessage(
        "ONLY_CONTEXTBASEDCONFIGURATION_SUPPORTED"));
    }
  }

  /**
   * Returns the LookAndFeel to use for this render.
   */
  public LookAndFeel getLookAndFeel()
  {
    if (_laf == null)
    {
      _laf = _getLookAndFeelManager().getLookAndFeel(this);
    }

    return _laf;
  }


  /**
   * Returns the rendering facet in use.
   */
  public String getFacet()
  {
    return _facet;
  }

  /**
   * Sets the facet of the current look-and-feel.  This must not
   * be called after getRendererManager()
   */
  public void setFacet(String facet)
  {
    if (_manager != null)
      throw new IllegalStateException(_LOG.getMessage(
        "FACET_MAY_NOT_BE_SET_AFTER_RENDERERMANAGER_HAS_BEEN_ASSIGNED"));
    if (facet == null)
      facet = UIConstants.FACET_DEFAULT;

    _facet = facet;
  }


  /**
   * Returns the RendererManager that should be used
   * for finding renderers.  The default implementation
   * returns the manager registered on the Configuration,
   * if any;  if that fails, it returns a globally shared instance,
   */
  public RendererManager getRendererManager()
  {
    RendererManager manager = _manager;

    // If the manager is
    if (manager == null)
    {
      Configuration config = getConfiguration();
      if (config != null)
      {
        Object o = config.getProperty(Configuration.RENDERER_MANAGER);
        if (o instanceof RendererManager)
        {
          manager = _manager = (RendererManager) o;
        }
      }

      if (manager == null)
      {
        LookAndFeel laf = getLookAndFeel();
        manager = _manager = laf.getRendererManager(_facet);
      }
    }

    return manager;
  }


  /**
   * Returns the LocaleContext that should be used for rendering.
   * The LocaleContext is the owner of all LocaleSpecific information
   * about the Locale.
   */
  abstract public LocaleContext getLocaleContext();

  /**
   * Returns the ResponseWriter that should be used
   * for rendering text.
   */
  public ResponseWriter getResponseWriter()
  {
    return getFacesContext().getResponseWriter();
  }

  /**
   * Sets the ResponseWriter that should be used
   * for rendering text.
   */
  public void setResponseWriter(ResponseWriter writer)
  {
    getFacesContext().setResponseWriter(writer);
  }


  /**
   * Stores a data object on the context.  Clients using this
   * method should strongly consider whether their code might
   * function more effectively by overriding getDataObject(),
   * and, for their namespaces, dynamically instantiating DataObjects.
   * @param namespaceURI the namespace of the data object
   * @param name the namespace of the data object
   * @param dataObject the data object
   */
  public void setDataObject(
    String     namespaceURI,
    String     name,
    DataObject dataObject
    )
  {
    boolean doAdd = (dataObject != null);

    Map<String, DataObject> dataObjects = _getDataObjectMap(namespaceURI, doAdd);

    if (doAdd)
    {
      dataObjects.put(name, dataObject);
    }
    else
    {
      if (dataObjects != null)
      {
        dataObjects.remove(name);
      }
    }
  }


  /**
   * Registers a dictionary for use in retrieving and storing data
   * objects for a single namespace.  Any data objects previously
   * registered for that namespace will be dropped.  If the dictionary
   * is null, all data objects for that namespace will be deleted.
   *
   * @param namespaceURI the namespace of the data object
   * @param dataObjectDict a Map of data objects
   */
  public void setDataObjectMap(
    String     namespaceURI,
    Map<String, DataObject> dataObjectDict)
  {
    if (dataObjectDict != null)
    {
      _dataObjects = ArrayMap.put(_dataObjects, namespaceURI, dataObjectDict);
    }
    else
    {
      _dataObjects = ArrayMap.remove(_dataObjects, namespaceURI);
    }
  }

  /**
   * Returns the DataObject for the given namespace and name pair.
   * It will call each data provider added with addDataProvider,
   * starting with the most recently added, until one returns non-null.
   * Then, it will look for DataObjects added with setDataObject().
   * Developers cannot override this method;  they must override
   * the 3-argument getDataObject().
   */
  @Override
  public final DataObject getDataObject(
    String namespaceURI,
    String name)
  {
    // Stub for backwards link-compatibility
    return super.getDataObject(namespaceURI, name);
  }


  /**
   * Returns the DataObject for the given namespace and name pair.
   * It will call each data provider added with addDataProvider,
   * starting with the most recently added, until one returns non-null.
   * Then, it will look for DataObjects added with setDataObject().
   * @since 2.0.12
   */
  @Override
  public DataObject getDataObject(
    UIXRenderingContext context,
    String namespaceURI,
    String name
    )
  {
    // try the dataproviders first
    DataObject data = super.getDataObject(context, namespaceURI, name);

    if (data == null)
    {
      Map<String, DataObject> dataObjects = _getDataObjectMap(namespaceURI, false);

      if (dataObjects != null)
      {
        data = dataObjects.get(name);
      }
    }

    return data;
  }


  /**
   * Gets a property stored on the context.
   */
  @Override
  public Object getProperty(
    String namespace,
    Object key
    )
  {
    Object o = super.getProperty(namespace, key);

    if (o == null)
    {
      if (ImageConstants.TECATE_NAMESPACE.equals(namespace))
      {
        if (ImageConstants.IMAGE_PROVIDER_PROPERTY == key)
        {
          ImageProvider imageProvider = _getDefaultImageProvider();

          setProperty(ImageConstants.TECATE_NAMESPACE,
                      ImageConstants.IMAGE_PROVIDER_PROPERTY,
                      imageProvider);

          o = imageProvider;
        }
      }
    }

    return o;
  }


  /**
   * This implementation returns an encoder that does no special encoding,
   * and returns an empty string as the default URL.
   * @see UIXRenderingContext#getURLEncoder()
   */
  public URLEncoder getURLEncoder()
  {
    URLEncoder urlEncoder = _urlEncoder;

    if (urlEncoder == null)
      return _URL_ENCODER;
    return urlEncoder;
  }


  /**
   * This implementation returns an encoder that does no special encoding,
   * and returns an empty string as the default URL.
   * @see UIXRenderingContext#getURLEncoder()
   */
  public void setURLEncoder(URLEncoder urlEncoder)
  {
    _urlEncoder = urlEncoder;
  }

  /**
   * Returns a Configuration object that will be used to
   * locate paths and return global properties.
   */
  public Configuration getConfiguration()
  {
    return _config;
  }

  /**
   * This implementation returns an encoder that does no special encoding.
   * @see UIXRenderingContext#getFormEncoder()
   */
  public FormEncoder getFormEncoder()
  {
    return _formEncoder;
  }

  /**
   * This implementation returns an encoder that does no special encoding.
   * @see UIXRenderingContext#getFormEncoder()
   */
  public void setFormEncoder(FormEncoder formEncoder)
  {
    _formEncoder = formEncoder;
  }

  /**
   * Get an interface that can be used for image lookups and rendering.
   */
  public ImageContext getImageContext()
  {
    return this;
  }

  /**
   * Returns a translated value from the skin's resource bundle.
   * Logs a severe message if there is a MissingResourceException.
   */
  public Object getTranslatedValue(String key)
  {
    String mappedKey = getSkinResourceMappedKey(key);

    if (mappedKey != null)
    {
      try
      {
        return getSkin().getTranslatedValue(getLocaleContext(),
                                            mappedKey);
      }
      catch (MissingResourceException e)
      {
        // log the error and return
        _LOG.severe(e);

        return null;
      }
    }
    else
      return null;

  }

  /**
   *
   * We'll look at the reading direction and append :rtl if needed to the name
   * @param namespace
   * @param iconName
   * @return
   */
  public Icon getIcon(
    String  iconName
    )
  {
    String mappedIconName = getSkinResourceMappedKey(iconName);
    Icon rtlIcon = null;
    Icon defaultRTLIcon = null;
    Skin skin = getSkin();
    if (mappedIconName != null)
    {
      if (getLocaleContext().isRightToLeft() &&
          (!mappedIconName.endsWith(StyleUtils.RTL_CSS_SUFFIX)))
      {
        // append :rtl to the mappedIconName. If no icon with that name,
        // default to getting the icon with the original mappedIconName.
        String rtlIconName = mappedIconName+StyleUtils.RTL_CSS_SUFFIX;
        // @todo I could optimize this a bit by having a method on SkinExtension
        // which doesn't create a NullIcon when the icon is not found, if I
        // know I want to set it to something else anyway.
        rtlIcon = skin.getIcon(rtlIconName);

        if (rtlIcon == null)
        {
          // we want :rtl icons to default to regular icons, not a NullIcon,
          //  which is what the Skin does.
          // Couldn't find rtl icon, so default it to the regular icon
          defaultRTLIcon = skin.getIcon(mappedIconName);
          if (defaultRTLIcon != null)
          {
            // cache regular icon so we don't need to get it again!
            skin.registerIcon(rtlIconName, defaultRTLIcon);
          }

        }
        return (rtlIcon != null) ? rtlIcon : defaultRTLIcon;

      }
      else
      {
        // no rtl icon
        return skin.getIcon(mappedIconName);
      }


    }
    else
      return null;

  }


  /**
   * Returns a translated String from the skin's resource bundle.
   * Logs a severe message if there is a MissingResourceException.
   */
  public String getTranslatedString(String key)
  {
    String mappedKey = getSkinResourceMappedKey(key);

    if (mappedKey != null)
    {
      try
      {
        return getSkin().getTranslatedString(getLocaleContext(),
                                             mappedKey);
      }
      catch (MissingResourceException e)
      {
        // log the error and return
        _LOG.severe(e);

        return null;
      }
    }
    else
      return null;

  }


  public Object getStyleClass(String  key)
  {
    return getSkinResourceMappedKey(key);
    /*** =-=jmw @todo get the style class from the skin.
     * the skin should return a short style class given a long style class
     * like af|breadCrumbs::step-selected
    if (mappedKey != null)
    {
      return getSkin().getStyleClass(mappedKey);
    }
    else
      return null;
  */
  }

  public final UIXRenderingContext getParentContext()
  {
    return null;
  }

  @Override
  public Object clone()
  {
    RootRenderingContext context = (RootRenderingContext)super.clone();

    if (_dataObjects !=  null)
    {
      Object[] dataObjects = new Object[_dataObjects.length];

      System.arraycopy(_dataObjects, 0, dataObjects, 0, _dataObjects.length);

      context._dataObjects = dataObjects;
    }

    return context;
  }

  /**
   * @see #createVariableResolver
   */
  public final UIVariableResolver getVariableResolver()
  {
    // lazily create the variableResolver:
    if (_varResolver == null)
      _varResolver = createVariableResolver();

    return _varResolver;
  }

  /**
   * creates a variable resolver to use with this render cycle
   */
  protected UIVariableResolver createVariableResolver()
  {
    return new UIVariableResolver();
  }

  /**
   * Returns the default initial number of nodes in the stack of
   * logical nodes.
   */
  @Override
  protected int getDefaultNodeStackSize()
  {
    return _DEFAULT_STACK_SIZE;
  }


  @Override
  protected int getDefaultPropertyMapSize()
  {
    return _DEFAULT_PROPERTY_MAP_SIZE;
  }


  /**
   * Returns the LookAndFeelManager to use.
   */
  private LookAndFeelManager _getLookAndFeelManager()
  {
    LookAndFeelManager manager = null;

    //
    // try getting the LookAndFeelManager from the Configuration
    //
    Configuration config = getConfiguration();

    if (config != null)
    {
      Object o = config.getProperty(Configuration.LOOK_AND_FEEL_MANAGER);
      if (o instanceof LookAndFeelManager)
      {
        manager = (LookAndFeelManager)o;
      }
    }

    if (manager == null)
    {
      // not set on the configuration, so use the default
      manager = LookAndFeelManager.getDefaultLookAndFeelManager();
    }

    return manager;
  }

  /**
   * Returns the Map of dataObjects for this namespace.
   */
  @SuppressWarnings("unchecked")
  private Map<String, DataObject> _getDataObjectMap(
    String   namespace,
    boolean  createIfNull
    )
  {
    Map<String, DataObject> dataObjects = 
      (Map<String, DataObject>) ArrayMap.getByIdentity(_dataObjects,
                                                       namespace);

    if (createIfNull && (dataObjects == null))
    {
      //
      // create a hashtable of properties for this namespace and add it
      // to the map of properties
      //
      dataObjects = new Hashtable<String, DataObject>(11);

      _dataObjects = ArrayMap.put(_dataObjects,
                                  namespace,
                                  dataObjects);
    }

    return dataObjects;
  }


  // Creates a default Tecate's FileSystemImageCache.
  // Uses the web server root and base image uri to determine the
  // location of the default cache
  private ImageProvider _getDefaultImageProvider()
  {
    ImageProvider provider = null;
    Configuration config = getConfiguration();
    try
    {
      // We explicitly avoid using the SHARED_CONTEXT_PATH for the
      // image cache.  That way, we can ensure that each application
      // creates its own local image cache.
      String contextPath = (String) _getContextProperty(CONTEXT_PATH_PROPERTY);
      String path = config.getPath(Configuration.IMAGES_CACHE_DIRECTORY,
                                   contextPath);

      provider = FileSystemImageCache.getSharedCache(path);
    }
    catch (Exception e)
    {
      _LOG.severe("CANNOT_GET_IMAGE_CACHE", e);
    }

    if (provider == null)
      return NullImageProvider.getInstance();

    return provider;
  }

  // Gets a context property from the correct namespace
  private Object _getContextProperty(
    Object key
    )
  {
    return getProperty(UIConstants.MARLIN_NAMESPACE, key);
  }

  // Implementation of ImageProvider which does nothing - used as a
  // placeholder when we can't get the real ImageProvider
  static private class NullImageProvider implements ImageProvider
  {
    private NullImageProvider() {}

    static public ImageProvider getInstance()
    {
      if (_sInstance == null)
        _sInstance = new NullImageProvider();

      return _sInstance;
    }

    public ImageProviderResponse getImage(
      ImageContext context,
      ImageProviderRequest request
      )
    {
      return null;
    }

    private static ImageProvider _sInstance;
  }


  static private final URLEncoder _URL_ENCODER = new URLEncoder()
  {
    public final String encodeURL(String url)
    {
      return url;
    }

    public final String encodeParameter(String key)
    {
      return key;
    }

    public final String getDefaultURL()
    {
      return "";
    }
  };

  private Object[]     _dataObjects;
  private LookAndFeel     _laf;
  private RendererManager _manager;
  private Configuration   _config;
  private URLEncoder      _urlEncoder;
  private String          _facet;
  private UIVariableResolver _varResolver = null;
  private FormEncoder     _formEncoder;
  private FacesContext  _facesContext;

  // Use a very large per-namespace map.  Even 101 might
  // be too small for what some developers are using this for.
  private static final int _DEFAULT_PROPERTY_MAP_SIZE = 101;

  private static final int _DEFAULT_STACK_SIZE = 40;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(RootRenderingContext.class);
}
