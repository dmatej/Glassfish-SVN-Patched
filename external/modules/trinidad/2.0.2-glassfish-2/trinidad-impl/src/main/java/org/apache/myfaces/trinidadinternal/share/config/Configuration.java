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
package org.apache.myfaces.trinidadinternal.share.config;


/**
 * The <code>Configuration</code> class and its default
 * implementation, <code>ConfigurationImpl</code>, are remnant
 * configuration classes for the UIX portions of Trinidad, and
 * should not be used for new code.
 * <p>
 * @see org.apache.myfaces.trinidadinternal.share.config.ConfigurationImpl
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/config/Configuration.java#0 $) $Date: 10-nov-2005.19:00:18 $
 */
abstract public class Configuration
{
  /**
   * Disables optimizations that are normally performed by the
   * Trinidad Renderers to reduce content size.
   * <p>
   * This Boolean property controls whether or not Trinidad Renderer
   * implementations should attempt to reduce the size of generated
   * content, for example, by compressing style class names.  These
   * optimizations are enabled by default.  In general,
   * users should not need to disable these optimizations.  However,
   * users who want to build custom skins for Trinidad will find this
   * setting essential.  Use Boolean.TRUE to disable compression.
   */
  static public final String DISABLE_CONTENT_COMPRESSION =
    "org.apache.myfaces.trinidad.DISABLE_CONTENT_COMPRESSION";
    
  static public final String CHECK_TIMESTAMP_PARAM =
  "org.apache.myfaces.trinidad.CHECK_FILE_MODIFICATION";

  /**
   * Key for the base UIX directory.
   */
  static public final Object BASE_DIRECTORY         = "base";

  /**
   * Key for the UIX images directory.
   */
  static public final Object IMAGES_DIRECTORY       = "images";

  /**
   * Key for the UIX image cache directory.
   */
  static public final Object IMAGES_CACHE_DIRECTORY = "imagesCache";

  /**
   * Key for the UIX styles directory.
   */
  static public final Object STYLES_DIRECTORY       = "styles";

  /**
   * Key for the UIX styles cache directory.
   */
  static public final Object STYLES_CACHE_DIRECTORY = "stylesCache";

  /**
   * Key for the UIX jsLibs directory.
   */
  static public final Object JSLIBS_DIRECTORY       = "jsLibs";

  /**
   * Key for the UIX JSP directory.  Clients must be careful
   * when setting this parameter.  When the JSPs are executed,
   * they will attempt to use the same Configuration as the
   * page that launched them.  However, if the JSPs were launched
   * onto a different machine, that configuration may not be
   * available.  Even if launched onto the same machine,
   * any context-relative directories will be resolved relative
   * to the new JSP - not the original page.  Consequently,
   * if clients set this directory to a full path/URI, they
   * should generally set all directories to full paths.
   */
  static public final Object JSPS_DIRECTORY         = "jsps";

  /**
   * Key for the Skin family property.  The Skin family stored
   * in this key is used to help determine the default
   * Skin instance to use.
   *
   * @see org.apache.myfaces.trinidadinternal.skin.Skin  */

  // THE VALUE OF THIS STRING MATTERS: IT IS USED IN XML PARSING  
  static public final Object SKIN_FAMILY = "skinFamily";

  /**
   * Key for the LookAndFeelManager property.  The LookAndFeelManager instance
   * stored under this key is used to choose the LookAndFeel to use.
   *
   * @see org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager
   */
  static public final Object LOOK_AND_FEEL_MANAGER = "lookAndFeelManager";

  /**
   * Key for the RendererManager property.  Any UIX Components RendererManager
   * stored at this key will be used by UIX Components's ServletRenderingContext
   * if none is explicitly attached.
   */
  static public final Object RENDERER_MANAGER = "rendererManager";

  /**
   * Key for the XMLProvider property.  UIX projects will use
   * the String, Class, or XMLProvider instance registered with
   * this property.
   */
  static public final Object XML_PROVIDER = "XMLProvider";

  /**
   * Key for enabled headless rendering.  The value is a Boolean object.  If
   * set to Boolean.TRUE, UIX Components will avoid any rendering which requires
   * graphical capabilities.  This means, for example, that existing
   * cached images will be used, but new images will not be dynamically
   * generated.  Enabling headless rendering might cause a slight
   * degradation of the UI, as elements which are best implemented as
   * images could instead be rendered as HTML links or using table-based
   * constructs.
   */
  // THE VALUE OF THIS STRING MATTERS: IT IS USED IN XML PARSING
  static public final Object HEADLESS = "headless";

  /**
   * Key used to specify the URL location of the ImageServlet to use
   * for image generation.
   */
  // THE VALUE OF THIS STRING MATTERS: IT IS USED IN XML PARSING
  static public final Object IMAGE_SERVLET_URL = "imageServletURL";


  public static final Object DISABLE_STANDARDS_MODE =
    "disableStandardsMode";

  /**
   * Return a URI for a UIX directory.
   * @param key the key used to identify the directory
   * @param contextURI the current contextURI;  this will
   *   be preprended to the returned URI if this directory
   *   is registered as (or defaulting to) context-relative.
   *   This path must not be terminated with a separator ("/").
   * @return a URI, which will always be terminated with a separator
   * @exception DirectoryUnavailableException if the directory is
   *    unavailable
   */
  abstract public String getURI(Object key, String contextURI);


  /**
   * Return a path for a UIX directory.
   * @param key the key used to identify the directory
   * @param contextPath the current context path;  this will
   *   be preprended to the returned path if this directory
   *   is registered as (or defaulting to) context-relative.
   * @return a full file system path, which will always be
   *   terminated with the appropriate separator for the file system
   * @exception DirectoryUnavailableException if the directory is
   *    unavailable
   */
  abstract public String getPath(Object key, String contextPath);


  /**
   * Return a registered property.
   * @param key the key used to identify the property
   * @return the registered object, or null if no object
   *    was registered.
   */
  abstract public Object getProperty(Object key);


  /**
   * Returns whether the configuration is in debug mode.
   */
  abstract public boolean isDebug();


  // Package-private constructor, reserved for
  // the default ConfigurationImpl configuration
  Configuration()
  {
  }
}
