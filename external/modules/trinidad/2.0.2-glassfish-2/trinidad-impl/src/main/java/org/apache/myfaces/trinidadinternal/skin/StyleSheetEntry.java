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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidadinternal.share.io.CachingNameResolver;
import org.apache.myfaces.trinidadinternal.share.io.FileInputStreamProvider;
import org.apache.myfaces.trinidadinternal.share.io.URLInputStreamProvider;
import org.apache.myfaces.trinidadinternal.share.xml.JaxpXMLProvider;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContextImpl;
import org.apache.myfaces.trinidadinternal.share.xml.XMLProvider;

import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.xml.StyleSheetDocumentUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;


/**
 * Package-private utility class used by Skin implementation
 * to manage a single XSS or CSS skin stylesheet source file .
 * This class calls the parsing code which parses either the XSS or CSS file (_createSkinStyleSheet),
 * and it stores a StyleSheetDocument object, which is a parsed representation of a 
 * Trinidad style sheet document whether that is in the css or xss format or merged.
 * This class could actually
 * be pushed into an inner class in Skin, but at the moment
 * it is separated out simply to reduce the amount of code in
 * Skin.java.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/StyleSheetEntry.java#0 $) $Date: 10-nov-2005.18:59:01 $
 */
class StyleSheetEntry
{
  /**
   * Creates a StyleSheetEntry for the specified context and styleSheetName.
   * This method will log any errors/exceptions and return
   * null if the style sheet source file could not be found/parsed.
   */
  public static StyleSheetEntry createEntry(
    StyleContext     context,
    String           styleSheetName
    )
  {
    // In order to create the StyleSheetEntry, we need to locate and
    // parse the style sheet file.  We use a NameResolver to use to
    // find the style sheet.
    NameResolver resolver = _getNameResolver(context, styleSheetName);
    if (resolver == null)
      return null;

    // a private static inner class to store the document, icon, and skin properties
    // =-=jmw @todo Should I just create a StyleSheetEntry directly,
    // and make the constructor public? (probably)
    StyleSheetEntry skinStyleSheet = _createSkinStyleSheet(resolver,
                                                           styleSheetName);


      if (skinStyleSheet == null)
        return null;

      // We either a special subclass of StyleSheetEntry that will recalculate 
      // the StyleSheetEntry if the skin is dirty or if the web.xml's 
      // CHECK_FILE_MODIFICATION flag is set and there are file modifications.
      boolean checkStylesModified = context.checkStylesModified();
      return new CheckModifiedEntry(styleSheetName,
                                    skinStyleSheet.getDocument(),
                                    resolver,
                                    checkStylesModified);





  }

  // Creates a StyleSheetEntry which never checks for
  // modifications.
  // =-=jmw there is a hotspot bug I filed with JDeveloper
  // 4102252 when this is private I get an IllegalAccessException
  // in CheckModifiedEntry. changing it to package private
  StyleSheetEntry(
    String                 styleSheetName,
    StyleSheetDocument     document
    )
  {
    _name       = styleSheetName;
    _document   = document;

  }

  StyleSheetEntry(String styleSheetName)
  {
    this(styleSheetName, null);
  }

  // Use full constructor
  private StyleSheetEntry()
  {
  }

  /**
   * Returns the name of the style sheet source file
   * for this StyleSheetEntry.
   */
  public String getStyleSheetName()
  {
    return _name;
  }

  /**
   * Returns the StyleSheetDocument for this
   * StyleSheetEntry.
   */
  public StyleSheetDocument getDocument()
  {
    return _document;
  }

  /**
   * Checks whether the underlying style sheet source file
   * has been modified and if so, reloads the StyleSheetDocument.
   * Returns true if the document has been modified (and the
   * StyleSheetDocument has been reloaded).
   */
  public boolean checkModified(StyleContext context)
  {
    return false;
  }
  
  // Gets a File for the specified name, or returns null if no file exists
  // Try the local styles directory.
  public static File resolveLocalFile(File localStylesDir, String name)
  {
    // Try the local styles directory
    File file = new File(localStylesDir, name);
    if (file.exists())
      return file;

    return null;
  }

  // Gets an URL for the specified name using ClassLoaderUtils.getResource
  public static URL resolveClassLoaderURL(String name)
  {
    if (name == null)
      return null;
    return ClassLoaderUtils.getResource(name);

  }

  // Gets an URL for the non static urls -- that is, urls that could change after the
  // server has started.
  public static URL resolveNonStaticURL(String name)
  {
    if (name == null)
      return null;
    FacesContext fContext = FacesContext.getCurrentInstance();
    if (fContext != null)
    {
      try
      {
        if (name.startsWith("http:") ||
            name.startsWith("https:") ||
            name.startsWith("file:") ||
            name.startsWith("ftp:") ||
            name.startsWith("jar:"))
        {
          URL url = new URL(name);
          if (url != null)
            return url;
        }
        else
        {
          String rootName = _getRootName(name);
          // Return a URL for the application resource mapped to the specified path,
          // if it exists; otherwise, return null.
          URL url = fContext.getExternalContext().getResource(rootName);
          if (url != null)
            return url;
        }
      }
      catch (MalformedURLException e)
      {
        // Eat the MalformedURLException - maybe the name isn't an URL
        ;
      }
    }
    return null;
  }  
  

  // Called by CheckModifiedEntry when the style sheet has changed
  void __setDocument(StyleSheetDocument document)
  {
    _document = document;
  }

  // Creates the SkinStyleSheet (a private static inner class that
  // contains StyleSheetDocument plus a list
  // of properties) from a CSS file
  //
  private static StyleSheetEntry _createSkinStyleSheet(
    NameResolver     resolver,
    String           styleSheetName
    )
  {

    StyleSheetEntry skinStyleSheet = null;

    if (styleSheetName.endsWith(".css"))
    {
      // this will parse a skin css file which allows icons, properties,
      // and styles.
        skinStyleSheet =  _createSkinStyleSheetFromCSS(resolver,
                                                       styleSheetName);
    } else {
      String message = _LOG.getMessage("INVALID_STYLESHEET_TYPE", new Object[]{styleSheetName});
      _LOG.severe(message);
    }

    return skinStyleSheet;

  }


  // Creates the StyleSheetEntry from a skinning file that ends in .css
  private static StyleSheetEntry _createSkinStyleSheetFromCSS(
    NameResolver     resolver,
    String           styleSheetName
    )
  {

     try
     {
        // We simply use a ParseContext as a place to store parameters like
       // inputStreamProviders and nameResolvers that will be reused when parsing
        ParseContextImpl parseContext = new ParseContextImpl();
        // if this is a utility that isn't in this file, then I can't return a SkinStyleSheet.
        // I think instead this parseCSSSource should return a new instance of StyleSheetEntry.
        return SkinStyleSheetParserUtils.parseCSSSource(
                                    parseContext,
                                    resolver,
                                    styleSheetName,
                                    StyleSheetEntry.class);
     }
     catch (Exception e)
     {
       if (_LOG.isSevere())
         _LOG.severe("CANNOT_LOAD_STYLESHEET", styleSheetName);
         _LOG.severe(e);

     }
      return null;
  }

  // Returns the NameResolver to use for locating and loading style sheet file.
  // Depending upon what the styleSheetName is, we load the file different way: local file,
  // url, etc.
  private static NameResolver _getNameResolver(
    StyleContext context,
    String       styleSheetName
    )
  {
    // get localStylesDirectory
    File localStylesDir = _getStylesDir(context);

    // Make sure we have some styles directory
    if ((localStylesDir == null))
    {
      _LOG.warning(_STYLES_DIR_ERROR);
      return null;
    }
    NameResolver resolver = null;

    try
    {
      resolver =
          _getNameResolverForStyleSheetFile(context, localStylesDir, styleSheetName);
    }
    catch (IOException e)
    {
      if (_LOG.isWarning())
        _LOG.warning("CANNOT_LOAD_STYLESHEET", styleSheetName);
        _LOG.warning(e.getMessage());
    }
    if (resolver == null)
    {
      // If we can't get a NameResolver, something is seriously wrong.
      // createResolver() logged the error already, so just return null.
      return null;
    }

    // Wrap up the resolver in a CachingNameResolver that we can
    // can use to check for updates to imported style sheets
    return new CachingNameResolver(resolver, null, true);
  }
  
  /**
   * <p>
   * This method tries to find the Skin's stylesheet file (e.g., purple-desktop.css). 
   * It creates a NameResolver object, and it returns the NameResolver object. 
   * A NameResolver object contains an
   * InputStreamProvider (this object loads the file) and a sub- NameResolver 
   * that finds files that are relative to the base file, like an @import file in a .css file.
   * </p>
   * <p>
   * This method tries to find the stylesheet file, first locally, or using an url, or a static url, 
   * then we create a StyleSheetNameResolver and we pass in the InputStreamProvider we created that
   * we know can find the file. If we can't find the file any of these ways, then we check 
   * META-INF/services for a NameResolver service. This is how a third party can customize
   * how they can find files, by supplying a META-INF/services NameResolver implementation. 
   * </p>
   * @param context
   * @param localStylesDir File the local styles directory
   * @param filename the stylesheet name
   * @return NameResolver - either a StyleSheetNameResolver or the META-INF/services NameResolver
   * implementation. The META-INF/services NameResolver implementation is the way a third party
   * can customize the way they find and load files.
   * @throws FileNotFoundException when the file could not be found in all of the ways we tried to find it.
   */
  private static NameResolver _getNameResolverForStyleSheetFile(
    StyleContext context, 
    File         localStylesDir,
    String       filename) throws IOException
  {
    InputStreamProvider provider = null;
    
    File file = StyleSheetEntry.resolveLocalFile(localStylesDir, filename);
    if (file != null)
      provider = new FileInputStreamProvider(file);
    
    if (provider == null)
    {
      // Gets an URL for the specified name.
      // Try a few different means to get the file as an url and then create the appropriate
      // InputStreamProvider from that URL.
      URL url = resolveNonStaticURL(filename);
      if (url != null)
        provider = new URLInputStreamProvider(url);
      else
      {
        // see if it is an URL that can be loaded by the ClassLoader.
        // We create a StaticURLInputStreamProvider from the url because we consider the
        // url static because it can't be changed without restarting the server, so we don't
        // need to check if the source has changed.
        url = resolveClassLoaderURL(filename);
        if (url != null)
          provider = new StaticURLInputStreamProvider(url);
      }
    }
    // If at this point we have found an InputStreamProvider, then we will create a 
    // StyleSheetNameResolver. Otherwise, we need to check for a custom NameResolver.
    if (provider != null)
      return StyleSheetNameResolver.createResolver(context, localStylesDir, provider);

    // If we still can't locate the file at this point, then look for a custom
    // NameResolver specified as a META-INF\services.
    NameResolver servicesNameResolver = _loadNameResolverFromServices(filename);
    if (servicesNameResolver != null)
    {
      if (_LOG.isFine())
      {
        _LOG.fine("Using the InputStreamProvider from META-INF\\services");
      }
      return servicesNameResolver;
    }

    // If we couldn't locate the file, throw an IOException
    throw new FileNotFoundException(_getFileNotFoundMessage(localStylesDir, filename));
  }


  // Subclass of StyleSheetEntry which recreates the StyleSheetEntry
  // if the skin is marked dirty (skin.isDirty()) or if the underlying 
  // source files have been modified and CHECK_FILE_MODIFICATION flag is set
  // in web.xml.
  private static class CheckModifiedEntry extends StyleSheetEntry
  {
    public CheckModifiedEntry(
      String                 styleSheetName,
      StyleSheetDocument     document,
      NameResolver           resolver,
      boolean                checkFileModifiedFlagSet
      )
    {
      super(styleSheetName, document);

      // We need the InputStreamProvider in order to check
      // for modifications.  Get it from the NameResolver.
      _provider = _getInputStreamProvider(resolver);
      _checkFileModifiedFlagSet = checkFileModifiedFlagSet;
    }

    // Override of checkModified() which first checks if the file
    // needs to be reparsed and a new CSS file generated.
    // The conditions are if the skin is marked dirty, or if the 
    // web.xml's CHECK_FILE_MODIFICATION flag is set and the source
    // has changed. The InputStreamProvider's hasSourceChanged method
    // is called to see if the source has changed.
    @Override
    public boolean checkModified(StyleContext context)
    {
      // We would synchronize here, but at the moment synchronization
      // is provided by Skin.getStyleSheetDocument().
      if (context.isDirty() || 
          (_checkFileModifiedFlagSet && 
           ((_provider != null) && (_provider.hasSourceChanged())))  )
      {
        // Throw away the old InputStreamProvider and StyleSheetDocument
        _provider = null;
        __setDocument(null);

        // Get a new NameResolver
        String name = getStyleSheetName();
        NameResolver resolver = _getNameResolver(context, name);
        if (resolver != null)
        {
          

          // Recreate the StyleSheetEntry for the styleSheet using the new NameResolver
          // (e.g., if purpleSkin.css
          // has changed, create the SkinStyleSheetEntry for purpleSkin.css)
          // Using a new NameResolver like we do ensures that we don't get a 
          // cached result from the provider 
          // (see SkinStyleSheetParserUtils.parseCSSSource's getCachedResult)
          StyleSheetEntry skinStyleSheet = _createSkinStyleSheet(resolver,
                                                                 name);

          if (skinStyleSheet != null)
          {
            _provider = _getInputStreamProvider(resolver);
            __setDocument(skinStyleSheet.getDocument());
          }

          return true;
        }
      }

      return false;
    }

    private InputStreamProvider _getInputStreamProvider(
      NameResolver resolver
      )
    {
      // Note: We assume that we are using a CachingNameResolver,
      // and that the InputStreamProvider for the source file has
      // already been retrieved.  That way, when we call
      // NameResolver.getProvider(), we are actually getting
      // the same InputStreamProvider that was used to read the
      // style sheet earlier.

      assert (resolver instanceof CachingNameResolver);

      try
      {
        return resolver.getProvider(getStyleSheetName());
      }
      catch (IOException e)
      {
        // We shouldn't get here - we know we were able to
        // get the InputStreamProvider before - so we should be
        // able to get the cached InputStreamProvider now
        assert false;
      }

      return null;
    }

    private InputStreamProvider _provider;
    private boolean _checkFileModifiedFlagSet;
  }
  


  // Construct error message for the specified file name
  private static String _getFileNotFoundMessage(File localStylesDir, String name)
  {
    StringBuffer buffer = new StringBuffer();
    buffer.append("Unable to locate the skin's style sheet \"");
    buffer.append(name);
    buffer.append("\" in ");

    if (localStylesDir != null)
    {
      buffer.append("local styles directory (");
      buffer.append(localStylesDir.getPath());
      buffer.append("), ");
    }

    buffer.append("or on the class path.\n");
    buffer.append("Please be sure that this style sheet is installed.");

    return buffer.toString();
  }

  // Returns the File corresponding to the styles directory - either
  // the local directory or the shared directory - depending on the
  // shared value
  private static File _getStylesDir(
    StyleContext context)
  {
    String contextPath = context.getGeneratedFilesPath();

    // We only need to look up the shared styles path if the shared
    // context path is non-null.  If the shared context path is null,
    // we don't have a shared styles directory (and calling
    // Configuration.getPath() may throw a DirectoryUnavailableException).
    if (contextPath == null)
      return null;

    String stylesPath = contextPath + "/adf/styles";

    // Convert the path to a File
    File stylesDir = new File(stylesPath);

    // Make sure the directory actually exists
    if (stylesDir.exists())
       return stylesDir;

    return null;
  }

  // Returns a name which can be resolved relative to the
  // ServletContext root.
  private static String _getRootName(String name)
  {
    // Tack on a starting "/" if the name doesn't already have one -
    // seems to be required by ServletContext.getRealPath() and
    // ServletContext.getResource() - at least on OC4J.
    return (name.startsWith("/")) ? name : ("/" + name);
  }

  /**
   * Returns an instance of NameResolver that was set in META-INF\services.
   * This is used only if the stylesheet cannot be found any other way.
   * This way third party users can create their own way to find the file e.g., MDS.
   *
   * @return a NameResolver instance that has been defined in META-INF\services\
   * org.apache.myfaces.trinidad.share.io.NameResolver
  // In this file they will have a line like "org.mycompany.io.MyNameResolverImpl".
   * null if no NameResolver is found.
   */
  static private NameResolver _loadNameResolverFromServices(String name)
  {
    // We don't want to check services every time, so instead store it on the applicationMap.
    FacesContext context = FacesContext.getCurrentInstance();
    Map<String, Object> appMap = context.getExternalContext().getApplicationMap();

    // Is it stored on the application map already? If so, use it.
    NameResolver savedResolver = (NameResolver)appMap.get(_SERVICES_RESOLVER_KEY);
    if (savedResolver != null)
      return savedResolver;

    List<NameResolver> resolvers = ClassLoaderUtils.getServices(
                                      _NAME_RESOLVER_CLASS_NAME);

    for (NameResolver customNameResolver : resolvers)
    {
      InputStreamProvider provider = null;
      try
      {
        provider = customNameResolver.getProvider(name);
      }
      catch (IOException e)
      {
        // Log fine message. Try the next factory to get a provider
        if (_LOG.isFine())
          _LOG.fine(_SERVICES_RESOLVER_IOEXCEPTION_MSG);       
      }
      // Found a customNameResolver with a provider. So store it away and return it from the method.
      if (provider != null)
      {
        appMap.put(_SERVICES_RESOLVER_KEY, customNameResolver);
        return customNameResolver;
      }
    }
    
    return null;

  }

  // A subclass of URLInputStreamProvider which never checks for
  // modifications
  private static class StaticURLInputStreamProvider
    extends URLInputStreamProvider
  {
    public StaticURLInputStreamProvider(URL url)
    {
      super(url);
    }

    @Override
    public boolean hasSourceChanged()
    {
      return false;
    }
  }

  // for META-INF\services\org.apache.myfaces.trinidad.share.io.NameResolver
  // In this file they will have a line like "org.mycompany.io.MyNameResolverImpl"
  static private final String _NAME_RESOLVER_CLASS_NAME =
    NameResolver.class.getName();

  // Error messages
  private static final String _STYLES_DIR_ERROR =
    "Could not locate the Trinidad styles directory."
    + "Please be sure that the Trinidad installable resources are installed.";
  
  private static final String _SERVICES_RESOLVER_IOEXCEPTION_MSG =
    "IOException when calling the META-INF/services NameResolver's getProvider method. " +
    "Trying next nameResolver.";

  private static final String _SERVICES_RESOLVER_KEY =
    "org.apache.myfaces.trinidadinternal.skin.SERVICES_RESOLVER_KEY";  

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StyleSheetEntry.class);
  
  private String              _name;
  private StyleSheetDocument  _document;

}
