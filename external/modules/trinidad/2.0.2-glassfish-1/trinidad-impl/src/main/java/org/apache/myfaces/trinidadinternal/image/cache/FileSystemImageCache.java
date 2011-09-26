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
package org.apache.myfaces.trinidadinternal.image.cache;

import java.awt.Color;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.color.CMMException;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;

import java.util.Hashtable;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProvider;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.image.ImageProviderResponse;
import org.apache.myfaces.trinidadinternal.image.ImageRenderer;
import org.apache.myfaces.trinidadinternal.image.ImageType;
import org.apache.myfaces.trinidadinternal.image.ImageTypeManager;
import org.apache.myfaces.trinidadinternal.image.encode.ImageEncoder;
import org.apache.myfaces.trinidadinternal.image.encode.ImageEncoderManager;
import org.apache.myfaces.trinidadinternal.image.util.FileUtils;
import org.apache.myfaces.trinidadinternal.image.util.MapArea;
import org.apache.myfaces.trinidadinternal.image.xml.ImageProviderRequestUtils;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.share.xml.XMLProvider;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;
import org.apache.myfaces.trinidadinternal.style.util.GraphicsUtils;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * FileSystemImageCache is an ImageProvider implementation which caches
 * images on the file system.  Since the FileSystemImageCache
 * are fairly expensive objects to create, FileSystemImageCache instances
 * are shared across applications in the same VM.  Clients can access
 * the shared FileSystemImageCache instance for a particular file system
 * cache location via the getSharedCache method.
 *
 * @see org.apache.myfaces.trinidadinternal.image.ImageProvider
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/FileSystemImageCache.java#0 $) $Date: 10-nov-2005.19:06:06 $
 */
public class FileSystemImageCache implements ImageProvider, ImageConstants
{
  static public final String BLOCK_IMAGE_GENERATION =
    "org.apache.myfaces.trinidad.image.BlockImageGeneration";

  /**
   * Returns a shared cache instance.
   * @param realPath The real path of the root directory of the cache.  If the
   *  specified path does not exist and can not be created, and
   *  IllegalArgumentException is thrown.
   * @return Returns an ImageProvider instance which can be used to
   *   obtain cached images.
   */
  public static ImageProvider getSharedCache(
    String realPath
    )
  {
    // First, normalize the path to make sure that lookups don't
    // fail due to insignificant path incosistencies.
    realPath = _getCanonicalPath(realPath);

    ImageProvider cache = _sSharedCaches.get(realPath);

    // If we didn't find a shared cache, create a new cache
    // and cache it in the shared cache cache.  :-)
    if (cache == null)
    {
      // Create the new cache
      cache = new FileSystemImageCache(realPath);

      // Before we save the new cache, make sure another thread hasn't
      // already cached a different instance.  Synchronize to lock up
      // _sSharedCaches.
      synchronized (_sSharedCaches)
      {
        ImageProvider tmp = _sSharedCaches.get(realPath);
        if (tmp != null)
        {
          // Stick with tmp
          cache = tmp;
        }
        else
        {
          _sSharedCaches.put(realPath, cache);
        }
      }
    }

    return cache;
  }

  /**
   * Creates a FileSystemImageCache.  Clients should use getSharedCache()
   * to obtain FileSystemImageCache instances.
   *
   * @param realPath The real path of the root directory of the cache.  If the
   *  specified path does not exist and can not be created, and
   *  IllegalArgumentException is thrown.
   */
  protected FileSystemImageCache(String realPath)
  {
    _realPath = realPath;

    // Make sure cache directory exists
    File f = new File(_realPath);
    if (!f.exists() && !f.mkdirs())
    {
      throw new IllegalArgumentException(_CACHE_DIRECTORY_ERROR + realPath);
    }

    // Initialize our table of per-Locale caches.  Note on table size:
    // We don't expect a single FileSystemImageCache to be serving up images
    // for a huge number of different locales - using a small table size.
     _caches = new ConcurrentHashMap<String, Cache>(19);
  }

  /**
   * Implementation of ImageCache.getImage().
   *
   * @see org.apache.myfaces.trinidadinternal.image.ImageProvider#getImage
   */
  public ImageProviderResponse getImage(
    ImageContext         context,
    ImageProviderRequest request
    )
  {
    // FileSystemImageCache's implementation of ImageProvider.getImage()
    // does three things:
    //
    // 1. Look for the image in the in-memory cache.
    // 2. Look for the image in the file-system cache.  (It may
    //    have been generated by another FileSystemImageCache instance
    //    running in a separate process.)
    // 3. Try to generate a new image.
    //
    // In general, if the image isn't produced by step #3, the request
    // is cached as a "miss", so that future lookups for the same request
    // will return quickly.

    try
    {
      // Get the ImageType for this request
      ImageType type = _getImageType(context, request);
      assert (type != null);

      // Get the in-memory Cache for this request
      Cache cache = _getCache(context, type);
      assert (cache != null);

      // Convert the ImageProviderRequest into a Cache key.
      Object key = null;

      // If the ImageProviderRequest object is a CacheKey instance,
      // we just use that.  Otherwise, we have to use a CacheKeyFactory
      // to convert the requested properties into a key
      if (request instanceof CacheKey)
        key = request;
      else
        key = _getCacheKey(context, type, request);

      assert (key != null);

      // Check the in-memory Cache for a matching image
      CacheEntry entry = _getImageFromCache(context, cache, key,
                                            request);

      // If we've got a match, return it
      if (entry != null)
      {
        if (entry == _MISS_ENTRY)
          return null;

        if (entry == _RETRY_ENTRY)
        {
          // _RETRY_ENTRYs are used to indicate that a problem
          // occurred when image generation was last attempted,
          // possibly due to a network error with the Tecate
          // Servlet.  Check to see if we should retry again
          // now.  If so, just keep going - otherwise, the request
          // is considered a miss.
          if (!_shouldRetry(context))
            return null;
        }
        else
        {
          return entry;
        }
      }

      // Next, check for a match on the file system.  A matching image
      // may have been generated by another FileSystemImageCache instance
      // running in a different process.  We need the request properties
      // to do this, in order to produce the target file name.
      Map<Object, Object> properties = _getFilteredProperties(context, type, request);

      // If we don't have any properties, we can't render.
      if (properties == null)
      {
        // Cache the miss.
        _putMissEntry(context, type, request, cache, key);
        return null;
      }

      // Now, check the file system
      entry = _getImageFromFileSystem(context, type, cache, key, properties);

      // If we've got a match, return it
      if (entry != null)
      {
        if (entry == _MISS_ENTRY)
          return null;

        // If we've got a retry entry at this point, let's retry it!
        if (entry != _RETRY_ENTRY)
          return entry;
      }

      // Finally, try generating a new image to satistfy the request
      return _generateImage(context,
                            type,
                            request,
                            cache,
                            key,
                            properties);
    }
    catch (CacheException e)
    {
      _LOG.warning(e);
    }

    return null;
  }

  // Loads metadata from file system into a Cache object
  private void _loadCache(
    ImageContext context,
    Cache        cache,
    boolean      localized
    ) throws CacheException
  {
    // Check to make sure locale-specific cache directory exists
    String localeString = _getLocaleString(context);

    // Real path of the root of the locale-specific cache
    String realPath = _realPath + File.separatorChar;

    if (localized)
      realPath += (localeString + File.separatorChar);

    File directory = new File(realPath);
    if (!directory.exists() && (!directory.mkdir()))
      _error(_CACHE_DIRECTORY_ERROR + realPath);

    // Create the cache and load metadata from imx files
    if (_LOG.isFine())
      _LOG.fine("Initializing image cache: " + realPath + " ...");

    String files[] = directory.list(IMXFilter.getInstance());
    if (files == null)
      return;

    // We generate the URI for each image in this buffer
    long lastLog = 0;

    // Read in each metadata file and create a cache entry
    for (int i=0; i<files.length; i++)
    {
      long current = System.currentTimeMillis();
      if ((current - lastLog) > 5000)
      {
        if (_LOG.isFine())
          _LOG.fine("Loading image " + i + " of " + files.length +
                    " from image cache: " + realPath);
        lastLog = current;
      }

      String imxName = files[i];
      String imxPath = realPath + imxName;

      // Create a parser to use for parsing IMX files
      XMLProvider parser = _getXMLProvider(context);
      assert (parser != null);

      try
      {
        _loadImage(context, cache, new File(imxPath), parser);
      }
      catch (CacheException e)
      {
        _LOG.warning(e);
      }
    }

    if (_LOG.isFine())
      _LOG.fine("Finished initializing image cache: " + realPath);
  }

  // Returns a fully loaded cache for the specified Locale
  private Cache _getCache(ImageContext context, ImageType type)
    throws CacheException
  {
    boolean localized = _isTypeLocalized(type);
    String language = _getLocaleString(context);
    Cache cache = null;

    if (localized)
      cache = _caches.get(language);
    else
      cache = _globalCache;

    if (cache != null)
      return cache;

    // Create a new cache.
    // Note - don't bother synchronizing while cache is being created,
    // this may take a while.
    cache = new Cache();

    if (localized)
    {
      // Store off our new cache in the caches table
      // Make sure some other thread hasn't already loaded
      // the same cache
      if (!_caches.containsKey(language))
      {
        // Install the empty Cache object into the _caches table before
        // loading so that other threads don't have to wait for the cache
        // to finish loading files from the file system.  If a second
        // thread comes through before the Cache finishes loading, the
        // second thread will find any images it needs on the file system
        // and load them concurrently.
        _caches.put(language, cache);
        _loadCache(context, cache, localized);
      }
      else
      {
        cache = _caches.get(language);
      }
    }
    else
    {
      if (_globalCache == null)
      {
        // Install the empty Cache object before loading so that other
        // threads don't have to wait for the cache to finish loading files
        // from the file system.
        _globalCache = cache;
        _loadCache(context, cache, localized);
      }
      else
      {
        cache = _globalCache;
      }
    }

    if (cache == null)
    {
      String message = _CREATE_CACHE_ERROR + _realPath;
      if (localized)
        message += "/" + language;

      _error(message);
    }

    return cache;
  }

  // Looks up an image in the in-memory cache
  private CacheEntry _getImageFromCache(
    ImageContext context,
    Cache        cache,
    Object       key,
    ImageProviderRequest request
    ) throws CacheException
  {
    // Check the Cache
    CacheEntry entry = cache.get(context, key);

    // If the image hasn't been generated yet, return null
    if ((entry == null) || (entry == _MISS_ENTRY) || (entry == _RETRY_ENTRY))
      return entry;

    // If the image has been generated, make sure it still exists
    // For performance reasons, we avoid checking every time.
    if (_checkModified(context))
    {
      long time = System.currentTimeMillis();

      if (time > entry.getLastChecked() + _LAST_CHECK_INTERVAL)
      {
        if (!_imageExists(entry.getImageURI()))
        {
          cache.remove(context, key, entry);
          return null;
        }

        // Also checked to see if the image is still valid
        if (!entry.isValid(context, request))
        {
          cache.remove(context, key, entry);

          // We need to remove the image file too
          _removeImageFromFileSystem(entry);

          return null;
        }

        entry.setLastChecked(time);
      }
    }

    return entry;
  }

  // Loads potential matching images from the file system.  If a match
  // is found, the corresponding CacheEntry is returned.
  private CacheEntry _getImageFromFileSystem(
    ImageContext         context,
    ImageType            type,
    Cache                cache,
    Object               key,
    Map<Object, Object>  properties
    ) throws CacheException
  {
    // We identify potential matches on the file system by checking
    // files with names that match the name that we would use when
    // saving the image generated by this request.  The assumption here
    // is that other FileSystemImageCache instances which write to this
    // cache directory use the same naming conventions as this instance.
    // So, if a matching image has been generated by another
    // FileSystemImageCache instance in another process, that image should
    // have the same base name as the same image generated by this instance.

    // Get the name that we would use for this image
    String name = _getFileName(context, type, properties);
    assert (name != null);

    // We use an XMLProvider to parse the IMX file.  However, we don't
    // load the XMLProvider until we've actually got something to parse.
    XMLProvider parser = null;

    // Load all images which match the base name
    while (true)
    {
      // Get the next unique name for this base name
      String uniqueName = cache.getUniqueName(name);

      // Convert the name into the file system path of the IMX file
      String path = _getRealPath(context, type, uniqueName, __IMX_EXTENSION);
      File file = new File(path);

      // If the IMX file doesn't exist, stop checking for matches
      if (!file.exists())
      {
        // We free up the unique name that we obtained from the Cache
        // so that it can be reused.
        cache.releaseUniqueName(uniqueName);
        return null;
      }

      // Try loading the image into the in-memory cache
      if (parser == null)
      {
        parser = _getXMLProvider(context);
        assert (parser != null);
      }

      Object newKey = null;

      try
      {
        newKey = _loadImage(context, cache, file, parser);
      }
      catch (CacheException e)
      {
         // If we couldn't load the image, just continue on...
        _LOG.warning(e);
      }

      // If we've got a match, return the corresponding entry.
      // Otherwise, move on to the next unique name...
      if (key.equals(newKey))
        return cache.get(context, key);
    }
  }

  // Generates an image and stores it in the cache.  Returns the CacheEntry
  // for the generated image, or null if the image wasn't generated.
  private CacheEntry _generateImage(
    ImageContext context,
    ImageType    type,
    ImageProviderRequest request,
    Cache        cache,
    Object       key,
    Map<Object, Object> properties
    )
    throws CacheException
  {
    // Regardless of whether renering is servlet-based or local,
    // rendering produces a status code and a set of response properties.
    // The image is returned in the response properties Map as an
    // array of bytes, using the _IMAGE_DATA_KEY property.

    // The response properties Map must be large enough to hold:
    // 1. WIDTH_RESPONSE_KEY
    // 2. HEIGHT_RESPONSE_KEY
    // 3. IMAGE_MAP_AREAS_RESPONSE_KEY
    ArrayMap<Object, Object> responseProperties = 
      new ArrayMap<Object, Object>(3);
    
    byte[] imageData = null;

    try
    {
      // Render the image locally
      imageData = _renderImageLocal(context,
                                    type,
                                    properties,
                                    responseProperties);
    }
    catch (CMMException e)
    {
      // We have seen seemingly random CMMExceptions generated
      // at various points during image rendering.  Rather than
      // allow these unchecked exceptions to propagate up and
      // out of Tecate, let's catch and log these here.  That way,
      // instead of terminating the page render with a CMMException,
      // the page will be allowed to continue rendering, possibly
      // with alternate content for images which failed to generate.
      _LOG.warning(e);
    }

    if (imageData == null)
    {
      // Cache the request as a miss, so we don't check again.
      _putMissEntry(context, type, request, cache, key);
      return null;
    }

    // Before we start writing to the file system, we need to make sure
    // that the cache directory still exists.  We could just do this up
    // front, but File.exists() does have some overhead, so we wait until
    // we know that we are actually going to have to write.
    cache = _checkCacheExists(context, type);
    assert (cache != null);

    // One more time before we write, make sure the image hasn't shown
    // up some how in the meantime...
    CacheEntry entry = _getImageFromCache(context, cache, key, request);
    if (entry == null)
      entry = _getImageFromFileSystem(context, type, cache, key, properties);

    if (entry != null)
    {
      if ((entry != _MISS_ENTRY) && (entry != _RETRY_ENTRY))
        return entry;
    }

    // Get the base name for the generated file
    String name = _getFileName(context, type, properties);
    assert (name != null);

    // Make sure the name is unique
    String uniqueName = cache.getUniqueName(name);
    assert (uniqueName != null);

    // Write the image and the IMX file
    File imxFile = _writeImageMetadataFile(context,
                                       type,
                                       uniqueName,
                                       properties,
                                       responseProperties);

    // If we didn't generate the IMX file successfully, don't bother
    // trying to save the image.
    if (imxFile == null)
      return null;

    File imageFile = _writeImageFile(context,
                                     type,
                                     uniqueName,
                                     imageData,
                                     properties);

    // If the write didn't succeed, blow away the IMX file and
    // return null.  We'll try again next time...
    if (imageFile == null)
    {
      imxFile.delete();
      return null;
    }

    // Put the succesful cache entry in the in-memory cache and return it.
    return _putCachedImage(context,
                           type,
                           request,
                           cache,
                           key,
                           uniqueName,
                           properties,
                           responseProperties);
  }

  // Loads an image from the file system into the cache.  Returns the
  // key used to cache the image, or null if the image could not be
  // loaded.  Returns the key used to cache the image, or null if image
  // couldn't be loaded.
  private Object _loadImage(
    ImageContext context,
    Cache        cache,
    File         imxFile,
    XMLProvider  parser
    )
    throws CacheException
  {
    String imxPath = imxFile.getPath();
    String imxName = imxFile.getName();

    // Get an InputStream for the IMX file
    InputStream in = null;

    try
    {
      in = new FileInputStream(imxPath);
    }
    catch (FileNotFoundException fnfe)
    {
      _error (_METADATA_FILE_ERROR, fnfe);
    }

    // If we made it this far, we should have an InputStream
    assert (in != null);

    // Wrap up the stream in a UTF8 Reader
    Reader reader = _getUTF8Reader(in);

    // Wrap up the Reader in an InputSource
    InputSource source = new InputSource(reader);
    source.setSystemId(imxPath);

    ImageProviderRequest request = null;

    try
    {
      request =
        ImageProviderRequestUtils.createImageProviderRequest(context,
                                                             parser,
                                                             source);
    }
    catch (SAXException e)
    {
      _error(_XML_DECODING_ERROR + imxPath, e);
    }
    catch (IOException e)
    {
      _error(_XML_DECODING_ERROR + imxPath, e);
    }
    finally
    {
      try { in.close(); } catch (IOException e) { _error(e); }
    }

    if (request == null)
      return null;

    // Get the ImageType from the request
    ImageType type = _getImageType(context, request);
    assert (type != null);

      // Get the properties from the request
    Map<Object, Object> properties = request.getRenderProperties(context);
    assert (properties != null);

    // Get the base name for the URI (minus the extension)
    int dotIndex = imxName.lastIndexOf('.');
    String baseName = (dotIndex == -1) ?
                        imxName : imxName.substring(0, dotIndex);

    // Generate the new URI
    StringBuffer uriBuffer = new StringBuffer(imxName.length() + 3);
    if (_isTypeLocalized(type))
    {
      uriBuffer.append(_getLocaleString(context));
      uriBuffer.append(_URI_DELIMITER);
    }

    uriBuffer.append(baseName);

    // Replace ".imx" with our image extension
    String extension = _getImageEncodingExtension(context, properties);
    assert (extension != null);

    uriBuffer.append(extension);

    String uri = uriBuffer.toString();

    // Make sure the required files exist - if not don't add it to cache
    if (!_imageExists(uri))
      return null;

    // Create the cache key
    CacheKeyFactory keyFactory = _getCacheKeyFactory(type);
    Object key = keyFactory.getCacheKey(context, properties);

    // Create the cache entry
    CacheEntry entry = _createCacheEntry(context, type, uri, properties);

    // Cache it
    cache.put(context, key, entry);

    return key;
  }

  // Stores the specified image in the cache
  private CacheEntry _putCachedImage(
    ImageContext context,
    ImageType    type,
    ImageProviderRequest request,
    Cache        cache,
    Object       key,
    String       name,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties
    ) throws CacheException
  {
    // Make sure that we've got a new CacheKey.  We wouldn't want to
    // use a CacheKey provided by the client - as the client might be
    // reusing CacheKey instances.
    if (request == key)
    {
      key = _getCacheKey(context, type, request);
      assert (key != null);
    }

    // Generate the URI for this image
    boolean localized = _isTypeLocalized(type);
    String language = _getLocaleString(context);
    assert (language != null);

    String extension = _getImageEncodingExtension(context, properties);
    assert (extension != null);

    int length = name.length() + extension.length();
    if (localized)
      length += language.length();

    StringBuffer buffer = new StringBuffer(length);

    if (localized)
    {
      buffer.append(language);
      buffer.append("/");
    }

    buffer.append(name);
    buffer.append(extension);

    String uri = buffer.toString();

    // Create the cache entry
    CacheEntry entry = _createCacheEntry(context, type, uri,
                                         responseProperties);

    cache.put(context, key, entry);

    return entry;
  }

  // Caches a miss
  private void _putMissEntry(
    ImageContext         context,
    ImageType            type,
    ImageProviderRequest request,
    Cache                cache,
    Object               key
    ) throws CacheException
  {
    // Make sure that we've got a new CacheKey.  We wouldn't want to
    // use a CacheKey provided by the client - as the client might be
    // reusing CacheKey instances.
    if (request == key)
    {
      key = _getCacheKey(context, type, request);
      assert (key != null);
    }

    cache.put(context, key, _MISS_ENTRY);
  }

  // Removes the image represented by the CacheEntry from
  // the file system cache
  private void _removeImageFromFileSystem(
    CacheEntry entry
    )
  {
    String uri = entry.getImageURI();
    String imagePath = _getRealPath(uri);
    File imageFile = new File(imagePath);
    imageFile.delete();

    // Also remove the IMX file
    String encoding = entry.getEncoding();
    ImageEncoderManager manager =
      ImageEncoderManager.getDefaultImageEncoderManager();
    String extension = manager.getImageExtension(encoding);

    // Build up the path for the IMX file
    String imxPath = imagePath.substring(0,
                       imagePath.length() - extension.length());

    File imxFile = new File(imxPath + __IMX_EXTENSION);
    imxFile.delete();
  }

  // Render the requested image locally.
  // Returns the image data as a byte array.
  private byte[] _renderImageLocal(
    ImageContext context,
    ImageType    type,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties
    ) throws CacheException
  {
    Configuration config = context.getConfiguration();
    // See if we don't want any image generation (which is basically
    // just for the renderkit test).  Always generate a width and a height,
    // and generate an area map if needed
    if (Boolean.TRUE.equals(config.getProperty(BLOCK_IMAGE_GENERATION)))
    {
      responseProperties.put(WIDTH_RESPONSE_KEY, _TEST_WIDTH);
      responseProperties.put(HEIGHT_RESPONSE_KEY, _TEST_HEIGHT);
      Object o = properties.get(TABS_KEY);
      if (o != null)
      {
        int length = ((Object[]) o).length;
        MapArea[] areas = new MapArea[length];
        for (int i = 0; i < length; i++)
        {
          areas[i] = new MapArea(new Rectangle(i, 0, 1, 1));
        }

        responseProperties.put(IMAGE_MAP_AREAS_RESPONSE_KEY, areas);
      }

      return new byte[0];
    }

    // Before we do any rendering check for Configuration.HEADLESS
    if (Boolean.TRUE.equals(config.getProperty(Configuration.HEADLESS)) ||
        !GraphicsUtils.isGraphicalEnvironment())
    {
      // We're special casing colorized icons so that we can generate
      // them even if we don't have an X server
      if (TECATE_NAMESPACE.equals(type.getNamespaceURI()) &&
          COLORIZED_ICON_NAME.equals(type.getLocalName()))
      {
        return _readColorizedIconData(context,
                                      properties,
                                      responseProperties);
      }

      return null;
    }

    // Get the ImageRenderer to use for the requested type
    ImageRenderer renderer = _getImageRenderer(type);
    assert (renderer != null);

    // Render the image
    Image image = renderer.renderImage(context,
                                       properties,
                                       responseProperties);

    // If we didn't get an Image, we're done.
    if (image == null)
      return null;

    // Convert the generated image to a byte array
    ByteArrayOutputStream out = new ByteArrayOutputStream();

    try
    {
      ImageEncoder encoder = _getImageEncoder(context, properties);
      assert (encoder != null);

      encoder.encodeImage(image, out);
    }
    catch (IOException e)
    {
      _error(e);
    }
    finally
    {
      image.flush();
    }

    return out.toByteArray();
  }



  // Read in image data from an InputStream
  private byte[] _readImageData(
    InputStream  in
    ) throws IOException, CacheException
  {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    byte[] buffer = new byte[1024];
    int length = 0;

    while ((length = (in.read(buffer))) >= 0)
      out.write(buffer, 0, length);

    return out.toByteArray();
  }

  // Reads in source icon data from the specified InputStreamProvider
  private byte[] _readSourceIconData(
    InputStreamProvider provider
    ) throws IOException, CacheException
  {
    InputStream in = null;

    try
    {
      in = provider.openInputStream();
    }
    catch (IOException e)
    {
      // If we are unable to open the InputStream at all,
      // consider it a miss.
      return null;
    }

    byte[] iconData = null;

    try
    {
      iconData = _readImageData(in);
    }
    finally
    {
      in.close();
    }

    return iconData;
  }



  // Tests whether it is time to retry generating the image
  // for a particular request
  private boolean _shouldRetry(ImageContext context)
  {
    // Back when we supported an external URL for generation,
    // this might return false
    return true;
  }

  // Writes an image data to the file system
  private File _writeImageFile(
    ImageContext context,
    ImageType    type,
    String       name,
    byte[]       data,
    Map<Object, Object> properties
    ) throws CacheException
  {
    // Derive the full path of the image file
    String extension = _getImageEncodingExtension(context, properties);
    String path = _getRealPath(context, type, name, extension);
    File file = new File(path);

    try
    {
      OutputStream out = new FileOutputStream(file);
      out.write(data);
      out.close();
    }
    catch (IOException e)
    {
      _error(e);
    }

    return file;
  }


  // Writes an image map to the file system
  private File _writeImageMetadataFile(
    ImageContext context,
    ImageType    type,
    String       name,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties
    )
    throws CacheException
  {
    // Derive the full path of the IMX file
    String path = _getRealPath(context, type, name, __IMX_EXTENSION);
    File file = new File(path);

    PrintWriter writer = null;

    try
    {
      // One last time... Check to see if the File already exists.
      // It may have been created by another process.  If the file doesn't
      // exist, we attempt to create it by calling File.createNewFile(),
      // which should guarantee us that we are creating a unique file.
      if (file.exists() || !file.createNewFile())
      {
        // At this point, we just give up until next time.
        return null;
      }

      writer = new PrintWriter(FileUtils.getUTF8Writer(path));
    }
    catch (IOException e)
    {
      _error(e);
    }

    try
    {
      _writeImageProviderRequest(context,
                                 type,
                                 properties,
                                 responseProperties,
                                 writer);
    }
    finally
    {
      writer.flush();
      writer.close();

      // Check for errors
      if (writer.checkError())
      {
        if (file.exists())
          file.delete();

        _error(_XML_ENCODING_ERROR + path);
      }
    }

    return file;
  }

  // Writes out the ImageProviderRequest request
  private void _writeImageProviderRequest(
    ImageContext context,
    ImageType    type,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties,
    PrintWriter  writer
    ) throws CacheException
  {
    // Write out the XML declaration - we always use UTF-8
    writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");

    try
    {
      ImageProviderRequestUtils.encodeImageProviderRequest(context,
                             type.getNamespaceURI(),
                             type.getLocalName(),
                             type,
                             properties,
                             responseProperties,
                             writer);
    }
    catch (IllegalArgumentException e)
    {
      throw new CacheException(e);
    }
  }

  //
  // Everything from here on out is utility methods used to
  // simplify the above code
  //

  // Checks whether the cache still exists.  Called before writing to
  // avoid writing to a cache directory which has been deleted.  If the
  // cache directory has been deleted, it is re-created and a new, empty
  // Cache obect is returned.
  private Cache _checkCacheExists(
    ImageContext context,
    ImageType    type
    ) throws CacheException
  {
    boolean localized = _isTypeLocalized(type);
    String language = _getLocaleString(context);
    String directoryPath = _getRealPath(localized ? language : "");
    File directory = new File(directoryPath);

    if (!directory.exists())
    {
      // If the directory no longer exists, dump the corresponding
      // Cache instance, if it exists and create a new directory.
      if (localized)
      {
        synchronized (_caches)
        {
          if (_caches.containsKey(language))
          {
            _caches.remove(language);
          }

          // While we are at it, check to see if the global cache
          // directory exists - and if not, blow away the global
          // Cache.  We do this, since the call to mkdirs() will
          // recreate both the locale-specific cache directory, as
          // well as the global cache directory.  If we didn't blow
          // away the global cache, we'll get an error logged for every
          // missing file in the global cache.
          File parent = directory.getParentFile();
          if ((parent != null) && (!parent.exists()))
          {
            synchronized (this)
            {
              _globalCache = null;
            }
          }
        }
      }
      else
      {
        synchronized (this)
        {
          _globalCache = null;
        }
      }

      // Try creating the directory now.  Subsequent calls to _getCache()
      // will create the new Cache object.
      directory.mkdirs();
    }

    // Generate a name for the image
    return _getCache(context, type);
  }

  // Tests whether or not to check for modifications
  private boolean _checkModified(ImageContext context)
  {
    // check configuration once and cache whether or not
    // to check if the files exist
    if (_configNotChecked)
    {
      _checkModified = true;
      _configNotChecked = false;
    }

    return _checkModified;
  }


  // Creates a CacheEntry with the specified uri and properties
  private CacheEntry _createCacheEntry(
    ImageContext context,
    ImageType    type,
    String       uri,
    Map<Object, Object> properties)
  {
    // Get the cache entry
    int width = _getIntSize(properties, WIDTH_RESPONSE_KEY);
    int height = _getIntSize(properties, HEIGHT_RESPONSE_KEY);
    MapArea[] areas = (MapArea[])properties.get(IMAGE_MAP_AREAS_RESPONSE_KEY);
    String encoding = _getImageEncoding(context, properties);

    if (areas == null)
    {
      Object checkSource = type.getProperty(ImageType.CHECK_SOURCE_PROPERTY);
      if (Boolean.TRUE.equals(checkSource))
        return new SourceCheckingCacheEntry(uri, width, height, encoding);

      return new CacheEntry(uri, width, height, encoding);
    }

    return new MapCacheEntry(uri, width, height, areas, encoding);
  }

  // Throws a CacheException with a message
  private void _error(String message)
    throws CacheException
  {
    throw new CacheException(message);
  }

  // Throws a CacheException with a Throwable
  private void _error(Throwable t)
    throws CacheException
  {
    throw new CacheException(t);
  }

  // Throws a CacheException with a message and Throwable
  private void _error(String message, Throwable t)
    throws CacheException
  {
    throw new CacheException(message, t);
  }

  // Returns the key for the request
  private Object _getCacheKey(
    ImageContext context,
    ImageType type,
    ImageProviderRequest request
    )
    throws CacheException
  {
    // Get a key based on the requested properties
    CacheKeyFactory keyFactory = _getCacheKeyFactory(type);
    assert (keyFactory != null);

    Object key = keyFactory.getCacheKey(context,
                                        request.getRenderProperties(context));

    if (key == null)
      _error(_CACHE_KEY_ERROR + type);

    return key;
  }

  // Returns the CacheKeyFactory for the specified type
  private CacheKeyFactory _getCacheKeyFactory(ImageType type)
    throws CacheException
  {
    CacheKeyFactory keyFactory = (CacheKeyFactory)
      type.getProperty(CacheKeyFactory.CACHE_KEY_FACTORY_PROPERTY);

    if (keyFactory == null)
      _error(_CACHE_KEY_FACTORY_ERROR + type);

    return keyFactory;
  }

  // Utility method for getting canonical paths.  File.getCanonicalPath()
  // can be slow, so we cache canonical paths to avoid calling
  // getCanonicalPath() each time we need to get a shared FileSystemImageCache
  // instance.
  private static String _getCanonicalPath(String path)
  {
    String canonicalPath = _sCanonicalPaths.get(path);
    if (canonicalPath != null)
      return canonicalPath;

    File file = new File(path);

    try
    {
      canonicalPath =  file.getCanonicalPath();
    }
    catch (IOException e)
    {
      throw new IllegalArgumentException(_CACHE_PATH_ERROR + path);
    }

    if (canonicalPath != null)
      _sCanonicalPaths.put(path, canonicalPath);

    return canonicalPath;
  }

  // Gets the base file name for the requested image
  private String _getFileName(
    ImageContext context,
    ImageType    type,
    Map<Object, Object> properties
    )
    throws CacheException
  {
    // First, get the base name for the requested image
    NameProvider nameProvider = (NameProvider)
      type.getProperty(NameProvider.NAME_PROVIDER_PROPERTY);

    if (nameProvider == null)
      _error(_NAME_PROVIDER_ERROR + type);

    String name = nameProvider.getName(context, properties);

    if (name == null)
      _error(_NAME_PROVIDING_ERROR + type);

    return name;
  }

  // Returns the complete set of properties needed to render/encode
  // the requested image
  private Map<Object, Object> _getFilteredProperties(
    ImageContext context,
    ImageType type,
    ImageProviderRequest request
    )
  {
    Map<Object, Object> properties = request.getRenderProperties(context);
    if (properties == null)
      return null;

    // Make sure the encoding is specified
    if (properties.get(ENCODING_TYPE_KEY) == null)
    {
      String encoding = _getImageEncoding(context, properties);
      properties.put(ENCODING_TYPE_KEY, encoding);
    }

    PropertiesFilter filter = (PropertiesFilter)
      type.getProperty(PropertiesFilter.PROPERTIES_FILTER_PROPERTY);

    if (filter != null)
      return filter.filterProperties(context, properties);

    return properties;
  }

  // Returns the ImageEncoder to use for the requested image
  private ImageEncoder _getImageEncoder(
    ImageContext context,
    Map<Object, Object> properties
    ) throws CacheException
  {
    String encoding = _getImageEncoding(context, properties);

    // _getImageEncoding() always returns non-null encoding
    assert (encoding != null);

    ImageEncoderManager manager =
      ImageEncoderManager.getDefaultImageEncoderManager();
    ImageEncoder encoder = manager.getImageEncoder(encoding);

    if (encoder == null)
      _error(_IMAGE_ENCODER_ERROR + encoding);

    return encoder;
  }

  // Returns the encoding to use for images generated
  // using the specified context
  private String _getImageEncoding(
      ImageContext context, 
      Map<Object, Object> properties)
  {
    String encoding = (String)properties.get(ENCODING_TYPE_KEY);
    if (encoding != null)
      return encoding;

    // Pick an encoding based on the Configuration/Agent
    Configuration config = context.getConfiguration();
    TrinidadAgent agent = context.getAgent();
    //encodings was part of old Agent capabilities, not in new Agent Caps. So replaced
    //Object encodingsObj = agent.getCapability(AdfFacesAgent.CAP_IMAGE_ENCODINGS);
    //int encodings = ((encodingsObj == null)
    //                 ? (AdfFacesAgent.IMAGE_ENCODINGS_CAP_GIF |
    //                    AdfFacesAgent.IMAGE_ENCODINGS_CAP_PNG)
    //                 : ((Number) encodingsObj).intValue());

    // If GIF is enabled and supported, use it
    //if (((encodings & AdfFacesAgent.IMAGE_ENCODINGS_CAP_GIF) != 0) &&
    //      !Boolean.FALSE.equals(config.getProperty(_GIF_ENABLED)))
    //{
    //  return ImageEncoderManager.GIF_TYPE;
    //}
    if ((agent.getCapability(TrinidadAgent.CAP_GIF_TYPE_IMAGE) == Boolean.TRUE) &&
            !Boolean.FALSE.equals(config.getProperty(_GIF_ENABLED)))
    {
      return ImageEncoderManager.GIF_TYPE;
    }

    //encodings was part of old Agent capabilities, not in new Agent Caps. So replaced
    //if ((encodings & AdfFacesAgent.IMAGE_ENCODINGS_CAP_PNG) != 0)
    //{
    //  return ImageEncoderManager.PNG_TYPE;
    //}
    if (agent.getCapability(TrinidadAgent.CAP_PNG_TYPE_IMAGE) == Boolean.TRUE)
    {
      return ImageEncoderManager.PNG_TYPE;
    }


    // There must be some supported encoding!
    assert false;

    // What else can we do?
    return ImageEncoderManager.PNG_TYPE;
  }

  // Returns the extension for the current image encoding
  private String _getImageEncodingExtension(
    ImageContext context,
    Map<Object, Object> properties
    ) throws CacheException
  {
    String encoding = _getImageEncoding(context, properties);

    // _getImageEncoding() always returns non-null encoding
    assert (encoding != null);

    ImageEncoderManager manager =
      ImageEncoderManager.getDefaultImageEncoderManager();

    String extension = manager.getImageExtension(encoding);

    if (extension == null)
      _error(_IMAGE_ENCODING_EXTENSION_ERROR + encoding);

    return extension;
  }

  // Returns the ImageRenderer for the specified type
  private ImageRenderer _getImageRenderer(
    ImageType type
    )
    throws CacheException
  {
    ImageRenderer renderer = (ImageRenderer)
      type.getProperty(ImageType.IMAGE_RENDERER_PROPERTY);

    if (renderer == null)
      _error(_IMAGE_RENDERER_ERROR + type);

    return renderer;
  }

  // Gets an ImageType
  private ImageType _getImageType(
    ImageContext         context,
    ImageProviderRequest request
    )
    throws CacheException
  {
    String namespace = request.getNamespaceURI();
    String name = request.getLocalName();

    ImageTypeManager manager = CacheUtils.getImageTypeManager(context);
    assert (manager != null);

    ImageType type = manager.getImageType(namespace, name);

    if (type == null)
      _error(_IMAGE_TYPE_ERROR + namespace + ", " + name);

    return type;
  }

  // Returns an int from the specified Map
  private int _getIntSize(
      Map<Object, Object> properties, 
      Object key)
  {
    Integer value = (Integer)properties.get(key);

    if (value == null)
      return ImageProviderResponse.UNKNOWN_SIZE;

    return value.intValue();
  }

  // Returns string for the locale specified by the ImageContext
  private String _getLocaleString(ImageContext context)
  {
    Locale locale = context.getLocaleContext().getTranslationLocale();
    String language = locale.getLanguage();

    // Chinese is a special case.  We keep Simplified Chinese and
    // Traditional Chinese images in separate directories in order
    // to avoid collisions when the "Dialog" font is used.  In general,
    // this shouldn't be a problem - as locale-specific Albany fonts should
    // be used in the future - which should be sufficient to distinguish
    // between Simplified/Traditional Chinese images in the same directory.
    if (!_CHINESE_LANGUAGE.equals(language))
      return language;

    return _isSimplifiedChinese(locale.getCountry()) ?
             _SIMPLIFIED_CHINESE_DIRECTORY :
             _TRADITIONAL_CHINESE_DIRECTORY;
  }

  // Returns the real path for the specified uri
  private String _getRealPath(String uri)
  {
    int length = _realPath.length() + uri.length() + 1;
    StringBuffer buffer = new StringBuffer(length);

    buffer.append(_realPath);
    buffer.append(File.separatorChar);
    buffer.append(uri);

    return buffer.toString();
  }

  // Returns the real path for the specified name
  private String _getRealPath(
    ImageContext context,
    ImageType    type,
    String       name,
    String       extension)
  {
    boolean localized = _isTypeLocalized(type);
    String language = _getLocaleString(context);
    assert (language != null);

    int length = _realPath.length() + name.length() + 1;
    if (localized)
      length += (language.length() + 1);
    if (extension != null)
      length += extension.length();

    StringBuffer buffer = new StringBuffer(length + 1);
    buffer.append(_realPath);
    buffer.append(File.separatorChar);

    // Append the language if it is specified
    if (localized)
    {
      buffer.append(language);
      buffer.append(File.separatorChar);
    }

    buffer.append(name);

    if (extension != null)
      buffer.append(extension);

    return buffer.toString();
  }

  // Converts an InputStream into a UTF8 Reader
  private Reader _getUTF8Reader(InputStream  in)
    throws CacheException
  {
    Reader reader = null;

    try
    {
      reader = new BufferedReader(new InputStreamReader(in, _UTF8));
    }
    catch (UnsupportedEncodingException e)
    {
      _error(e);
    }

    // Reader should never be null at this point
    assert (reader != null);

    return reader;
  }


  // Returns an XMLProvider to use for parsing XML documents
  private XMLProvider _getXMLProvider(ImageContext context)
    throws CacheException
  {
    // Create a parser to use for parsing IMX files
    Configuration config = context.getConfiguration();
    XMLProvider parser = XMLUtils.getXMLProvider(config);

    if (parser == null)
     _error(_XML_PROVIDER_ERROR);

    return parser;
  }

  // Utility method to test whether the image file corresponding to
  // the specified uri actually exists
  private boolean _imageExists(String uri)
  {
    // First, check to see if the image exists
    String path = _getRealPath(uri);
    File file = new File(path);

    return file.exists();
  }

  // Tests whether the specified Chinese region uses Simplified glyphs
  private static boolean _isSimplifiedChinese(String region)
  {
    // Right now, the only Chinese locale that uses Simplified
    // characters is zh_CN.
    return ("CN".equals(region));
  }

  // Tests whether the image type is localized
  private boolean _isTypeLocalized(ImageType type)
  {
    Object localized = type.getProperty(ImageType.LOCALIZED_PROPERTY);

    return !Boolean.FALSE.equals(localized);
  }


  // Reads colorized icon image data from the request properties
  private byte[] _readColorizedIconData(
    ImageContext context,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties
    ) throws CacheException
  {
    // Only bother doing this if we're generating GIFs
    Object encoding = _getImageEncoding(context, properties);
    if (!ImageEncoderManager.GIF_TYPE.equals(encoding))
      return null;

    // Only bother doing this if we're in LTR
    if (CacheUtils.getReadingDirection(context, properties) !=
          LocaleUtils.DIRECTION_LEFTTORIGHT)
    {
      // We can't use the source icon because it needs
      // to be flipped to RTL
      return null;
    }

    // Only bother doing this if we're using the default colors
    Color darkColor = (Color)properties.get(DARK_COLOR_KEY);
    if ((darkColor != null) &&
        ((darkColor.getRGB() & 0x00ffffff) != 0x00336699))
    {
      return null;
    }

    Color darkAccentColor = (Color)properties.get(DARK_ACCENT_COLOR_KEY);
    if ((darkAccentColor != null) &&
        ((darkAccentColor.getRGB() & 0x00ffffff) != 0x00d2d8b0))
    {
      return null;
    }

    InputStreamProvider provider = (InputStreamProvider)properties.get(
                                     SOURCE_INPUT_STREAM_PROVIDER_KEY);
    if (provider != null)
    {
      byte[] data = null;;

      try
      {
        data = _readSourceIconData(provider);
      }
      catch (IOException e)
      {
        _error(e);
      }

      if (data != null)
      {
        // Make sure we've got a GIF
        if (data[0] == 'G' && (data[1] == 'I') && (data[2] == 'F'))
        {
          // We've got a GIF - get the width/height
          int width = (data[6] | (data[7] << 8));
          int height = (data[8] | (data[9] << 8));
          responseProperties.put(WIDTH_RESPONSE_KEY, width);
          responseProperties.put(HEIGHT_RESPONSE_KEY, height);

          return data;
        }
      }
    }

    return null;
  }

  // FilenameFilter for .imx files
  private static class IMXFilter implements FilenameFilter
  {
    private IMXFilter() {}

    public static FilenameFilter getInstance()
    {
      if (_sInstance == null)
        _sInstance = new IMXFilter();

      return _sInstance;
    }

    public boolean accept(File dir, String name)
    {
      return name.endsWith(__IMX_EXTENSION);
    }

    private static IMXFilter _sInstance;
  }


  // Cache root directory
  private String _realPath;

  // Hashtable containing caches (ie. Hashtables) hashed by Locale.
  // Each cache stores mappings from PropertiesKeys to the "base name"
  // of the image/metadata/map.
  private ConcurrentHashMap<String, Cache> _caches;

  // The locale-independent cache
  private Cache _globalCache;

  // Table of shared FileSystemImageCaches, hashed by path.
  // Note on table size: We don't expect to have very many FileSystemCaches
  // running in a single VM - table can be small.
  // -= Simon Lessard =-
  // FIXME: The code already take care of synchronization when needed it seems,
  //        using a HashMap should be more performant.
  private static final Hashtable<String, ImageProvider> _sSharedCaches = 
    new Hashtable<String, ImageProvider>(19);

  // Extension for IMX files
  static final String __IMX_EXTENSION = ".imx";

  private static final String _URI_DELIMITER = "/";

  // The default time interval for which we wait before re-checking whether an
  // cache entry's image exists.
  private static final long _LAST_CHECK_INTERVAL = 10000;

  // Whether the config was checked already
  private boolean _configNotChecked = true;

  // Whether to check if files exist
  private boolean _checkModified = true;

  // The CacheEntry used to represent cached misses.
  private static final CacheEntry _MISS_ENTRY = new CacheEntry(null, -1, -1);

  // The CacheEntry used for misses which should be retried
  private static final CacheEntry _RETRY_ENTRY = new CacheEntry(null, -1, -1);

  // Error messages
  private static final String _CACHE_PATH_ERROR =
   "Could not get canonical path for image cache directory ";
  private static final String _CACHE_DIRECTORY_ERROR =
    "Could not create image cache directory ";
  private static final String _XML_ENCODING_ERROR =
    "Error while generating image metadata file ";
  private static final String _CREATE_CACHE_ERROR =
    "Could not create image cache for ";
  private static final String _XML_DECODING_ERROR =
    "Could not decode image metadata from ";
  private static final String _CACHE_KEY_ERROR =
    "Could not create cache key for image type ";
  private static final String _CACHE_KEY_FACTORY_ERROR =
    "No CacheKeyFactory registered for image type ";
  private static final String _METADATA_FILE_ERROR =
    "Could not locate metadata file ";
  private static final String _IMAGE_TYPE_ERROR =
   "No image type registered for ";
  private static final String _IMAGE_RENDERER_ERROR =
    "No ImageRenderer registered for image type ";
  private static final String _NAME_PROVIDER_ERROR =
    "No NameProvider registered for image type ";
  private static final String _NAME_PROVIDING_ERROR =
    "Could not get image file name name for image type ";
  private static final String _IMAGE_ENCODER_ERROR =
    "No ImageEncoder registered for image encoding type ";
  private static final String _IMAGE_ENCODING_EXTENSION_ERROR =
    "No file extension registered for image encoding type ";
  private static final String _XML_PROVIDER_ERROR =
    "Could not load XMLProvider";

  // UTF8 encoding name
  private static String _UTF8 = "UTF8";

  // Some constants related to special handling for Chinese language.
  // Chinese language images are placed in separate directories depending
  // on whether the region uses Simplified or Traditional Chinese
  private static final String _CHINESE_LANGUAGE              = "zh";
  private static final String _SIMPLIFIED_CHINESE_DIRECTORY  = "zhs";
  private static final String _TRADITIONAL_CHINESE_DIRECTORY = "zht";

  private static final Integer _TEST_WIDTH = Integer.valueOf(23);
  private static final Integer _TEST_HEIGHT = Integer.valueOf(32);

  // Configuration property to test whether GIF support is enabled.
  private static final String _GIF_ENABLED = "gifEnabled";

  // -= Simon Lessard =-
  // FIXME: Hashtable is synchronized and more than often inefficient
  //        use a HashMap instead
  private static final Hashtable<String, String> _sCanonicalPaths = 
    new Hashtable<String, String>(19);
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FileSystemImageCache.class);
}
