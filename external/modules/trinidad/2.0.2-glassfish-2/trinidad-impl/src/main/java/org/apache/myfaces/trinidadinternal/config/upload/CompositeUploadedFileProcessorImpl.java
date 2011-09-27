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
package org.apache.myfaces.trinidadinternal.config.upload;


import java.io.File;
import java.io.IOException;
import java.io.Serializable;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Collections;
import java.util.Map;

import javax.portlet.ActionRequest;
import javax.portlet.PortletContext;
import javax.portlet.PortletRequest;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.UploadedFile;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;
import org.apache.myfaces.trinidadinternal.context.external.PortletApplicationMap;
import org.apache.myfaces.trinidadinternal.context.external.PortletInitParameterMap;
import org.apache.myfaces.trinidadinternal.context.external.PortletRequestMap;
import org.apache.myfaces.trinidadinternal.context.external.ServletApplicationMap;
import org.apache.myfaces.trinidadinternal.context.external.ServletInitParameterMap;
import org.apache.myfaces.trinidadinternal.context.external.ServletRequestMap;

/**
 * This is the default implementaion of UploadedFileProcessor. It works with
 * multiple ChainedUploadedFileProcessor declared in trinidad-config.xml by dispatching
 * the lifecycle calls to them in the order of declaration.
 **/
public class CompositeUploadedFileProcessorImpl implements UploadedFileProcessor
{

  public CompositeUploadedFileProcessorImpl()
  {
    chainedProcessors = Collections.emptyList();
  }

  public CompositeUploadedFileProcessorImpl(
          List<? extends UploadedFileProcessor> chainedProcessors)
  {
    this.chainedProcessors = chainedProcessors;
  }

  @Override
  public void init(Object context)
  {
    _init(context);
    for(UploadedFileProcessor processor: chainedProcessors)
    {
      //call other chained processor's init
      processor.init(context);
    }

  }

  @Override
  public UploadedFile processFile(Object request, UploadedFile tempFile) throws IOException
  {
    //NOTE: The following optimization was suggested at one point:
    // Check if there are any processors in the list. If yes then call _processFile
    // else just return tempFile we got. Calling _processFile buffers the entire
    // file content. tempFile arg however is not buffered. If there are no
    // ChainedUploadedFileProcessor in the list, then there is no need to buffer.
    // BUT, the call to getLength() and dispose() on tempFile fails.
    // This is becasue, the impl of these 2 methods are not complete -
    // Bug - https://issues.apache.org/jira/browse/TRINIDAD-1765
    // This bug could not be fixed due to the explanatio in the bug.

    //call _processFile first. This will create the auto buffer that we want.
    UploadedFile original = _processFile(request, tempFile);
    if(chainedProcessors.isEmpty())
    {
      return original;
    }
    else
    {
      List<UploadedFile> files = new ArrayList<UploadedFile>(chainedProcessors.size()+1);
      files.add(original);
      for(UploadedFileProcessor processor: chainedProcessors)
      {
        try
        {
          original = processor.processFile(request, original);
        }
        catch (IOException ioe)
        {
          _LOG.severe(ioe);
          String test = ioe.getLocalizedMessage();
          original = new ErrorFile(ioe.getLocalizedMessage());
          // The chain breaks if one of the chained processor throws an IOException, if the intent
          //  is to allow rest of the processors in chain to process, they could return custom 
          //  UploadedFile instance with length -1 and opaqueData having the failure details.
          break;
        }
        files.add(original);
      }
      //the dispose order should be reverse!
      Collections.reverse(files);
      return new WrappedUploadedFileImpl(original, files);
    }
  }


  /**
   * Wrapper around UploadedFileImpl to listen for dispose() call and dispatch to
   * other UploadedFileImpls returned by other chained processors
   */
  private static class WrappedUploadedFileImpl implements UploadedFile, Serializable
  {

    public WrappedUploadedFileImpl(UploadedFile original, List<UploadedFile> files)
    {
      this.original = original;
      this.files = files;
    }

    public void dispose()
    {
      try
      {
        original.dispose();
      }
      catch(Exception e)
      {
        _LOG.warning("Exception while disposing!", e);
        //log and continue disposing the rest.
      }
      if(!files.isEmpty())
      {
        List<UploadedFile> disposed = new ArrayList<UploadedFile>();
        disposed.add(original);
        for(UploadedFile file: files)
        {
          if(!disposed.contains(file))
          {
            try
            {
              file.dispose();
            }
            catch(Exception e)
            {
              _LOG.warning("Exception while disposing!", e);
              //log and continue disposing the rest.
            }
            disposed.add(file);
          }
        }
      }
    }

    public String getFilename()
    {
      return original.getFilename();
    }

    public String getContentType()
    {
      return original.getContentType();
    }

    public long getLength()
    {
      return original.getLength();
    }

    public Object getOpaqueData()
    {
      return original.getOpaqueData();
    }

    public InputStream getInputStream() throws IOException
    {
      return original.getInputStream();
    }

    private final UploadedFile original;
    private final List<UploadedFile> files;
    private static final long serialVersionUID = 1L;
  }

  private void _init(Object context)
  {
    ContextInfo info;
    if(_PORTLET_CONTEXT_CLASS != null && _PORTLET_CONTEXT_CLASS.isInstance(context))
    {
      info = _getPortletContextInfo(context);
    }
    else
    {
      info = _getServletContextInfo(context);
    }

    //
    // Get MaxMemory and TempDir properties from servlet init params
    //
    if (_maxMemory == -1)
    {
      String maxMemory = info.initParams.get(MAX_MEMORY_PARAM_NAME);
      if (maxMemory != null)
      {
        try
        {
          _maxMemory = Long.parseLong(maxMemory);
        }
        catch (NumberFormatException nfe)
        {
          _maxMemory = _DEFAULT_MAX_MEMORY;
        }
      }
      else
      {
        _maxMemory = _DEFAULT_MAX_MEMORY;
      }
    }

    if (_maxDiskSpace == -1)
    {
      String maxDiskSpace = info.initParams.get(MAX_DISK_SPACE_PARAM_NAME);
      if (maxDiskSpace != null)
      {
        try
        {
          _maxDiskSpace = Long.parseLong(maxDiskSpace);
        }
        catch (NumberFormatException nfe)
        {
          _maxDiskSpace = _DEFAULT_MAX_DISK_SPACE;
        }
      }
      else
      {
        _maxDiskSpace = _DEFAULT_MAX_DISK_SPACE;
      }
    }

    if (_tempDir == null)
    {
      _tempDir = info.initParams.get(TEMP_DIR_PARAM_NAME);
      // Use the webapp temporary directory if the temporary directory
      // has not been explicitly set.
      if (_tempDir == null)
      {
        File tempDirFile = (File)
          info.attributes.get("javax.servlet.context.tempdir");
        if (tempDirFile != null)
          _tempDir = tempDirFile.getAbsolutePath();
      }
    }
  }

  private UploadedFile _processFile(
      Object request, UploadedFile tempFile)
  {
    RequestInfo info = _getRequestInfo(request);
    int contentLength = getContentLength(request);
    Map<String, Object> requestMap;

    if (_isPortletRequestClass(request))
      requestMap = _getPortletRequestMap(request);
    else
      requestMap = _getServletRequestMap(request);

    Long maxMemory = (Long)requestMap.get(MAX_MEMORY_PARAM_NAME);
    Long maxDiskSpace = (Long)requestMap.get(MAX_DISK_SPACE_PARAM_NAME);
    String tempDir = (String)requestMap.get(TEMP_DIR_PARAM_NAME);

    if (maxMemory != null)
    {
      _maxMemory = maxMemory;
    }

    if (maxDiskSpace != null)
    {
      _maxDiskSpace = maxDiskSpace;
    }

    if (tempDir != null)
      _tempDir = tempDir;

    if(contentLength>_maxDiskSpace)
    {
      return new ErrorFile(_LOG.getMessage("UPLOADED_FILE_LARGE"));
    }
    // Process one new file, loading only as much as can fit
    // in the remaining memory and disk space.
    UploadedFileImpl file = new UploadedFileImpl();
    try
    {
      file.loadFile(tempFile,
                    _maxMemory - info.totalBytesInMemory,
                    _maxDiskSpace - info.totalBytesOnDisk,
                    _tempDir);
    }
    catch(IOException ioe)
    {
      _LOG.severe(ioe);
      return new ErrorFile(ioe.getLocalizedMessage());
    }

    // Keep a tally of how much we've stored in memory and on disk.
    long length = file.getLength();
    if (file.__isInMemory())
    {
      info.totalBytesInMemory += length;
    }
    else
    {
      info.totalBytesOnDisk += length;
    }

    return file;
  }

  private int getContentLength(Object request)
  {
    int length = -1;
    if (_isPortletRequestClass(request))
    {
      length = _getPortletRequestLength(request);
    }
    else
    {
      length = _getServletRequestLength(request);
    }

    return length;
  }

  private RequestInfo _getRequestInfo(Object request)
  {
    Map<String, Object> attributes;
    if (_isPortletRequestClass(request))
    {
      attributes = _getPortletRequestMap(request);
    }
    else
    {
      attributes = _getServletRequestMap(request);
    }


    RequestInfo info = (RequestInfo) attributes.get(_REQUEST_INFO_KEY);

    if (info == null)
    {
      info = new RequestInfo();
      attributes.put(_REQUEST_INFO_KEY, info);
    }

    return info;
  }

  private boolean _isPortletRequestClass(Object request)
  {
    return (_PORTLET_REQUEST_CLASS != null && _PORTLET_REQUEST_CLASS.isInstance(request));
  }

  private static final ContextInfo _getServletContextInfo(final Object context)
  {
    assert(context instanceof ServletContext);

    final ServletContext sContext = (ServletContext)context;
    return new ContextInfo(
             new ServletInitParameterMap(sContext),
             new ServletApplicationMap(sContext));
  }

  private static final ContextInfo _getPortletContextInfo(final Object context)
  {
    assert(context instanceof PortletContext);

    final PortletContext pContext = (PortletContext)context;
    return new ContextInfo(
             new PortletInitParameterMap(pContext),
             new PortletApplicationMap(pContext));
  }

  private static final Map<String, Object> _getServletRequestMap(final Object request)
  {
    assert(request instanceof ServletRequest);

    return new ServletRequestMap((ServletRequest) request);
  }

  private static final Map<String, Object> _getPortletRequestMap(final Object request)
  {
    assert(request instanceof PortletRequest);

    return new PortletRequestMap((PortletRequest) request);
  }

  private static final int _getServletRequestLength(final Object request)
  {
    assert(request instanceof ServletRequest);

    return ((ServletRequest) request).getContentLength();
  }

  private static final int _getPortletRequestLength(final Object request)
  {
    if (!(request instanceof ActionRequest))
      return -1;

    return ((ActionRequest) request).getContentLength();
  }

  static private class RequestInfo
  {
    public long totalBytesInMemory;
    public long totalBytesOnDisk;
  }

  static private class ContextInfo
  {
    public ContextInfo(Map<String,String> init, Map<String, Object> attrib)
    {
      initParams= init;
      attributes = attrib;
    }

    public Map<String, String> initParams;
    public Map<String, Object> attributes;
  }

  private long   _maxMemory = -1;
  private long   _maxDiskSpace = -1;
  private String _tempDir = null;

  private static final long _DEFAULT_MAX_MEMORY = 102400;
  private static final long _DEFAULT_MAX_DISK_SPACE = 2048000;

  private static final String _REQUEST_INFO_KEY = CompositeUploadedFileProcessorImpl.class.getName()+
    ".UploadedFilesInfo";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(CompositeUploadedFileProcessorImpl.class);

  private static final Class<?>        _PORTLET_CONTEXT_CLASS;
  private static final Class<?>       _PORTLET_REQUEST_CLASS;

  static
  {
    Class<?> context;
    Class<?> request;
    try
    {
      context = ClassLoaderUtils.loadClass("javax.portlet.PortletContext");
      request = ClassLoaderUtils.loadClass("javax.portlet.PortletRequest");
    }
    catch (final ClassNotFoundException e)
    {
      _LOG
          .fine("Portlet API is not available on the classpath.  Portlet configurations are disabled.");
      context = null;
      request = null;
    }

    _PORTLET_CONTEXT_CLASS = context;
    _PORTLET_REQUEST_CLASS = request;
  }


  private final List<? extends UploadedFileProcessor> chainedProcessors;
 
}
