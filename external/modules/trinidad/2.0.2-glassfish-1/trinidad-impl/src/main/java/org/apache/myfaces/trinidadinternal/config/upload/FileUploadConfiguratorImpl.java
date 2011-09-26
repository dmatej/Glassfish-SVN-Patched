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

import java.io.IOException;
import java.io.InputStream;

import java.lang.reflect.Proxy;

import java.util.HashMap;
import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.portlet.faces.annotation.ExcludeFromManagedRequestScope;

import javax.servlet.http.HttpServletRequest;

import org.apache.myfaces.trinidad.config.Configurator;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.UploadedFile;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidad.util.RequestStateMap;
import org.apache.myfaces.trinidad.util.RequestType;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;
import org.apache.myfaces.trinidadinternal.share.util.MultipartFormHandler;
import org.apache.myfaces.trinidadinternal.share.util.MultipartFormItem;

/**
 * This configurator will handle the FileUploads for Trinidad.
 *
 * @version $Revision$ $Date$
 */
public class FileUploadConfiguratorImpl extends Configurator
{

  /**
   * Returns the added parameters
   *
   * @param externalContext
   * @return
   */
  static public Map<String, String[]> getAddedParameters(ExternalContext externalContext)
  {
    @SuppressWarnings("unchecked")
    Map<String, String[]> map = (Map<String, String[]>) externalContext.getRequestMap().get(_PARAMS);

    return map;
  }

  /**
   * Returns <code>true</code> if the request wrapper has been applied.
   *
   * @param context
   * @return
   */
  static public boolean isApplied(ExternalContext context)
  {
    return (RequestStateMap.getInstance(context).get(_APPLIED)!=null);
  }

  /**
   *
   */
  @SuppressWarnings("unchecked")
  static public void apply(ExternalContext context)
  {
    RequestStateMap.getInstance(context).put(_APPLIED, AppliedClass.APPLIED);
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidad.config.Configurator#beginRequest(javax.faces.context.ExternalContext)
   */
  @Override
  @SuppressWarnings("unchecked")
  public void beginRequest(ExternalContext externalContext)
  {
    /*
     * Note: This class does not do a dispose on the file uploads.  The
     * reason for this is that in a portal environment, multiple render-requests
     * may depend on the same set of files being available to the view layer.
     * Instead the files will be automatically cleaned up when the portlet
     * generated the next request.  If we need to clean up sooner then we should
     * clean up on the end-request.
     */
    // FIXME AdamWiner We should clean up ASAP - these are potentially very
    // large allocations of memory and file, so cleaning up as soon
    // as possible is a good thing
    //Process MultipartForm if need be
    if (MultipartFormHandler.isMultipartRequest(externalContext) &&
       (externalContext.getRequest() instanceof HttpServletRequest || ExternalContextUtils.isPortlet(externalContext)))
    {
      try
      {
        final MultipartFormHandler mfh = new MultipartFormHandler(externalContext);

        // TODO: How is this set?
        // AdamWiner: looks like the previous Trinidad incarnation
        // of this code didn't have any allowed configuration...
        mfh.setMaximumAllowedBytes(_maxAllowedBytes);
        mfh.setCharacterEncoding(ExternalContextUtils.getCharacterEncoding(externalContext));

        final HashMap<String, String[]> parameters = new HashMap<String, String[]>();
        MultipartFormItem item;
        final UploadedFiles files = new UploadedFiles(externalContext);
        while ((item = mfh.getNextPart()) != null)
        {
          final String name = item.getName();
          String value = null;
          // No filename - it's not a file uploaded field
          if (item.getFilename() == null)
          {
            value = item.getValue();
            final Object oldValue = parameters.get(name);
            if (oldValue == null)
            {
              parameters.put(name, new String[]{value});
            }
            else
            {
              final String[] oldArray = (String[]) oldValue;
              final String[] newArray = new String[oldArray.length + 1];
              System.arraycopy(oldArray, 0, newArray, 1, oldArray.length);
              newArray[0] = value;
              parameters.put(name, newArray);
            }
          }
          // Upload a file
          else if (item.getFilename().length() > 0)
          {
            _doUploadFile(RequestContext.getCurrentInstance(), externalContext, files, item);
          }
        }
        externalContext.getRequestMap().put(_PARAMS, parameters);
      }
      catch (Throwable t)
      {
        if(_LOG.isSevere())
        {
          _LOG.severe(t);
        }
      }
    }
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidad.config.Configurator#getExternalContext(javax.faces.context.ExternalContext)
   */
  @Override
  public ExternalContext getExternalContext(ExternalContext externalContext)
  {
    //Wrap only if there are parameters present
    Map<String, String[]> addedParams = getAddedParameters(externalContext);

    if(addedParams != null)
    {
      return _getExternalContextWrapper(externalContext, addedParams);
    }

    return externalContext;
  }
  
  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidad.config.Configurator#endRequest(javax.faces.context.ExternalContext)
   */
  @Override
  public void endRequest(ExternalContext externalContext)
  {
    // TODO matzew check portlet env.
    if(!ExternalContextUtils.isPortlet(externalContext))
    {
      UploadedFiles files = UploadedFiles.getUploadedFiles(externalContext);
      if(files != null)
        files.dispose();
    }
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidad.config.Configurator#init(javax.faces.context.ExternalContext)
   */
  @Override
  public void init(ExternalContext externalContext)
  {
    super.init(externalContext);
    //TODO initialize _maxAllowedBytes
  }

  private void _doUploadFile(
      final RequestContext   context,
      final ExternalContext  externalContext,
      final UploadedFiles     files,
      final MultipartFormItem item) throws IOException
  {
    final UploadedFile temp = new TempUploadedFile(item);
    Map<String, Object> sessionMap = externalContext.getSessionMap();
    Map<String, Object> requestMap = externalContext.getRequestMap();
    
    _copyParamsFromSessionToRequestMap(sessionMap, requestMap,
      UploadedFileProcessor.MAX_MEMORY_PARAM_NAME,
      UploadedFileProcessor.MAX_DISK_SPACE_PARAM_NAME,
      UploadedFileProcessor.TEMP_DIR_PARAM_NAME);
    
    final UploadedFile file =
      context.getUploadedFileProcessor().processFile(externalContext.getRequest(), temp);

    if (file != null)
    {
      // Store the file.
      files.__put(item.getName(), file);

      if (_LOG.isFine())
      {
        _LOG.fine("Uploaded file " + file.getFilename() + "(" +
            file.getLength() + " bytes) for ID " + item.getName());
      }
    }
  }

  /**
   * copies some params (varargs) from the session map to the request map 
   */
  private void _copyParamsFromSessionToRequestMap(Map<String, Object> sessionMap, Map<String, Object> requestMap, String... params)
  {
    for(String param : params)
    {
      requestMap.put(param,  sessionMap.get(param));
    }
  }

  static private ExternalContext _getExternalContextWrapper(ExternalContext externalContext, Map<String, String[]> addedParams)
  {
    if(!isApplied(externalContext))
    {
      RequestType type = ExternalContextUtils.getRequestType(externalContext);
      
      switch(type)
      {
        case SERVLET:
          externalContext.setRequest(new UploadRequestWrapper(externalContext, addedParams));
          break;
        case RESOURCE:
          externalContext.setRequest(new UploadResourceRequest(externalContext, addedParams));
          break;
        case ACTION:
          //Portlet 2.0 should use the framework provided wrapper.  Portlet 1.0 needs to implement
          //the interface.  Because we need to compile against Portlet 2.0, we implement the Portlet
          //1.0 scenario using a Proxy.
          Object req;
          
          if(_ENHANCED_PORTLET_SUPPORTED)
          {
            req = _getActionRequestWrapper(externalContext, addedParams);
          }
          else
          {
            req = _getActionRequestProxy(externalContext, addedParams);
          }
          
          externalContext.setRequest(req);
      }
      apply(externalContext);        
    }

    //If we don't have any wrapped params or we have a render portal request,
    //return the origional external context
    return externalContext;
  }
  
  static private Object _getActionRequestProxy(ExternalContext ec, Map<String, String[]> params)
  {
    try
    {
      Class<?> actionRequestClass = ClassLoaderUtils.loadClass("javax.portlet.ActionRequest");
      return Proxy.newProxyInstance(ClassLoaderUtils.getContextClassLoader(), new Class<?>[]{actionRequestClass}, new UploadActionInvocationHandler(ec, params));
    }
    catch (ClassNotFoundException e)
    {
      throw new RuntimeException(e);
    }
  }
  
  static private Object _getActionRequestWrapper(ExternalContext ec, Map<String, String[]> params)
  {
    try
    {
      Class<?> wrapperClass = ClassLoaderUtils.loadClass("org.apache.myfaces.trinidadinternal.config.upload.UploadActionRequestWrapper");
      return wrapperClass.getConstructor(ExternalContext.class, Map.class).newInstance(ec, params);
    }
    catch (Exception e)
    {
      throw new RuntimeException(e);
    }
  }
  
  //This will ensure the property is removed on the next request
  @ExcludeFromManagedRequestScope
  static private class AppliedClass
  {
    static public final AppliedClass APPLIED = new AppliedClass();
  }

  static private class TempUploadedFile implements UploadedFile
  {
    public TempUploadedFile(MultipartFormItem item)
    {
      _item = item;
      assert(item.getValue() == null);
    }

    public String getFilename()
    {
      return _item.getFilename();
    }

    public String getContentType()
    {
      return _item.getContentType();
    }

    public long getLength()
    {
      // The length is not known yet.
      return -1L;
    }

    public Object getOpaqueData()
    {
      return null;
    }

    public InputStream getInputStream() throws IOException
    {
      return _item.getInputStream();
    }

    public void dispose()
    {
      throw new UnsupportedOperationException();
    }

    private MultipartFormItem _item;
  }
  static private final String _APPLIED = FileUploadConfiguratorImpl.class.getName()+".APPLIED";
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FileUploadConfiguratorImpl.class);
  static private final String _PARAMS = FileUploadConfiguratorImpl.class.getName()+".PARAMS";
  static private final boolean _ENHANCED_PORTLET_SUPPORTED = ExternalContextUtils.isRequestTypeSupported(RequestType.RESOURCE);
  
  private long _maxAllowedBytes = 1L << 27;
}
