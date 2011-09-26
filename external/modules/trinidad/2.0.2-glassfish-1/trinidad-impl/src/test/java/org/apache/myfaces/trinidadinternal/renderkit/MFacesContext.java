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
package org.apache.myfaces.trinidadinternal.renderkit;

import java.io.File;
import java.io.InputStream;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.render.RenderKit;

import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadbuild.test.MockFacesContext12;
import org.apache.shale.test.mock.MockExternalContext;
import org.apache.shale.test.mock.MockServletContext;

/**
 * Mock faces context for use with unit tests
 */
public class MFacesContext extends MockFacesContext12
{
  public MFacesContext(Application application, boolean testMode)
  {
    super(application);
    setCurrentInstance(this);
    _external = new External(testMode, application);
  }

  @Override
  public ResponseWriter getResponseWriter()
  {
    return _responseWriter;
  }

  @Override
  public void setResponseWriter(ResponseWriter responseWriter)
  {
    _responseWriter = responseWriter;
  }

  @Override
  public Iterator<FacesMessage> getMessages()
  {
    return getMessages(_GLOBAL_MESSAGE);
  }

  @SuppressWarnings("unchecked")
  @Override
  public Iterator<FacesMessage> getMessages(String id)
  {
    if (id == null)
      id = _GLOBAL_MESSAGE;

    List<FacesMessage> messages = _messages.get(id);
    if (messages == null)
      messages = Collections.EMPTY_LIST;

    return messages.iterator();
  }

  @Override
  public void addMessage(String id, FacesMessage message)
  {
    if (id == null)
      id = _GLOBAL_MESSAGE;

    List<FacesMessage> messages = _messages.get(id);
    if (messages == null)
    {
      messages = new ArrayList<FacesMessage>();
      _messages.put(id, messages);
    }

    messages.add(message);
  }

  @Override
  public FacesMessage.Severity getMaximumSeverity()
  {
    FacesMessage.Severity max = FacesMessage.SEVERITY_INFO;

    Iterator<String> clients = getClientIdsWithMessages();
    while (clients.hasNext())
    {
      String messagesKey = clients.next();
      List<FacesMessage> messages = _messages.get(messagesKey);
      int len = _messages.size();
      for (int i = 0; i < len; i++)
      {
        FacesMessage fm = messages.get(i);
        FacesMessage.Severity nextSev = fm.getSeverity();

        if (max.compareTo(nextSev) < 0)
        {
          max = nextSev;
          if (max.compareTo(FacesMessage.SEVERITY_FATAL) >= 0)
          {
            return max;
          }
        }
      }
    }
    return max;
  }

  @Override
  public Iterator<String> getClientIdsWithMessages()
  {
    return _messages.keySet().iterator();
  }

  @Override
  public Application getApplication()
  {
    return MApplication.sharedInstance();
  }

  @Override
  public UIViewRoot getViewRoot()
  {
    return _viewRoot;
  }

  @Override
  public void setViewRoot(UIViewRoot viewRoot)
  {
    _viewRoot = viewRoot;
    if (_viewRoot != null)
    {
      _kit = RenderKitBootstrap.getRenderKit(this);
    }
    else
      _kit = null;
  }

  public static void clearContext()
  {
    FacesContext.setCurrentInstance(null);
  }

  public Locale getLocale()
  {
    return Locale.ENGLISH;
  }

  @Override
  public ExternalContext getExternalContext()
  {
    // this method is called a lot, so we don't want to use the "mock"
    // implementations as those expect a specific number of calls:
    return _external;
  }

  @Override
  public RenderKit getRenderKit()
  {
    if (_viewRoot == null)
      throw new IllegalStateException("Trying to get a RenderKit without a UIViewRoot");
    // this method is called a lot, so we don't want to use the "mock"
    // implementations as those expect a specific number of calls:
    return _kit;
  }

  private RenderKit       _kit;
  private ExternalContext _external;
  private ResponseWriter  _responseWriter;
  private UIViewRoot      _viewRoot;
  private Map<String, List<FacesMessage>> _messages =
    new HashMap<String, List<FacesMessage>>();

  private static final class MServletContext
    extends MockServletContext
  {
    @Override
    public String getContextPath()
    {
      return "/test-context-path";
    }
  }

  private static final class External
    extends MockExternalContext
  {
    public External(boolean testMode, Object contextObject)
    {
      super(new MServletContext(), null, null);

      _testMode = testMode;
      _contextObject = contextObject;

      File file = null;
      try
      {
        String tmpdir = System.getProperty("java.io.tmpdir");
        file = new File(tmpdir,
                        "adftest/view/faces/cache".replace('/',
                                           File.separatorChar));
        file.mkdirs();
        _applicationMap.put("javax.servlet.context.tempdir", file);
      }
      catch (Exception e)
      {
        System.err.println("Could not create temp directory " + file + ": " + e);
      }
    }

    @Override
    public Object getRequest() { return _requestObject; }

    @Override
    public Object getResponse() { return _responseObject; }

    @Override
    public Object getSession(boolean create)
    {
      // implement lazy behavior for session creation
      if (create)
      {
        // force SessionMap to be created
        getSessionMap();
      }

      // use the session Map as the session object
      return _sessionMap;
    }

    @Override
    public String getRequestContextPath() { return "/test-context-path"; }

    @Override
    public String getRequestServletPath() { return "/test-faces"; }

    @Override
    public String getInitParameter(String name)
    {
      if (_testMode && Configuration.DISABLE_CONTENT_COMPRESSION.equals(name))
        return "true";
      // A hack to disable image generation
      if ("org.apache.myfaces.trinidadinternal.BLOCK_IMAGE_GENERATION".equals(name))
        return "true";
      return null;
    }

    @Override
    public String encodeNamespace(String in) { return in; }


    @Override
    public String encodeResourceURL(String url)
    {
      // The spec requires encodeResourceURL() to throw NPE here,
      // though not all impls do
      if (url == null)
        throw new NullPointerException("encodeResourceURL called with null URL");
      return "encoded-resource-url:" + url;
    }

    @Override
    public String encodeActionURL(String url)
    {
      // The spec requires encodeActionURL() to throw NPE here,
      // though not all impls do
      if (url == null)
        throw new NullPointerException("encodeActionURL called with null URL");

      return "encoded-action-url:" + url;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, String> getRequestHeaderMap()
    {
      return Collections.EMPTY_MAP;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, String> getRequestParameterMap()
    {
      return Collections.EMPTY_MAP;
    }

    @Override
    public InputStream getResourceAsStream(String path)
    {
      return MFacesContext.class.getResourceAsStream(path);
    }

    @Override
    public URL getResource(String path) throws MalformedURLException
    {
      return MFacesContext.class.getResource(path);
    }

    @Override
    public Map<String, Object> getApplicationMap()
    {
      // This used to be an unmodifiable map - but I ran into
      // renderers that happened to be the first to lazily boot
      // up a global system, which then cached values on the
      // application map...
      return _applicationMap;
    }

    @Override
    public Map<String, Object> getSessionMap()
    {
      // The underlying Shale Test implementation goes to the servlet
      // request, session, etc.  For the purposes of this test,
      // we shouldn't use any servlet APIs.  So, intercept the
      // session map.   Ideally, renderers shouldn't write into
      // the session map, but see above...
      if (_sessionMap == null)
      {
        synchronized (_contextObject)
        {
          if (_sessionMap == null)
          {
            _sessionMap = Collections.synchronizedMap(new HashMap<String, Object>(2));
          }
        }
      }

      return _sessionMap;
    }

    @Override
    public Map<String, Object> getRequestMap()
    {
      // this method is called a lot, so we don't want to use the "mock"
      // implementations as those expect a specific number of calls:
      return _requestMap;
    }
    
    @Override
    public String getRequestScheme()
    {
      return "http";
    }

    private final Object _contextObject;
    private final Object _requestObject = new String("request object");
    private final Object _responseObject = new String("response object");

    private final Map<String, Object> _requestMap = new HashMap<String, Object>(2);
    private final Map<String, Object> _applicationMap =
                                        Collections.synchronizedMap(new HashMap<String, Object>(2));

    private volatile Map<String, Object> _sessionMap = null;

    private final boolean _testMode;
  }
  private static final String _GLOBAL_MESSAGE = "org.apache.myfaces.trinidadinternal.renderkit.MFacesContext.GLOBAL_MESSAGE";
}
