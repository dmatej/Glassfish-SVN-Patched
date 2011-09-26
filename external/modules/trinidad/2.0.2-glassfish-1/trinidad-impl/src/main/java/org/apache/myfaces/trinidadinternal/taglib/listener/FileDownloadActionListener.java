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
package org.apache.myfaces.trinidadinternal.taglib.listener;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import java.util.Map;

import javax.el.MethodExpression;

import javax.faces.application.FacesMessage;
import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.FacesBeanImpl;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ComponentUtils;
import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidadinternal.util.MimeUtility;


/**
 * @todo Look at moving to org.apache.myfaces.trinidad.event
 * @todo Extending FacesBean is very lame if we make this
 *   class part of our public API, but the FacesBean API
 *   would otherwise require a private subclass of FacesBeanImpl.
 *   We need a better way out.
 */
public class FileDownloadActionListener extends FacesBeanImpl
  implements ActionListener, StateHolder
{
  static public final FacesBean.Type TYPE = new FacesBean.Type();
  static public final PropertyKey FILENAME_KEY =
    TYPE.registerKey("filename");
  // Must be a ValueExpression
  static public final PropertyKey CONTENT_TYPE_KEY =
    TYPE.registerKey("contentType");
  static public final PropertyKey METHOD_KEY =
    TYPE.registerKey("method",
                     MethodExpression.class,
                     PropertyKey.CAP_NOT_BOUND);
  

  /**
    * <p>The message identifier of the {@link FacesMessage} to be created when
    * there is a download error.</p>
    */
     public static final String DOWNLOAD_MESSAGE_ID =
         "org.apache.myfaces.trinidad.event.FileDownloadActionListener.DOWNLOAD_ERROR";  

  static
  {
    TYPE.lock();
  }

  public FileDownloadActionListener()
  {
  }

  public void processAction(ActionEvent event)
  {
    String filename = getFilename();
    String contentType = getContentType();
    
    FacesContext context = FacesContext.getCurrentInstance();
    Object response = context.getExternalContext().getResponse();
    if (!(response instanceof HttpServletResponse))
    {
      _LOG.warning("FILE_DOWNLOAD_LISTENER_REQUIRES_SERVLET");
    }
    else
    {
      HttpServletResponse hsr = (HttpServletResponse) response;
      try
      {
        if (contentType != null)
          // TODO: encoding?
          hsr.setContentType(contentType);
        if (filename != null)
        {
          // check for supported user agents. Currently IE, Gecko, and WebKit.
          // IE and WebKit use UTF-8 encoding.
          boolean isGecko = true;
          Map<String, String> headers = context.getExternalContext().getRequestHeaderMap();
          String agentName = headers.get("User-Agent").toLowerCase();
          if (agentName.contains("msie") || agentName.contains("applewebkit") || agentName.contains("safari"))
            isGecko = false;
          // boolean isIE = CoreRenderer.isIE(RenderingContext.getCurrentInstance());
          String encodeHTTPHeaderFilename = MimeUtility.encodeHTTPHeader(filename, !isGecko);
          // double quotes are needed in case the filename is long. otherwise the filename gets
          // truncated in Firefox.
          hsr.setHeader("Content-Disposition",
                        "attachment; filename=\""+encodeHTTPHeaderFilename + "\"");

        }
        MethodExpression method = getMethod();
        OutputStream out = new BufferedOutputStream(new OnDemandOutputStream(hsr));
        try
        {
          method.invoke(context.getELContext(), new Object[]{context, out});
        }
        catch (Exception e)
        {
          FacesMessage error = MessageFactory.getMessage(e);
          context.addMessage(null, error);
          throw e;
        }
        out.close();
         
      }
      catch (Exception e)
      {
        hsr.reset();
        _LOG.warning(e);       
        FacesMessage message = MessageFactory.getMessage(context, DOWNLOAD_MESSAGE_ID);
        context.addMessage(null, message);
        context.renderResponse();
        return;
      }
    }
    
    context.responseComplete();
  }

  public MethodExpression getMethod()
  {
    return (MethodExpression) getProperty(METHOD_KEY);
  }

  public void setMethod(MethodExpression method)
  {
    setProperty(METHOD_KEY, method);
  }



  public String getFilename()
  {
    return ComponentUtils.resolveString(getProperty(FILENAME_KEY));
  }

  public void setFilename(String filename)
  {
    setProperty(FILENAME_KEY, filename);
  }

  public String getContentType()
  {
    return ComponentUtils.resolveString(getProperty(CONTENT_TYPE_KEY));
  }

  public void setContentType(String contentType)
  {
    setProperty(CONTENT_TYPE_KEY, contentType);
  }

  @Override
  public Type getType()
  {
    return TYPE;
  }

  public boolean isTransient()
  {
    return false;
  }

  public void setTransient(boolean newTransientValue)
  {
    throw new UnsupportedOperationException();
  }
  
  /**
   * The purpose of this class is to avoid retrieving OutputStream from the response
   * until we are actually ready to write something. This solves the problem of not being
   * able to retrieve a Writer from the response later in case an exception was thrown
   * before anything was written into the stream
   * 
   * This class retrieves an OutputStream from the response
   * 'on demand' - whenever we are trying to write anything into the stream.
   */
  static class OnDemandOutputStream extends OutputStream 
  {
    OnDemandOutputStream(HttpServletResponse hsr)
    {
      _hsr = hsr;
    }
    
    public void write(int b) throws IOException    
    {
      _getOutputStream(true).write(b);
    }
    
    public void write(byte[] b) throws IOException
    {
      _getOutputStream(true).write(b);
    }
    
    public void write(byte[] b, int off, int len) throws IOException
    {
      _getOutputStream(true).write(b, off, len);
    }
    
    public void flush() throws IOException
    {
      // If we have not written anything, don't bother retrieving a delegate
      // stream and flushing it
      OutputStream delegate = _getOutputStream(false);
      if (delegate != null)
      {
        delegate.flush();
      }
    }
    
    public void close() throws IOException
    {
      // If we have not written anything, don't bother retrieving a delegate
      // stream and closing it
      OutputStream delegate = _getOutputStream(false);
      if (delegate != null)
      {
        delegate.close();
      }
    }
    
    private OutputStream _getOutputStream(boolean create) throws IOException
    {
      if (create && _delegate == null)
      {
        _delegate = _hsr.getOutputStream();
      }
      return _delegate;
    }
    
    private final HttpServletResponse _hsr;
    
    private OutputStream _delegate = null;
  }

  // saveState() and restoreState() come from FacesBeanImpl
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FileDownloadActionListener.class);
}
