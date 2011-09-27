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
package org.apache.myfaces.trinidadinternal.facelets;

import java.io.IOException;
import java.io.OutputStream;

import javax.el.ELException;
import javax.el.MethodExpression;
import javax.el.ValueExpression;
import javax.faces.FacesException;
import javax.faces.component.ActionSource;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import javax.faces.view.facelets.ComponentHandler;

import org.apache.myfaces.trinidadinternal.taglib.listener.FileDownloadActionListener;

import javax.faces.view.facelets.FaceletContext;
import javax.faces.view.facelets.FaceletException;
import javax.faces.view.facelets.TagAttribute;
import javax.faces.view.facelets.TagConfig;
import javax.faces.view.facelets.TagHandler;

/**
 *
 */
public class FileDownloadActionListenerTag extends TagHandler
{

  public FileDownloadActionListenerTag(TagConfig tagConfig)
  {
    super(tagConfig);
    _filename = getAttribute("filename");
    _contentType = getAttribute("contentType");
    _method = getRequiredAttribute("method");
  }

  public void apply(FaceletContext faceletContext,
          UIComponent parent) throws IOException, FacesException, FaceletException, ELException
  {
    if(ComponentHandler.isNew(parent))
    {
      FileDownloadActionListener listener = new FileDownloadActionListener();
      if (_filename != null)
      {
        if (_filename.isLiteral())
          listener.setFilename(_filename.getValue());
        else
        {
          ValueExpression valueExp = _filename.getValueExpression(faceletContext,
                                                                  Object.class);
          listener.setValueExpression(FileDownloadActionListener.FILENAME_KEY,
                                     valueExp);
        }
      }

      if (_contentType != null)
      {
        if (_contentType.isLiteral())
          listener.setContentType(_contentType.getValue());
        else
        {
          ValueExpression valueExp = _contentType.getValueExpression(faceletContext,
                                                                  Object.class);
          listener.setValueExpression(FileDownloadActionListener.CONTENT_TYPE_KEY,
                                      valueExp);
        }
      }
      
      MethodExpression me = _method.getMethodExpression(faceletContext,
                                                        Object.class,
                                                        _METHOD_PARAMS);
      listener.setMethod(me);

      ActionSource actionSource = (ActionSource)parent;
      actionSource.addActionListener(listener);
    }
  }

  static private final Class[] _METHOD_PARAMS =
    new Class[]{FacesContext.class, OutputStream.class};

  private final TagAttribute _method;
  private final TagAttribute _filename;
  private final TagAttribute _contentType;
}
