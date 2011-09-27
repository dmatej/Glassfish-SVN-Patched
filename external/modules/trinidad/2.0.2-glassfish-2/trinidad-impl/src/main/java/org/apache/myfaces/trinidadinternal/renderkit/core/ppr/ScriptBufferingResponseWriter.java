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
package org.apache.myfaces.trinidadinternal.renderkit.core.ppr;


import java.io.IOException;
import java.io.Writer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.io.ResponseWriterDecorator;


/**
 * ResponseWriter implementation that buffers scripts.
 */
public class ScriptBufferingResponseWriter extends ResponseWriterDecorator
{
  public ScriptBufferingResponseWriter(ResponseWriter out)
  {
    super(out);
  }
  
  public ScriptBufferingResponseWriter(ResponseWriter out, boolean enabled)
  {
    super(out);
    _enabled = enabled;
  }

  /**
   * Constructor for clones - share the scripts and libraries list.
   */
  ScriptBufferingResponseWriter(ResponseWriter out, ScriptBufferingResponseWriter base)
  {
    super(out);
    _scripts = base._scripts;
    _libraries = base._libraries;
    _enabled = base._enabled;
  }

  // Returns a List of Strings containing script content, or null
  // if no scripts were encountered.
  public List<String> getBufferedScripts()
  {
    if (_scripts != null)
      return Collections.unmodifiableList(_scripts);
    
    return null;
  }

  // Returns a List of Strings representing JavaScript library urls, or null
  // if no libraries were encountered.
  public List<String> getBufferedLibraries()
  {
    if (_libraries != null)
      return Collections.unmodifiableList(_libraries);

    return null;
  }
  
  // Resets our buffers.  This is needed for the streaming case, where we
  // flush scripts contents to the browser as each streamed data set becomes
  // available.  When this happens, we need to clear out our script/library
  // buffers.  This should be called immediately after calls to 
  // getBufferedScripts()/getBufferedLibraries().
  public void clearBufferedContents()
  {
    _scripts = null;
    _libraries = null;
  }
  
  public ResponseWriter cloneWithWriter(Writer writer)
  {
    ScriptBufferingResponseWriter rw = new ScriptBufferingResponseWriter(
      getResponseWriter().cloneWithWriter(writer));

    return rw;
  }

  public void writeComment(Object text) throws IOException
  {
    // Assume that comments in scripts come from people doing
    // the old HTML trick
    if (_inScript)
    {
      if (text != null)
        _getScriptBuilder().append(text);
    }
    else
    {
      super.writeComment(text);
    }
  }


  public void writeText(Object text, String property) throws IOException
  {
    if (_inScript)
    {
      if (text != null)
        _getScriptBuilder().append(text);
    }
    else
      super.writeText(text, property);
  }

  public void writeText(
                        char[]      text,
                        int         start,
                        int         length) throws IOException
  {
    if (_inScript)
      _getScriptBuilder().append(text, start, length);
    else
      super.writeText(text, start, length);
  }


  public void write(String text) throws IOException
  {
    if (_inScript)
      _getScriptBuilder().append(text);
    else
      super.write(text);
  }

  public void write(
                    char[]      text,
                    int         start,
                    int         length) throws IOException
  {
    if (_inScript)
      _getScriptBuilder().append(text, start, length);
    else
      super.write(text, start, length);
  }

  public void write(int ch) throws IOException
  {
    if (_inScript)
      _getScriptBuilder().append(ch);
    else
      super.write(ch);
  }

  @Override
  public void write(char[] c) throws IOException
  {
    if (_inScript)
      _getScriptBuilder().append(c);
    else
      super.write(c);
  }

  @Override
  public void write(String text, int off, int len) throws IOException
  {
    if (_inScript)
      _getScriptBuilder().append(text, off, len);
    else
      super.write(text, off, len);
  }

  /* Needed in JSF 1.2
  @Override
  public void writeText(Object      text,
                        UIComponent component,
                        String      propertyName) throws IOException
  {
    if (text != null)
    {
      if (_inScript)
        _getScriptBuilder().append(text);
      else
        super.writeText(text, component, propertyName);
    }
  }
  */

  public void startElement(String name, UIComponent component)
     throws IOException
  {
    if (_enabled && "script".equals(name))
    {
      _inScript = true;
    }
    else
    {
      super.startElement(name, component);
    }
  }


  public void endElement(String name) throws IOException
  {
    if (_inScript)
    {
      if (_scriptBuilder != null)
        _getScriptList().add(_scriptBuilder.toString());
      _scriptBuilder = null;
      _inScript = false;
    }
    else
    {
      super.endElement(name);
    }
  }

  public void writeAttribute(String     name,
                             Object     value,
                             String     property) throws IOException
  {
    if (value == null)
      return;

    if (_inScript)
    {
      if ("src".equals(name) && (value != null))
        _getLibraryList().add(value.toString());
    }
    else
    {
      super.writeAttribute(name, value, property);
    }
  }

  public void writeURIAttribute(
                                String     name,
                                Object     value,
                                String     property) throws IOException
  {
    if (_inScript)
    {
      if ("src".equals(name) && (value != null))
        _getLibraryList().add(value.toString());
    }
    else
    {
      super.writeURIAttribute(name, value, property);
    }
  }

  private StringBuilder _getScriptBuilder()
  {
    if (_scriptBuilder == null)
      _scriptBuilder = new StringBuilder();
    
    return _scriptBuilder;
  }


  private List<String> _getLibraryList()
  {
    return _libraries;
  }
  
  private List<String> _getScriptList()
  {
    return _scripts;
  }

  private boolean       _inScript;
  private StringBuilder _scriptBuilder;
  private List<String>  _libraries = new ArrayList<String>();
  private List<String>  _scripts = new ArrayList<String>();
  private boolean       _enabled = true;
}

