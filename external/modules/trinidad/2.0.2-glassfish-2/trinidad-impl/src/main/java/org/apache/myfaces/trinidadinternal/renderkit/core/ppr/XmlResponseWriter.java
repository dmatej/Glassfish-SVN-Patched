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

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.io.XMLEscapes;

public class XmlResponseWriter extends ResponseWriter
{
  public XmlResponseWriter(
    Writer writer,
    String encoding)
  {
    _out = writer;
    _encoding = encoding;
    _cdataCount = 0;
  }

  public String getCharacterEncoding()
  {
    return _encoding;
  }

  public String getContentType()
  {
    return "text/xml";
  }

  public void startDocument() throws IOException
  {
    //TODO include character encoding
    _out.write("<?xml version=\"1.0\" ?>\n");
  }

  /**
   * Writes out CDATA start.
   * @throws IOException on any read/write error
   */
  public void startCDATA() throws IOException 
  {
    closeStartIfNecessary();
    // Ignore all nested calls to start a CDATA section except the first - a CDATA section cannot contain the string 
    // "]]>" as the section ends ends with the first occurrence of this sequence.
    _cdataCount++;
    
    if (_cdataCount == 1)
      _out.write("<![CDATA[");
  }

  /**
   * Writes out an end CDATA element.
   * @throws IOException on any read/write error
   */
  public void endCDATA() throws IOException 
  {
    // Only close the outermost CDATA section and ignore nested calls to endCDATA(). 
    if (_cdataCount == 1)
      _out.write("]]>");
    
    _cdataCount--;
  }

  public void endDocument() throws IOException
  {
  }

  public void startElement(
    String      name,
    UIComponent component) throws IOException
  {
    closeStartIfNecessary();

    Writer out = _out;
    out.write('<');
    out.write(name);
    _closeStart = true;
  }

  public void writeAttribute(
    String name,
    Object value,
    String attrName) throws IOException
  {
    if (value == null)
      return;

    Writer out = _out;
    
    // write the attribute value
    out.write(' ');
    out.write(name);
    out.write("=\"");
    XMLEscapes.writeAttribute(out, value.toString());
    out.write("\"");
  }

  public void writeComment(
    Object comment) throws IOException
  {
    if (comment != null)
    {
      closeStartIfNecessary();

      Writer out = _out;
      out.write("<!-- ");
      out.write(comment.toString());
      out.write(" -->");
    }
  }

  public void writeText(
    char[] text,
    int    offset,
    int    length) throws IOException
  {
    if (text != null)
    {
      closeStartIfNecessary();
      XMLEscapes.writeText(_out, text, offset, length);
    }
  }

  public void writeText(
    Object text,
    String attrName) throws IOException
  {
    if (text != null)
    {
      closeStartIfNecessary();
      XMLEscapes.writeText(_out, text.toString());
    }
  }

  public void writeURIAttribute(
    String name,
    Object value,
    String attrName) throws IOException
  {
    writeAttribute(name, value, attrName);
  }

  public void endElement(
    String name) throws IOException
  {
    Writer out = _out;
    if (_closeStart)
    {
      out.write("/>");
      _closeStart = false;
    }
    else
    {
      out.write("</");
      out.write(name);
      out.write(">");
    }
  }

  public ResponseWriter cloneWithWriter(
    Writer writer)
  {
    return new XmlResponseWriter(writer, getCharacterEncoding());
  }

  public void write(
    char[] cbuf,
    int    off,
    int    len) throws IOException
  {
    closeStartIfNecessary();
    _out.write(cbuf, off, len);
  }

  public void write(char[] cbuf) throws IOException
  {
    closeStartIfNecessary();
    _out.write(cbuf);
  }

  public void write(
    int c) throws IOException
  {
    closeStartIfNecessary();
    _out.write(c);
  }

  public void write(
    String str) throws IOException
  {
    closeStartIfNecessary();
    _out.write(str);
  }

  public void write(
    String str, 
    int    off, 
    int    len) throws IOException
  {
    closeStartIfNecessary();
    _out.write(str, off, len);
  }

  public void close() throws IOException
  {
    _out.close();
  }

  public void flush() throws IOException
  {
    _out.flush();
  }

  protected void closeStartIfNecessary() throws IOException
  {
    if (_closeStart)
    {
      Writer out = _out;
      out.write('>');
      _closeStart = false;
    }
  }
  
  private final Writer      _out;
  private final String      _encoding;
  private       boolean     _closeStart;
  private       int         _cdataCount;
}
