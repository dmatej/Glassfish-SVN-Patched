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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidadinternal.util.FormattedTextParser;


/**
 * Utility class for working with formatted text.
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/XhtmlFormattedText.java#0 $) $Date: 10-nov-2005.18:54:17 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
class XhtmlFormattedText
{
  static public FormattedTextParser getFormattedTextParser()
  {
    return _sParser;
  }

  
  private XhtmlFormattedText()
  {
  }

  // This is for the font element. Only size is the allowed attribute.
  static private class FontElement
    extends FormattedTextParser.DefaultElementInfo
  {
    public FontElement(String name)
    {
      super(name);
    }

    public FontElement(String name, boolean empty, boolean closeRequired)
    {
      super(name, empty, closeRequired);
    }
    
    @Override
    public void writeInlineStyle(FacesContext context, String style)
    {

    }

    @Override
    public void writeStyleClass(FacesContext context, String styleClass)
    {

    }
    
    @Override
    public void writeHRef(FacesContext context, String href)
    {
    
    }  

    @Override
    public void writeSize(FacesContext context, String fontSize)
      throws IOException
    {
     context.getResponseWriter().writeAttribute("size", fontSize, null);
    }   
  }
  
  static private class XhtmlElement
    extends FormattedTextParser.DefaultElementInfo
  {
    public XhtmlElement(String name)
    {
      super(name);
    }

    public XhtmlElement(String name, boolean empty, boolean closeRequired)
    {
      super(name, empty, closeRequired);
    }

    @Override
    public void writeInlineStyle(FacesContext context, String style)
      throws IOException
    {
      //      if (XhtmlLafRenderer.supportsStyleAttributes(context))
      {
        super.writeInlineStyle(context, style);
      }
      /*else if (XhtmlLafRenderer.renderStyleElements(context))
      {
        // ??? =-=AEW How do we get a Style object???
      }*/
    }

    @Override
    public void writeStyleClass(FacesContext context, String styleClass)
      throws IOException
    {
      //      if (XhtmlLafRenderer.supportsStyleAttributes(context))
      {
        super.writeStyleClass(context, styleClass);
        //   XhtmlLafRenderer.renderStyleClassAttribute(context, styleClass);
      }
      /*else if (XhtmlLafRenderer.renderStyleElements(context))
      {
        // =-= Darn annoying to do given the current API for
        // ElementInfo;  we could start the style elements here
        // easily enough, but how do we know when to end the style elements?
      }*/
    }
  }

  static private final FormattedTextParser _sParser =
    new FormattedTextParser();
  
  static
  {
    // Register all the allowed elements
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("br", true, true));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("hr", true, true));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("li", false, false));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("p", false, false));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("b"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("i"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("tt"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("big"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("small"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("span"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("pre"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("ul"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("ol"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("em"));
    _sParser.addElement(new XhtmlFormattedText.XhtmlElement("a"));
    _sParser.addElement(new XhtmlFormattedText.FontElement("font"));   
  }
}
