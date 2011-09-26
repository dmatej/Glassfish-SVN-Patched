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
package org.apache.myfaces.trinidadinternal.util;

import java.io.IOException;

import java.util.ArrayList;
import java.util.HashMap;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidad.context.RenderingContext;

/**
 * Class responsible for performing a very lightweight parse
 * of a primitive HTML subset.  FormattedTextParsers are threadsafe
 * and will generally be created once and reused, though the
 * addElement() method should be called only before the parser
 * has been used.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/FormattedTextParser.java#0 $) $Date: 10-nov-2005.18:49:08 $
 */
public class FormattedTextParser
{
  /**
   * Create a FormattedTextParser.
   */
  public FormattedTextParser()
  {
    _elements = new HashMap<String, ElementInfo>(23);
  }

  /**
   * Adds a type of element to the parser.  The element
   * name will be supported in both lower and upper case.
   */
  public void addElement(ElementInfo element)
  {
    String name = element.getName();
    _elements.put(name, element);
    _elements.put(name.toUpperCase(), element);
  }


  /**
   * Outputs a String, using the set of registered
   * ElementInfos to handle contained elements.
   */
  public void writeFormattedText(
    FacesContext context,
    String           text) throws IOException
  {
    int length = text.length();

    ArrayList<ElementInfo> elementStack = 
      new ArrayList<ElementInfo>(10);

    // Constant for current parsing state
    int state = _OUT_OF_ELEMENT;
    ResponseWriter writer = context.getResponseWriter();

    int i = 0;
    while (i < length)
    {
      char c = text.charAt(i);
      switch (state)
      {
        case _OUT_OF_ELEMENT:
          // Start of an element tag:
          if (c == '<')
          {
            // First, find out if we're starting a new
            // element or closing an earlier one
            boolean close = false;
            if (((i + 1) < length) && (text.charAt(i + 1) == '/'))
            {
              close = true;
              i++;
            }

            // Seek to the end of this element name
            int endOfElementName = _getEndOfElementName(text, i + 1, length);
            if (endOfElementName < 0)
            {
              i = length;
              break;
            }

            // Retrieve the element name
            String elementName = text.substring(i + 1, endOfElementName);

            // Find out information about this element;  in particular,
            // is this an allowed element?
            ElementInfo info = _elements.get(elementName);

            // Allowed elements.
            if (info != null)
            {
              if (close)
              {
                if (_popElement(context, elementStack, info))
                {
                  // Render (only if "popping" found a match)
                  info.endElement(context);
                }

                // and skip to the end of the element
                int endOfElement = text.indexOf('>', i);
                if (endOfElement < 0)
                {
                  _parseError("Unterminated element", i);
                  i = length;
                  break;
                }

                i = endOfElement + 1;
              }
              // Starting an allowed element
              else
              {
                // Render and move to the start of the attributes
                _pushElement(context, elementStack, info);
                info.startElement(context);
                state = (info.isEmptyElement()
                         ? _IN_EMPTY_ELEMENT :  _IN_ELEMENT);

                i = endOfElementName;
              }
            }
            // An unsupported element.  Jump past its end, and output
            // nothing.
            else
            {
              int endOfElement = text.indexOf('>', i);
              if (endOfElement < 0)
              {
                _parseError("Unterminated element", i);
                i = length;
                break;
              }

              i = endOfElement + 1;
            }
          }
          // Not in an element;  render the text
          else
          {
            // An entity?
            if (c == '&')
            {
              int endOfEntity = _getEndOfEntity(text, i, length);

              // Couldn't find a semicolon;  this probably wasn't
              // intended as an entity.  Just output the
              // ampersand directly
              if (endOfEntity < 0)
              {
                char[] chars = new char[1];
				chars[0] = c;
				writer.writeText(chars, 0, 1);
                i++;
              }
              // It's an entity - output it.
              else
              {
                c = _getEntity(text, i, endOfEntity);
                if (c != 0)
				{
                   char[] chars = new char[1];
				   chars[0] = c;
				   writer.writeText(chars, 0, 1);
                }
				i = endOfEntity + 1;
              }
            }
            // Just write out the character
            else
            {
			  char[] chars = new char[1];
			  chars[0] = c;
			  writer.writeText(chars, 0, 1);
              i++;
            }
          }
          break;

        case _IN_EMPTY_ELEMENT:
        case _IN_ELEMENT:
          // Inside an element;  process attributes until
          // the element ends.
          if (c == '>')
          {
            // Ending an empty element - end it here.
            if (state == _IN_EMPTY_ELEMENT)
            {
              ElementInfo info = _peekElement(elementStack);
              info.endElement(context);
              _popElement(context, elementStack, info);
            }

            state = _OUT_OF_ELEMENT;
            i++;
          }
          else if (!Character.isWhitespace(c))
          {
            // Starting an attribute
            int endOfAttributeName = _getEndOfAttributeName(text, i, length);
            if (endOfAttributeName < 0)
            {
              _parseError("Unterminated attribute name", i);
              i = length;
              break;
            }

            String attributeName = text.substring(i, endOfAttributeName);
            // An attribute with a value
            if ('=' == text.charAt(endOfAttributeName))
            {
              if (endOfAttributeName + 1 >= length)
              {
                _parseError("Unterminated attribute value",
                            endOfAttributeName);
                i = length;
                break;
              }

              StringBuffer buffer = new StringBuffer();
              int endOfAttributeValue =
                 _getAttributeValue(text,
                                    endOfAttributeName + 1,
                                    length,
                                    buffer);

              if (endOfAttributeValue < 0)
              {
                _parseError("Unterminated attribute value",
                            endOfAttributeName + 1);
                i = length;
                break;
              }

              ElementInfo info = _peekElement(elementStack);

              // Output only the allowed attributes - CSS attributes only
              // and <font>'s size attribute.
              if ("class".equalsIgnoreCase(attributeName))
              {
                info.writeStyleClass(context, buffer.toString());
              }
              else if ("style".equalsIgnoreCase(attributeName))
              {
                info.writeInlineStyle(context, buffer.toString());
              }
              else if ("href".equalsIgnoreCase(attributeName))
              {
                info.writeHRef(context, buffer.toString());
              }
              else if ("size".equalsIgnoreCase(attributeName))
              {
                info.writeSize(context, buffer.toString());
              }

              i = endOfAttributeValue + 1;
            }
            // An empty attribute (no value) - treat as Boolean.TRUE
            else
            {
              // =-=AEW We don't currently support any boolean attributes!
              // out.writeAttribute(attributeName, Boolean.TRUE);
              if ('>' == text.charAt(endOfAttributeName))
                i = endOfAttributeName;
              else
                i = endOfAttributeName + 1;
            }
          }
          else
          {
            // Whitespace in an element - just skip over it.
            i++;
          }

          break;
      }
    }

    // Close up any leftover elements
    int size = elementStack.size() - 1;
    while (size >= 0)
    {
      ElementInfo info = elementStack.get(size);
      info.endElement(context);
      // These _should_ all be elements that do not require being closed.
      if (info.isCloseRequired())
        _parseError("Unterminated element " + info.getName(),  i);
      --size;
    }
  }


  // Push an element onto the stack.  Close elements
  // if needed.
  static private void _pushElement(
    FacesContext           context,
    ArrayList<ElementInfo> elementStack,
    ElementInfo            element) throws IOException
  {
    int size = elementStack.size();

    if (size != 0)
    {
      ElementInfo top = elementStack.get(size - 1);

      // If we were working on a "no-close" element, and starting
      // exactly the same element, close it off now.
      if (!top.isCloseRequired() && (element == top))
      {
        top.endElement(context);
        elementStack.remove(size - 1);
      }
    }

    elementStack.add(element);
  }

  // Look at the top element on the stack (null if the stack
  // is empty)
  static private ElementInfo _peekElement(
    ArrayList<ElementInfo> elementStack)
  {
    int size = elementStack.size();
    if (size == 0)
      return null;

    return elementStack.get(size - 1);
  }


  // Pop an element from the stack.  Close elements if needed.
  static private boolean _popElement(
    FacesContext           context,
    ArrayList<ElementInfo> elementStack,
    ElementInfo            element) throws IOException
  {
    int size;

    while ((size = elementStack.size()) > 0)
    {
      ElementInfo top = elementStack.remove(size - 1);
      // We've reached the correct element
      if (element == top)
      {
        return true;
      }

      // If we've exited the scope of a "noclose" element, forcibly close it
      if (!top.isCloseRequired())
      {
        top.endElement(context);
      }
      else
      {
        _parseError("Unclosed element", -1);
        break;
      }
    }

    return false;
  }


  //
  // Get an attribute value
  // @param text the text containing the value
  // @param start the index into the string immediately after
  // the equals sign
  // @param end the last allowed character
  // @param buffer a buffer to store the value
  // @return the index one past the end of the value, or -1 if
  //         the attribute wasn't properly terminated
  static private int _getAttributeValue(
    String       text,
    int          start,
    int          end,
    StringBuffer buffer)
  {
    char endChar = text.charAt(start);

    // Attributes should generally start with either
    // a single or double-quote, but we'll also support
    // no quotes (in which case any whitespace ends the
    // attribute)
    if ((endChar == '\'') || (endChar =='"'))
      start++;
    else
      endChar = 0;

    int i = start;
    while (i < end)
    {
      char c = text.charAt(i);

      // Started without a quote - whitespace or end-of-element ends
      if (endChar == 0)
      {
        if (Character.isWhitespace(c) || (c=='>'))
          // Return one back from this character - the return
          // is supposed to point to the last character in the
          // attribute value
          return i - 1;
      }
      // Started with a quote - only a matching quote ends
      else if (c == endChar)
      {
        return i;
      }

      // Found an entity - parse it
      if (c == '&')
      {
        int endOfEntity = _getEndOfEntity(text, i, end);
        if (endOfEntity < 0)
        {
          buffer.append(c);
          i++;
        }
        else
        {
          c = _getEntity(text, i, endOfEntity);
          if (c != 0)
            buffer.append(c);
          i = endOfEntity + 1;
        }
      }
      else
      {
        buffer.append(c);
        i++;
      }
    }

    return -1;
  }



  // Scan a string for the index one past the end of the attribute name
  static private int _getEndOfAttributeName(
    String text,
    int    start,
    int    end)
  {
    for (int i = start; i < end; i++)
    {
      char c = text.charAt(i);
      if ((c == '=') || Character.isWhitespace(c))
        return i;
    }

    return -1;
  }


  // Scan a string for the index one past the end of an element name
  static private int _getEndOfElementName(
    String text,
    int    start,
    int    end)
  {
    for (int i = start; i < end; i++)
    {
      char c = text.charAt(i);
      if ((c == '>') || Character.isWhitespace(c))
        return i;
    }

    return -1;
  }

  // Returns the index of the semicolon ending the entity
  static private int _getEndOfEntity(
    String text,
    int    start,
    int    end)
  {
    for (int i = start; i < end; i++)
    {
      char c = text.charAt(i);
      if (c == ';')
        return i;

      if (c == '&')
        continue;

      if (!Character.isLetterOrDigit(c))
        break;
    }

    return -1;
  }

  // Convert a substring representing an HTML entity into a single
  // character
  static private char _getEntity(
    String text,
    int    start,
    int    end)
  {
    start++;

    // Length - the number of chars in the entity, _not_ including
    // the final semicolon
    int length = end - start;
    if (length == 2)
    {
      if (text.startsWith("lt", start))
        return '<';
      if (text.startsWith("gt", start))
        return '>';
    }
    else if (length == 3)
    {
      if (text.startsWith("amp", start))
        return '&';
      if (text.startsWith("reg", start))
        return '\u00ae';
    }
    else if (length == 4)
    {
      if (text.startsWith("copy", start))
        return '\u00a9';
      if (text.startsWith("nbsp", start))
        return '\u00a0';
      if (text.startsWith("quot", start))
        return '"';
    }

    return 0;
  }


  static private void _parseError(
    String           message,
    int              position)
  {
    if (_LOG.isInfo())
    {
      if (position < 0)
        message = "Formatted text parse error:\n" +  message;
      else
        message = "Formatted text parse error at position " + position +
                  ":\n" + message;
      _LOG.info(message);
    }
  }


  /**
   * Abstract representation of an element type.
   */
  static public abstract class ElementInfo
  {
    /**
     * Create an ElementInfo.
     */
    public ElementInfo(String name)
    {
      _name = name.toLowerCase();
    }


    /**
     * Returns the name of the element defined by this object.
     */
    public String getName()
    {
      return _name;
    }

    /**
     * Called to render the results of entering this element.
     */
    abstract public void startElement(FacesContext context)
       throws IOException;


    /**
     * Called to render the results of leaving this element.
     */
    abstract public void endElement(FacesContext context)
       throws IOException;


    /**
     * Called to write out an inline style attribute.
     */
    abstract public void writeInlineStyle(
      FacesContext context, String style) throws IOException;

    /**
     * Called to write an HRef attribute.
     */
    abstract public void writeHRef(
      FacesContext context, String href) throws IOException;

    /**
     * Called to write out an CSS style class attribute.
     */
    abstract public void writeStyleClass(
      FacesContext context, String styleClass) throws IOException;

    /**
     * Called to write out a size attribute.
     */
    abstract public void writeSize(
      FacesContext context, String style) throws IOException;

    /**
     * If true, this element must be empty.
     */
    public boolean isEmptyElement()
    {
      return false;
    }

    /**
     * If false, the element does not have to be forcibly
     * closed, but can be implicitly closed.
     */
    public boolean isCloseRequired()
    {
      return !isEmptyElement();
    }

    private String _name;
  }


  /**
   * Default implementation of ElementInfo.
   */
  static public class DefaultElementInfo extends ElementInfo
  {
    public DefaultElementInfo(String name)
    {
      this(name, false, true);
    }

    public DefaultElementInfo(
      String  name,
      boolean empty,
      boolean closeRequired)
    {
      super(name);
      _empty   = empty;
      _closeRequired = closeRequired;
    }

    @Override
    public void startElement(FacesContext context)
       throws IOException
    {
      context.getResponseWriter().startElement(getName(), null);
    }

    @Override
    public void endElement(FacesContext context)
       throws IOException
    {
      context.getResponseWriter().endElement(getName());
    }

    @Override
    public void writeInlineStyle(
      FacesContext context, String style) throws IOException
    {
      context.getResponseWriter().writeAttribute("style", style, null);
    }

    @Override
    public void writeStyleClass(
      FacesContext context, String styleClass) throws IOException
    {
      context.getResponseWriter().writeAttribute("class", styleClass, null);
    }

    @Override
    public void writeSize(FacesContext context, String fontSize)
      throws IOException
    {
      //no-op. This is for the FontElement only.
    }

    @Override
    public void writeHRef(
      FacesContext context, String href) throws IOException
    {
      // Refuse javascript URLs.
      if (href.regionMatches(true,
                             0,
                             "javascript:",
                             0,
                             11))
        return;

      RenderingContext arc = RenderingContext.getCurrentInstance();
      if (!Boolean.FALSE.equals(arc.getAgent().getCapabilities().get(
               TrinidadAgent.CAP_NAVIGATION)))
      {  
        href = context.getExternalContext().encodeActionURL(href);
        context.getResponseWriter().writeURIAttribute("href", href, null);
      }
    }

    @Override
    public boolean isEmptyElement()
    {
      return _empty;
    }

    @Override
    public boolean isCloseRequired()
    {
      return _closeRequired;
    }

    private boolean _empty;
    private boolean _closeRequired;
  }


  // Map of all the allowed elements;  maps input element
  // names to ElementInfo objects
  private HashMap<String, ElementInfo> _elements;


  // Parsing state constants
  static private final int _OUT_OF_ELEMENT = 0;
  static private final int _IN_ELEMENT = 1;
  static private final int _IN_EMPTY_ELEMENT = 2;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FormattedTextParser.class);
}
