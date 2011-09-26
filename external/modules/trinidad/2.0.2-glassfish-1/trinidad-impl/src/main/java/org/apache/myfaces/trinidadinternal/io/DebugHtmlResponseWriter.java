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
package org.apache.myfaces.trinidadinternal.io;

import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;



/**
 * ResponseWriter that decorates another and checks for common
 * mistakes in HTML output.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/io/DebugHtmlResponseWriter.java#1 $) $Date: 11-nov-2005.14:59:40 $
 */
public class DebugHtmlResponseWriter extends ResponseWriterDecorator
{
  /**
   * Creates a DebugHtmlResponseWriter.
   */
  public DebugHtmlResponseWriter(ResponseWriter decorated)
  {
    super(decorated);
    _elementStack = new Stack<String>();
  }

  /**
   * Creates a new instance of this DebugHtmlResponseWriter, using a different
   * Writer.
   */
  @Override
  public ResponseWriter cloneWithWriter(Writer writer)
  {
    DebugHtmlResponseWriter cloned =
      new DebugHtmlResponseWriter(getResponseWriter().cloneWithWriter(writer));
    cloned._elementStack.addAll(_elementStack);
    return cloned;
  }



  @Override
  public void startElement(String name, UIComponent component) throws IOException
  {
    String lowerName = name.toLowerCase();
    // check for nested <form> tag, nested <html> tag,
    // and nested <body> tag
    if ("form".equals(lowerName) ||
        "body".equals(lowerName) ||
        "html".equals(lowerName))
    {
      if(_elementStack.contains(lowerName))
        _errorWithComment("Illegal HTML: Cannot nest <" + lowerName + "> elements");
    }


    // Check for elements that are not allowed inside each other -
    // but only when PPR is off (with PPR, we're not rendering the
    // whole tree, so it's easy for supposedly illegal combinations)
    RenderingContext rc = RenderingContext.getCurrentInstance();
    if (((rc == null) || (rc.getPartialPageContext() == null)) &&
        !_elementStack.empty())
    {
      String parent = _elementStack.peek();
      Collection<String> allowedParents = _sAllowedParents.get(lowerName);
      Collection<String> allowedChildren = _sAllowedChildren.get(parent);
      if (((allowedParents != null)  &&
           !allowedParents.contains(parent)) ||
          ((allowedChildren != null)  &&
           // In practice, <script> can be embedded anywhere
           !"script".equals(lowerName) &&
           !allowedChildren.contains(lowerName)))
      {
        _errorWithComment("Illegal HTML: cannot put a <" + lowerName + "> element in " +
                          "a <" + parent + "> element.");
      }
    }

    _elementStack.push(lowerName);

    super.startElement(name, component);
  }


  @Override
  public void endElement(String name) throws IOException
  {
    if (!_elementStack.empty())
      _elementStack.pop();

    super.endElement(name);
  }

  @Override
  public void writeAttribute(String name,
                             Object value,
                             String componentPropertyName)
        throws IOException
  {
    if ("name".equals(name))
    {
      // =-=AEW. Review this. Try checking against a regular expression instead
      if ((value != null) &&
          value.toString().indexOf(' ') >= 0)
      {
        // Because of the fix for bug 2944473, in some rare
        // cases UIX generates spaces in the "name" attribute of anchor tags.
        if (!"a".equals(_elementStack.peek()))
          _LOG.warning("Illegal character (space) in \"name\" attribute");
      }
      // And "target" causes problems too - see bug 2693457
      else if ("target".equals(value))
      {
        _LOG.warning("NAME_ATTRIBUTE_OF_TARGET_WILL_CAUSE_JAVASCRIPT_ERROR");
      }
    }
    // Javascript "onXXXX" handlers never need to start with
    // javascript:;  people constantly do this, which eventually causes
    // problems!
    else if ((name != null) && name.startsWith("on") && (value != null))
    {
      String valueStr = value.toString();
      if (valueStr.startsWith("javascript:") ||
          valueStr.startsWith("Javascript:"))
      {
        _LOG.info("UNNECESSARY_NAME_ATTRIBUTE_START_WITH_JAVASCRIPT", name);
      }
    }

    super.writeAttribute(name, value, componentPropertyName);
  }

  private void _errorWithComment(String text) throws IOException
  {
    _LOG.warning(text);
    writeComment("INVALID HTML:");
  }


  // Yes, Stack is slow and lame.  This code is used for debugging
  // only, so that is of little concern.
  private Stack<String> _elementStack;

  static private final Map<String, List<String>> _sAllowedParents = 
    new HashMap<String, List<String>>(13);
  static private final Map<String, List<String>> _sAllowedChildren  = 
    new HashMap<String, List<String>>(13);

  static
  {
    // Allowed children of "table".  "tr" isn't legit as far
    // as the HTML 4.0 spec is concerned, but reality says something
    // else.  Ditto for "script", which basically can go anywhere,
    // but we hardcode that rule.
    _sAllowedChildren.put("table",
       Arrays.asList(
          new String[]{"tr", "caption",
                       "thead", "tfoot", "tbody", "col", "colgroup"}));

    _sAllowedChildren.put("tr",
       Arrays.asList(
          new String[]{"th", "td"}));

    _sAllowedChildren.put("select",
       Arrays.asList(
          new String[]{"option", "optgroup"}));

    _sAllowedChildren.put("ol",
       Arrays.asList(
          new String[]{"li"}));

    _sAllowedChildren.put("ul",
       Arrays.asList(
          new String[]{"li"}));

    _sAllowedChildren.put("input",
       Arrays.asList(new String[0]));

    _sAllowedChildren.put("hr",
       Arrays.asList(new String[0]));

    _sAllowedChildren.put("br",
       Arrays.asList(new String[0]));

    _sAllowedChildren.put("area",
       Arrays.asList(new String[0]));

    _sAllowedChildren.put("link",
       Arrays.asList(new String[0]));

    _sAllowedChildren.put("img",
       Arrays.asList(new String[0]));

    _sAllowedChildren.put("col",
       Arrays.asList(new String[0]));

    _sAllowedChildren.put("frame",
       Arrays.asList(new String[0]));

    _sAllowedChildren.put("base",
       Arrays.asList(new String[0]));

    _sAllowedChildren.put("meta",
       Arrays.asList(new String[0]));

    _sAllowedParents.put("tr",
       Arrays.asList(
          new String[]{"table", "thead", "tfoot", "tbody"}));

    _sAllowedParents.put("td",
       Arrays.asList(
          new String[]{"tr"}));

    _sAllowedParents.put("th",
       Arrays.asList(
          new String[]{"tr"}));

    _sAllowedParents.put("option",
       Arrays.asList(
          new String[]{"select", "optgroup"}));
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(DebugHtmlResponseWriter.class);
}
