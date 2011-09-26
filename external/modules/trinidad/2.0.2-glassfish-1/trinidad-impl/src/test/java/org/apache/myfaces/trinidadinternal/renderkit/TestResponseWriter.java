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

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Pattern;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestResult;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.MimeTypes;
import org.apache.myfaces.trinidadinternal.share.util.CaboHttpUtils;
import org.apache.myfaces.trinidadinternal.io.XMLEscapes;

/**
 * Implementation of ResponseWriter for outputting XHTML.
 *
 */
public class TestResponseWriter extends ResponseWriter
{
  static public final String XHTML_CONTENT_TYPE = "application/xhtml+xml";
  /**
   * Creates an XhtmlResponseWriter.
   * @param out a Writer to write to
   * @param contentType the xhtml content type
   * @param encoding the character encoding the Writer uses
   */
  public TestResponseWriter(
    Writer out,
    String contentType,
    String encoding,
    Test   test,
    TestResult result) throws UnsupportedEncodingException
  {
    if (out == null)
      throw new NullPointerException();

    _out = out;
    _encoding = encoding;
    _test = test;
    _result = result;
    CaboHttpUtils.validateEncoding(encoding);

    _onlyValidIds = "true".equals(
       System.getProperty("org.apache.myfaces.trinidad.TestIdValidity"));
    _testBlockLevel = "true".equals(
       System.getProperty("org.apache.myfaces.trinidad.TestBlockElementNesting"));
  }

  @Override
  public String getCharacterEncoding()
  {
    return _encoding;
  }

  @Override
  public String getContentType()
  {
    return XHTML_CONTENT_TYPE;
  }

  @Override
  public void startDocument() throws IOException
  {
  }


  @Override
  public void endDocument() throws IOException
  {
    _out.flush();
  }

  @Override
  public void flush() throws IOException
  {
    _closeStartIfNecessary();
  }


  @Override
  public void close()throws IOException
  {
    flush();
    // =-=AEW And anything else?
  }

  @Override
  public void startElement(String name,
                           UIComponent component) throws IOException
  {
    // =-=AEW Should we force all lowercase?
    if (name.charAt(0) == 's')
    {
      // Optimization (see bug 2009019): our code has a tendency
      // to write out unnecessary empty <span> elements.  So,
      // when we start a "span" HTML element, don't actually write
      // out anything just yet;  mark it pending.
      if ("span".equals(name))
      {
        // push any pending element onto the stack of skipped elements
        _markPendingElements();

        // make this the current pending element
        _pendingElement = name;
        return;
      }
      else if ("script".equals(name) ||
               "style".equals(name))
      {
        _dontEscape = true;
      }
    }

    // start writing the element
    _startElementImpl(name);
  }

  @Override
  public void endElement(String name) throws IOException
  {
    // eliminate any <pending></pending> combinations
    if (_pendingElement != null)
    {
      // we need to return immedediately because in this
      // case, the element was never pushed onto the
      // element stack.
      _pendingElement = null;
      return;
    }

    _closeStartIfNecessary();

    // get the name of the last outputted element
    String element = _popSkippedElement();

    // non-null names indicate that the element was ouput, so its
    // end tag should be output as well
    if (element != null)
    {
      if (!element.equals(name))
      {
        _LOG.severe("Element End name:"           +
                    name                          +
                    " does not match start name:" +
                    element);
      }

      _elementStack.removeFirst();
      Writer out = _out;

      // always turn escaping back on once an element ends
      _dontEscape = false;

      _depth--;
      _writeIndent(_depth);
      out.write("</");
      out.write(name);
      out.write('>');
    }
  }

  @Override
  public void writeAttribute(String name,
                             Object value,
                             String componentPropertyName)
        throws IOException
  {
    if (value == null)
      return;

    // if we have a pending element, flush it because
    // it has an attribute, and is thus needed
    _outputPendingElements();

    // =-=AEW Why are images being written out with writeAttribute()???
    if (_isCachedImage(value))
    {
      value = "/adf/images/cache/GENERATED_IMAGE.gif";
      // And if it's actually a "src", then we'll have to
      // muck with some other attributes too
      if ("src".equals(name))
        _isCachedImage = true;

    }

    if (_onlyValidIds && "id".equals(name))
    {
      if (!_VALID_ID_PATTERN.matcher(value.toString()).matches())
        _LOG.severe("ID \"" + value + "\" is not a valid ID.");
    }

    if (_attributes.containsKey(name))
    {
      AssertionFailedError failure = new AssertionFailedError(
        "Attribute " + name + " was written twice");
      _result.addError(_test, failure);
    }
    else if (_uriAttributes.containsKey(name))
    {
      AssertionFailedError failure = new AssertionFailedError(
        "Attribute " + name + " was written as both an attribute and a URI");
      _result.addError(_test, failure);
    }

    _attributes.put(name, value);
  }

  private void _writeAttributeImpl(String name, Object value)
    throws IOException
  {
    Writer out = _out;

    Class<?> valueClass = value.getClass();

    // Output Boolean values specially
    if (valueClass == _BOOLEAN_CLASS)
    {
      if (Boolean.TRUE.equals(value))
      {
        out.write(' ');
        out.write(name);
        out.write("=\"");
        out.write(name);
        out.write("\"");
      }
    }
    else
    {
      out.write(' ');
      out.write(name);
      out.write("=\"");

      // write the attribute value
      _writeValue(valueClass, value, true);
      out.write('"');
    }
  }

  static private boolean _isCachedImage(Object value)
  {
    if (value instanceof String)
    {
      String valueStr = (String) value;
      // Consider all generated images the same until we get
      // a strategy for ensuring that generated images have consistent
      // names from run to run
      return ((valueStr.indexOf("/adf/images/cache") >= 0) &&
              valueStr.endsWith(".gif"));
    }

    return false;
  }

  @Override
  public void writeURIAttribute(String name,
                                Object value,
                                String componentPropertyName)
    throws IOException
  {
    if (value == null)
      return;

    // if we have a pending element, flush it because
    // it has an attribute, and is thus needed
    _outputPendingElements();

    if (_isCachedImage(value))
    {
      value = "/adf/images/cache/GENERATED_IMAGE.gif";
      // And if it's actually a "src", then we'll have to
      // muck with some other attributes too
      if ("src".equals(name))
        _isCachedImage = true;
    }

    if (_attributes.containsKey(name))
    {
      AssertionFailedError failure = new AssertionFailedError(
        "Attribute " + name + " was written as both an attribute and a URI");
      _result.addError(_test, failure);
    }
    else if (_uriAttributes.containsKey(name))
    {
      AssertionFailedError failure = new AssertionFailedError(
        "Attribute " + name + " was written twice");
      _result.addError(_test, failure);
    }

    _uriAttributes.put(name, value);
  }

  @Override
  public void writeComment(Object comment) throws IOException
  {
    if (comment != null)
    {
      _closeStartIfNecessary();

      // skip comments to avoid spurious diffs unrelated to rendered output
    }
  }

  @Override
  public void writeText(Object text, String componentPropertyName)
     throws IOException
  {
    _checkText(text);

    if (text != null)
    {
      _closeStartIfNecessary();
      if (_dontEscape)
      {
        String textStr = _fixId(text.toString());
        _out.write(textStr);
      }
      else
      {
        XMLEscapes.writeText(_out, text.toString());
      }
    }
  }


  @Override
  public void writeText(char text[], int off, int len)
        throws IOException
  {
    _checkText(text);
    // Hardly efficient, but easier to write the one bottleneck.
    writeText(new String(text, off, len), null);
  }

  @Override
  public void write(char cbuf[], int off, int len) throws IOException
  {
    _closeStartIfNecessary();
    _out.write(cbuf, off, len);
  }

  @Override
  public void write(String str) throws IOException
  {
    _closeStartIfNecessary();
    _out.write(str);
  }

  @Override
  public void write(int c) throws IOException
  {
    _closeStartIfNecessary();
    _out.write((char) c);
  }

  @Override
  public ResponseWriter cloneWithWriter(Writer writer)
  {
    try
    {
      TestResponseWriter trw = new TestResponseWriter(writer, getContentType(),
                                                      getCharacterEncoding(),
                                                      _test,
                                                      _result);
      trw._depth = _depth;
      return trw;
    }
    catch (UnsupportedEncodingException e)
    {
      // this can't happen;  the character encoding should already
      // be legal.
      assert(false);
      throw new IllegalStateException();
    }
  }

  //
  // Private methods
  //

  private void _startElementImpl(String name) throws IOException
  {
    // close any previously stated element, if necessary
    _closeStartIfNecessary();

    // note that we started a non-skipped element
    _pushOutputtedElement(name);

    if (_testBlockLevel &&
        !_elementStack.isEmpty() &&
        _BLOCK_LEVEL_ELEMENTS.contains(name) &&
        _isInline(_elementStack.getFirst()))
    {
      _LOG.severe("The block level element " + name + " may not be used " +
                  "inside of the inline element " + _elementStack.getFirst());
    }

    _elementStack.addFirst(name);

    int depth = _depth++;
    _writeIndent(depth);

    Writer out = _out;
    out.write('<');
    out.write(name);
    _closeStart = true;

    _writeIndent(depth + 2);
  }


  /**
   * Writes the value of an object
   */
  private void _writeValue(
    Class<?>    valueClass,
    Object      value,
    boolean     isAttribute
    ) throws IOException
  {
    assert(valueClass != _CHAR_ARRAY_CLASS) :
           "Character arrays not supported as HTML attributes";

    if (valueClass == _INTEGER_CLASS)
    {
      // Integers never need to be escaped - and
      // we can cache common instances.
      _out.write(IntegerUtils.getString((Integer) value));

      return;
    }

    String valueString = value.toString();
    
    if (isAttribute)
    {
      XMLEscapes.writeAttribute(_out, valueString);
    }
    else
    {
      XMLEscapes.writeText(_out, valueString);
    }
  }

  private void _checkText(Object text)
  {
    if (text == null)
      throw new NullPointerException();
  }


  private void _closeStartIfNecessary() throws IOException
  {
    _markPendingElements();

    if (_closeStart)
    {
      Iterator<String> iter = _attributes.keySet().iterator();
      while (iter.hasNext())
      {
        String name = iter.next();
        Object value = _attributes.get(name);
        // Ignore FALSE values, since they won't be written out at all
        if (Boolean.FALSE.equals(value))
          continue;

        // Do not write out "width" and "height" on any cached images,
        // because they may vary from platform to platform.
        if (_isCachedImage &&
            ("width".equals(name) || "height".equals(name)))
        {
          value = "generated-" + name;
        }
        // And ditto for coords, though this assumes that coords
        // is only used for generated images (currently true)
        else if ("coords".equals(name))
        {
          value = "generated-coords";
        }
        else if ("id".equals(name) ||
                 "name".equals(name) ||
                 "for".equals(name) ||
                 "headers".equals(name))
        {
          value = _fixId(value.toString());
        }
        // And look for Javascript and values that contain IDs
        else if (name.startsWith("on") || "value".equals(name))
        {
          String valueStr = value.toString();
          if (valueStr.indexOf("_id") >= 0)
          {
            String re = "_id[0-9]+";
            String sub = "_idXXX";
            value = valueStr.replaceAll(re, sub);
          }
        }

        _writeAttributeImpl(name, value);
        _writeIndent(_depth + 1);
      }

      iter = _uriAttributes.keySet().iterator();
      while (iter.hasNext())
      {
        String name = iter.next();
        Object value = _uriAttributes.get(name);
        // "id" and "name" are treated as URI attrs in link,
        // and "href" sometimes contains "#..." links to IDs
        if ("id".equals(name) ||
            "name".equals(name) ||
            "href".equals(name))
        {
          value = _fixId(value.toString());
        }

        _writeAttributeImpl(name, "uri-attr:" + value);
        _writeIndent(_depth + 1);
      }

      _attributes.clear();
      _uriAttributes.clear();
      _isCachedImage = false;

      _out.write('>');
      _writeIndent(_depth);
      _closeStart = false;
    }
  }

  // Find the old "unique ID" code in UIX 2.2 land,
  // and force them all to be the same to work past some
  // spurious diffs
  private String _fixId(String valueStr)
  {
    // All our golden files were produced with "_id" in JSF 1.1;
    // and now in 1.2, that's "j_id".  Eliminate this diff.
    if (valueStr.indexOf("j_id") >= 0)
      valueStr = valueStr.replaceAll("j_id", "_id");

    if (valueStr.indexOf("_id") >= 0)
    {
      String re = "_id[0-9]+";
      String sub = "_idXXX";
      return valueStr.replaceAll(re, sub);
    }
    else if (valueStr.indexOf("M__Id") >= 0)
    {
      String re = "M__Id[a-z]*";
      String sub = "_idXXX";
      return valueStr.replaceAll(re, sub);
    }

    return valueStr;
  }

  /**
   * Flushes out any pending element, celaring the pending
   * entry.
   */
  private void _outputPendingElements() throws IOException
  {
    String pendingElement = _pendingElement;

    if (pendingElement != null)
    {
      // we clear the pending element BEFORE calling
      // startElementImpl to prevent _startElementImpl's indirect call
      // to _markPendingElements from pushing our element onto
      // the skipped stack, imbalancing the stack
      _pendingElement = null;

      // start the pending element
      _startElementImpl(pendingElement);
    }
  }

  /**
   * If an element is pending, push it onto the stack of skipped
   * elements because it doesn't have any attributes
   */
  private void _markPendingElements()
  {
    String pendingElement = _pendingElement;
    if (pendingElement != null)
    {
      _pushSkippedElement();
      _pendingElement = null;
    }
  }


  /**
   * Retrieves the name of the last output element.  If it is null,
   * that element was skipped and thus its end tag should be suppressed
   * as well
   */
  private String _popSkippedElement()
  {
    int size = _skippedElements.size();
    if (size == 0)
      return null;

    return _skippedElements.remove(size - 1);
  }

  /**
   * Marks the skipped element so that the output of its
   * end tag can also be suppressed
   */
  private void _pushSkippedElement()
  {
    _skippedElements.add(null);
  }


  /**
   * Marks that we have outputted a real element so that the ordering of
   * the outputted and skipped elements can be maintained.  This
   * also aids debuggin by ensuring that the element being ended matches
   * the actual ouputted name.
   */
  private void _pushOutputtedElement(
    String name
    )
  {
    _skippedElements.add(name);
  }

  private void _writeIndent(int depth) throws IOException
  {
    depth = depth * _SPACES_PER_LEVEL;
    if (depth > _MAX_INDENT)
      depth = _MAX_INDENT;

    _out.write('\n');

    // If depth goes negative, this isn't just a no-op, it's
    // death.
    if (depth > 0)
    {
      _out.write(_sSpaces, 0, depth);
    }

  }

  static private boolean _isInline(String name)
  {
    return (!_BLOCK_LEVEL_ELEMENTS.contains(name) &&
            !_NEUTRAL_ELEMENTS.contains(name));
  }

  private boolean     _testBlockLevel;
  private boolean     _onlyValidIds;

  private boolean     _closeStart;
  private boolean     _dontEscape;

  private boolean _isCachedImage;
  private Map<String, Object> _attributes = new TreeMap<String, Object>();
  private Map<String, Object> _uriAttributes = new TreeMap<String, Object>();
  private LinkedList<String> _elementStack = new LinkedList<String>();
  private int     _depth;

  private Writer       _out;
  private String       _encoding;

  // holds an element that will only be started if it has attributes
  private String      _pendingElement;

  private Test        _test;
  private TestResult  _result;

  // stack of skipped and unskipped elements used to determine when
  // to suppress the end tag of a skipped element
  private final ArrayList<String> _skippedElements = new ArrayList<String>(20);


  private static final Pattern _VALID_ID_PATTERN  =
    Pattern.compile("[A-Za-z][A-Za-z0-9:_.-]*");

  private static final Class<?> _CHAR_ARRAY_CLASS = (new char[0]).getClass();
  private static final Class<Boolean> _BOOLEAN_CLASS = Boolean.class;
  private static final Class<Integer> _INTEGER_CLASS = Integer.class;

  static private final int _MAX_INDENT = 50;
  static private final int _SPACES_PER_LEVEL = 2;

  static private final char[] _sSpaces;

  static
  {
    _sSpaces = new char[_MAX_INDENT];

    for (int i = 0; i < _MAX_INDENT; i++)
    {
      _sSpaces[i] = ' ';
    }
  }

  static private final Set<String> _BLOCK_LEVEL_ELEMENTS =
    new HashSet<String>();
  static private final Set<String> _NEUTRAL_ELEMENTS =
    new HashSet<String>();
  {
    _BLOCK_LEVEL_ELEMENTS.add("address");
    _BLOCK_LEVEL_ELEMENTS.add("blockquote");
    _BLOCK_LEVEL_ELEMENTS.add("center");
    _BLOCK_LEVEL_ELEMENTS.add("dir");
    _BLOCK_LEVEL_ELEMENTS.add("div");
    _BLOCK_LEVEL_ELEMENTS.add("dl");
    _BLOCK_LEVEL_ELEMENTS.add("fieldset");
    _BLOCK_LEVEL_ELEMENTS.add("form");
    _BLOCK_LEVEL_ELEMENTS.add("h1");
    _BLOCK_LEVEL_ELEMENTS.add("h2");
    _BLOCK_LEVEL_ELEMENTS.add("h3");
    _BLOCK_LEVEL_ELEMENTS.add("h4");
    _BLOCK_LEVEL_ELEMENTS.add("h5");
    _BLOCK_LEVEL_ELEMENTS.add("h6");
    _BLOCK_LEVEL_ELEMENTS.add("hr");
    _BLOCK_LEVEL_ELEMENTS.add("isindex");
    _BLOCK_LEVEL_ELEMENTS.add("menu");
    _BLOCK_LEVEL_ELEMENTS.add("noframes");
    _BLOCK_LEVEL_ELEMENTS.add("noscript");
    _BLOCK_LEVEL_ELEMENTS.add("ol");
    _BLOCK_LEVEL_ELEMENTS.add("p");
    _BLOCK_LEVEL_ELEMENTS.add("pre");
    _BLOCK_LEVEL_ELEMENTS.add("table");
    _BLOCK_LEVEL_ELEMENTS.add("ul");
    // These technically aren't block-level, but are allowed
    // to contain block-level elements
    _BLOCK_LEVEL_ELEMENTS.add("dd");
    _BLOCK_LEVEL_ELEMENTS.add("dt");
    _BLOCK_LEVEL_ELEMENTS.add("frameset");
    _BLOCK_LEVEL_ELEMENTS.add("li");
    _BLOCK_LEVEL_ELEMENTS.add("tbody");
    _BLOCK_LEVEL_ELEMENTS.add("tfoot");
    _BLOCK_LEVEL_ELEMENTS.add("th");
    _BLOCK_LEVEL_ELEMENTS.add("thead");
    _BLOCK_LEVEL_ELEMENTS.add("tr");
    _BLOCK_LEVEL_ELEMENTS.add("td");
    _NEUTRAL_ELEMENTS.add("applet");
    _NEUTRAL_ELEMENTS.add("body");
    _NEUTRAL_ELEMENTS.add("button");
    _NEUTRAL_ELEMENTS.add("del");
    _NEUTRAL_ELEMENTS.add("iframe");
    _NEUTRAL_ELEMENTS.add("ins");
    _NEUTRAL_ELEMENTS.add("map");
    _NEUTRAL_ELEMENTS.add("object");
    _NEUTRAL_ELEMENTS.add("script");
  }
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TestResponseWriter.class);
}
