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
package org.apache.myfaces.trinidadinternal.share.xml;

import java.io.IOException;

import java.util.Map;
import java.util.Stack;

import java.util.logging.Level;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Class responsible for building a tree of objects from
 * an XML stack.  TreeBuilders are thread safe, and so can
 * be used from multiple threads simultaneously.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/TreeBuilder.java#0 $) $Date: 10-nov-2005.18:59:16 $
 */
public class TreeBuilder
{
  /**
   * Creates a TreeBuilder with no ParserManager or root class.
   * Such a TreeBuilder can only be used with NodeParsers that never
   * try to use {@link org.apache.myfaces.trinidadinternal.share.xml.ParseContext#getParser ParseContext.getParser()}.
   */
  public TreeBuilder()
  {
    this(null, null);
  }

  /**
   * Creates a TreeBuilder using a given ParserManager.
   * @param manager the ParserManager to use
   * @param rootClass the desired type of object to return as the root
   */
  public TreeBuilder(ParserManager manager,
                     Class<?>      rootClass)
  {
    // Allow a null ParserManager.  It may sound wacky,
    // but it's sometimes easiest to always explicitly state
    // which parser you want to use, and never rely on type information.
    if (manager == null)
      _manager = new ParserManager();
    else
      // Clone the ParserManager, so changes to it will not
      // "escape" - see bug 1852367
      _manager   = (ParserManager) manager.clone();

    _rootClass = rootClass;
  }


  public void setEntityResolver(EntityResolver resolver)
  {
    _entityResolver = resolver;
  }

  public EntityResolver getEntityResolver()
  {
    return _entityResolver;
  }

  /**
   * Parses the document.
   * @param provider an implementation of the XMLProvider interface
   * @param source a SAX input source
   * @return an object that is an instance of the desired class
   */
  public Object parse(
    XMLProvider provider,
    InputSource source)
    throws IOException, SAXException
  {
    ParseContextImpl context = new ParseContextImpl();
    return parse(provider, source, context);
  }


  /**
   * Parses the document.
   * @param provider an implementation of the XMLProvider interface
   * @param source a SAX input source
   * @param context a parsing context
   * @return an object that is an instance of the desired class
   */
  public Object parse(
    XMLProvider      provider,
    InputSource      source,
    ParseContext     context) throws IOException, SAXException
  {
    return parse(provider, source, context, null);
  }

  public Object parse(InputSource source,
                      NodeParser rootParser) throws IOException, SAXException
  {
    return parse(null, source, new ParseContextImpl(), rootParser);
  }

  /**
   * Parses the document.
   * @param provider an implementation of the XMLProvider interface
   * @param source a SAX input source
   * @param context a parsing context
   * @param rootParser the root parser to start with;  if null,
   *         a root parser will be derived based on the rootClass
   *         requested in the constructor.
   * @return an object that is the result of parsing.
   */
  public Object parse(
    XMLProvider      provider,
    InputSource      source,
    ParseContext     context,
    NodeParser       rootParser) throws IOException, SAXException
  {
    if ((_rootClass == null) && (rootParser == null))
      throw new NullPointerException(_LOG.getMessage(
        "NULL_ROOTCLASS_ROOTPARSER"));

    if (provider == null)
      provider = new JaxpXMLProvider();

    ParseContextWrapper wrappedContext = new ParseContextWrapper(context,
                                                                 _manager,
                                                                 provider);
    Handler handler = new Handler(wrappedContext, rootParser);
    XMLReader reader = provider.getXMLReader();

    // Force these two features to be set the way we want.
    // These are the default values, but we'd crash and burn
    // if they're wrong.
    reader.setFeature("http://xml.org/sax/features/namespaces", true);
    reader.setFeature("http://xml.org/sax/features/namespace-prefixes", false);

    reader.setContentHandler(handler);
    reader.setErrorHandler(handler);
    if (getEntityResolver() != null)
      reader.setEntityResolver(getEntityResolver());

    reader.parse(source);

    return handler.getRoot();
  }


  // Workhorse class;  contains all the parse-specific state
  private class Handler implements ContentHandler, ErrorHandler
  {
    public Handler(ParseContextWrapper context, NodeParser rootParser)
    {
      _context = context;
      _parsers = new Stack<StackEntry>();
      _rootParser = rootParser;
    }

    /**
     * Returns the root object;  can only be called after a parse
     * has finished.  The root object will be an instance of
     * the requested root class.
     */
    public Object getRoot()
    {
      if (!_parsers.isEmpty())
        throw new IllegalStateException();

      return _root;
    }

    /**
     * Begin the scope of a prefix-URI namespace mapping.
     */
    public void startPrefixMapping(
      String prefix,
      String uri)
    {
      _context.__addPrefixMapping(prefix, uri);
    }

    public void endPrefixMapping(String prefix)
    {
    }

    /**
     * Receive notification of the beginning of a document.
     */
    public void startDocument()
    {
      _parsers.setSize(0);
      _root = null;
    }


    /**
     * Receive notification of the end of a document.
     */
    public void endDocument()
    {
      assert (_parsers.isEmpty());
    }

    /**
     * Receive notification of the beginning of an element.
     */
    public void startElement(
      String namespaceURI,
      String localName,
      String rawName,
      Attributes atts) throws SAXException
    {
      _context.__startElement();

      NodeParser parser = _getCurrentParser();
      if (parser == null)
      {
        // No parser yet - we're starting at the top.  Get
        // a default parser
        if (_rootParser != null)
          parser = _rootParser;
        else
          parser = _context.getParser(_rootClass, namespaceURI, localName);

        if (parser == null)
        {
          String message = "No " + _rootClass.getName() +
                           " parser registered for top element;" +
                           "check your namespace declaration." +
                           "   Namespace: " + namespaceURI + "\n" +
                           "   Local name: " + localName + ")";

          SAXParseException e =
              new SAXParseException(message,
                                    _context.getLocator());
          fatalError(e);
          throw e;
        }

        _pushParser(parser, false);
      }
      else
      {
        // Notify the current parser that we're starting a child element.
        // If it returns null, we'll continue talking to the same
        // parser, and only call startChildElement() and endChildElement().
        NodeParser newParser;
        try
        {
          newParser = parser.startChildElement(_context,
                                               namespaceURI,
                                               localName,
                                               atts);
        }
        catch (SAXParseException e)
        {
          // Log the error, and bail
          fatalError(e);
          throw e;
        }

        // The NodeParser will continue handling parsing itself.
        // Note that on the stack, and bail.
        if (newParser == parser)
        {
          _pushParser(null, false);
          return;
        }
        else
        {
          boolean isExtension = false;
          if (newParser == null)
          {
            newParser = _processElementExtensions(namespaceURI,
                                                  localName,
                                                  atts);
            if (newParser != null)
            {
              isExtension = true;
            }
            else
            {
              ParseErrorUtils.log(_context,
                                  "<" + localName + ">" +
                                  " is not an understood element.\n" +
                                  "This sometimes means the element's " +
                                  "namespace (" + namespaceURI + ") is set " +
                                  "incorrectly.  This may also be an " +
                                  "issue with the syntax of its parent " +
                                  "element.",
                                  null,
                                  Level.WARNING,
                                  _LOG);
              newParser = BaseNodeParser.getIgnoreParser();
            }
          }

          _pushParser(newParser, isExtension);
          parser = newParser;
        }
      }

      // Start the (non-child) element
      try
      {
        _processAttributeExtensions(namespaceURI, localName, atts);
        parser.startElement(_context, namespaceURI, localName, atts);
      }
      catch (SAXParseException e)
      {
        // Log the error, and bail
        fatalError(e);
        throw e;
      }
    }

    private NodeParser _processElementExtensions(
      String     namespaceURI,
      String     localName,
      Attributes atts) throws SAXParseException
    {
      ParserExtension extension = _context.getExtension(namespaceURI);
      if (extension != null)
      {
        return extension.startExtensionElement(_context,
                                               namespaceURI,
                                               localName,
                                               atts);
      }

      return null;
    }


    private void _processAttributeExtensions(
      String     elementNamespaceURI,
      String     elementLocalName,
      Attributes atts) throws SAXParseException
    {
      StackEntry entry = null;
      int length = atts.getLength();
      for (int i = 0; i < length; i++)
      {
        String namespaceURI = atts.getURI(i);
        if ((namespaceURI == null) || (namespaceURI.length() == 0))
          continue;

        ParserExtension extension = _context.getExtension(namespaceURI);
        if (extension == null)
          continue;

        String localName = atts.getLocalName(i);
        String value = atts.getValue(i);

        if (value != null)
        {
          if (entry == null)
            entry = _getLastNonNullStackEntry();

          entry.put(namespaceURI, localName, value);
        }
      }

      if (entry != null)
      {
        int count = entry.getMapCount();
        for (int i = 0; i < count; i++)
        {
          String namespaceURI = entry.getMapNamespace(i);
          ParserExtension extension = _context.getExtension(namespaceURI);
          assert (extension != null);

          extension.elementStarted(_context,
                                   elementNamespaceURI,
                                   elementLocalName,
                                   entry.getMap(i));
        }
      }
    }


    private Object _finishExtensions(
      String     elementNamespaceURI,
      String     elementLocalName,
      StackEntry entry,
      Object     child) throws SAXParseException
    {
      int count = entry.getMapCount();
      for (int i = 0; i < count; i++)
      {
        String namespaceURI = entry.getMapNamespace(i);
        ParserExtension extension = _context.getExtension(namespaceURI);
        assert (extension != null);

        child = extension.elementEnded(_context,
                                       elementNamespaceURI,
                                       elementLocalName,
                                       child,
                                       entry.getMap(i));
      }

      return child;
    }

    /**
     * Receive notification of the end of an element.
     */
    public void endElement(
      String namespaceURI,
      String localName,
      String rawName) throws SAXException
    {
      StackEntry oldEntry = _popParser();

      NodeParser oldParser;
      boolean    isExtension;
      if (oldEntry != null)
      {
        isExtension = oldEntry.isExtension();
        oldParser = oldEntry.getParser();
      }
      else
      {
        isExtension = false;
        oldParser   = null;
      }

      NodeParser parser = _getCurrentParser();

      // If the "old parser" was null, then that the "parser"
      // is handling its own children.  Otherwise, we need to
      // terminate the element, and pass the Java object the "old"
      // parser produced to "addCompletedChild()" on the parser
      if (oldParser != null)
      {
        Object child;

        try
        {
          child = oldParser.endElement(_context,
                                        namespaceURI,
                                        localName);
          child = _finishExtensions(namespaceURI, localName, oldEntry, child);
        }
        catch (SAXParseException e)
        {
          // Log the error, and bail
          fatalError(e);
          throw e;
        }

        if (parser == null)
        {
          // If there's no current parser, we must be at
          // the end.  Store the root, and be done.
          assert (_parsers.isEmpty());
          _root = child;
        }
        else
        {
          try
          {
            if (isExtension)
            {
              StackEntry currentEntry = _getLastNonNullStackEntry();
              currentEntry.put(namespaceURI, localName, child);
            }
            else
            {
              parser.addCompletedChild(_context,
                                       namespaceURI,
                                       localName,
                                       child);
            }

          }
          catch (SAXParseException e)
          {
            // Log the error, and bail
            fatalError(e);
            throw e;
          }
        }
      }

      if ((parser != null) && (oldParser == null))
      {
        try
        {
          parser.endChildElement(_context, namespaceURI, localName);
        }
        catch (SAXParseException e)
        {
          // Log the error, and bail
          fatalError(e);
          throw e;
        }
      }

      _context.__endElement();
    }

    /**
     * Receive notification of character data.
     */
    public void characters(
      char[] text,
      int    start,
      int    length) throws SAXException
    {
      try
      {
        NodeParser parser = _getCurrentParser();
        if (parser != null)
        {
          int savedStart = start;
          int end = start + length - 1;

          // Trim whitespace from both ends of the text.
          while (start <= end)
          {
            if (!Character.isWhitespace(text[start]))
              break;

            start++;
          }

          if (start != savedStart)
          {
            parser.addWhitespace(_context, text, savedStart, start - savedStart);
          }

          int savedEnd = end;
          while (end > start + 1)
          {
            if (!Character.isWhitespace(text[end]))
              break;

            end--;
          }

          if (end >= start)
          {
            parser.addText(_context, text, start, end - start + 1);
            if (savedEnd != end)
              parser.addWhitespace(_context, text, end + 1, savedEnd - end);
          }
        }
      }
      catch (SAXParseException e)
      {
        // Log the error, and bail
        fatalError(e);
        throw e;
      }
    }


    public void setDocumentLocator(Locator locator)
    {
      _context.__setLocator(locator);
    }

    /**
     * Receive notification of ignorable whitespace in element content.
     */
    public void ignorableWhitespace(
      char[] text,
      int    start,
      int    length) throws SAXException
    {
      try
      {
        NodeParser parser = _getCurrentParser();
        if (parser != null)
        {
          parser.addWhitespace(_context, text, start, length);
        }
      }
      catch (SAXParseException e)
      {
        // Log the error, and bail
        fatalError(e);
        throw e;
      }
    }

    /**
     * Receive notification of a processing instruction.
     */
    public void processingInstruction(
      String target,
      String data)
    {
    }

    /**
     * Receive notification of a skipped entity.
     */
    public void skippedEntity(String name)
    {
    }


    //
    // ErrorHandler implementation
    //

    /**
     * Receive notification of a warning.
     */
    public void warning(SAXParseException exception) throws SAXException
    {
      _logError(exception, Level.INFO);
    }

    /**
     * Receive notification of a recoverable error.
     */
    public void error(SAXParseException exception) throws SAXException
    {
      _logError(exception, Level.WARNING);
    }

    /**
     * Receive notification of a fatal error.
     */
    public void fatalError(SAXParseException exception) throws SAXException
    {
      if (!_fatalError)
      {
        _fatalError = true;
        _logError(exception, Level.SEVERE);
      }
    }

    private void _logError(SAXParseException exception, Level verbosity)
    {
      ParseErrorUtils.log(_context,
                          null,
                          exception,
                          exception.getLineNumber(),
                          exception.getColumnNumber(),
                          exception.getSystemId(),
                          verbosity,
                          _LOG);
    }

    private void _pushParser(NodeParser parser, boolean isExtension)
    {
      if (parser == null)
      {
        _parsers.push(null);
      }
      else
      {
        _parsers.push(new StackEntry(parser, isExtension));
        _current = parser;
      }

      assert (_current == _getLastNonNullParser());
    }

    private StackEntry _popParser()
    {
      StackEntry entry = _parsers.pop();
      if (entry != null)
      {
        _current = _getLastNonNullParser();
        return entry;
      }
      else
      {
        return null;
      }
    }

    private NodeParser _getCurrentParser()
    {
      assert (_current == _getLastNonNullParser());
      return _current;
    }

    private StackEntry _getLastNonNullStackEntry()
    {
      for (int i = _parsers.size() - 1; i >= 0; i--)
      {
        Object entry = _parsers.elementAt(i);
        if (entry != null)
        {
          return (StackEntry) entry;
        }
      }

      return null;
    }

    private NodeParser _getLastNonNullParser()
    {
      StackEntry entry = _getLastNonNullStackEntry();
      if (entry != null)
        return entry.getParser();

      return null;
    }


    private ParseContextWrapper _context;

    // A stack of parsers (stored in StackEntries);  null at any position means the
    // parser didn't change
    // -= Simon Lessard =- 
    // TODO:  Check if synchronization is required since Stack is bad.
    private Stack<StackEntry> _parsers;

    // The current parser.  This will always equal the last
    // non-null parser
    private NodeParser      _current;

    // The root
    private Object           _root;

    // Has a fatal error already been logged?  Used to work around
    // the Oracle parser's tendency to fire the same fatal error
    // multiple times
    private boolean          _fatalError;

    // The parser to use as the root of the tree
    private final NodeParser _rootParser;
  }

  //
  // StackEntry objects store:
  //   (1) The NodeParser
  //   (2) A set of Dictionaries, one per namespace, used for storing
  //       extension's results
  //
  static private class StackEntry
  {
    public StackEntry(NodeParser parser, boolean isExtension)
    {
      _parser = parser;
      _isExtension = isExtension;
    }

    public NodeParser getParser()
    {
      return _parser;
    }

    public boolean isExtension()
    {
      return _isExtension;
    }

    // Get the number of Dictionaries
    public int getMapCount()
    {
      if (_dictionaries == null)
        return 0;

      return _dictionaries.length / 2;
    }

    // Get the namespace of the index'th dictionary
    public String getMapNamespace(int index)
    {
      return (String) _dictionaries[index * 2];
    }

    // Get the index'th dictionary
    @SuppressWarnings("unchecked")
    public Map<Object, Object> getMap(int index)
    {
      return (Map<Object, Object>) _dictionaries[index * 2 + 1];
    }

    // Add a namespace/key value to the entry
    @SuppressWarnings("unchecked")
    public void put(String namespace, Object key, Object value)
    {
      Map<Object, Object> subDict = 
        (Map<Object, Object>) ArrayMap.get(_dictionaries, namespace);
      
      if (subDict == null)
      {
        subDict = new ArrayMap<Object, Object>(5, 5);
        _dictionaries = ArrayMap.put(_dictionaries, namespace, subDict);
      }

      subDict.put(key, value);
    }

    private NodeParser _parser;
    private boolean    _isExtension;
    private Object[]   _dictionaries;
  }

  final Class<?>               _rootClass;
  private EntityResolver       _entityResolver;
  final private ParserManager  _manager;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TreeBuilder.class);
}
