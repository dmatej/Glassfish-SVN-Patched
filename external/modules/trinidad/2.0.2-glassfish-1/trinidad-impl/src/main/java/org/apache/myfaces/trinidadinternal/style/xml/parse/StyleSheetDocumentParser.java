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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.io.File;
import java.io.IOException;

import java.net.URL;
import java.net.URLConnection;

import java.util.Vector;
import java.util.Iterator;
import java.util.ArrayList;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;



import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.URLUtils;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;

import org.apache.myfaces.trinidadinternal.style.StyleConstants;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;

/**
 * NodeParser for style sheet document nodes. This parses the XSS file.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleSheetDocumentParser.java#0 $) $Date: 10-nov-2005.18:58:46 $
 */
public class StyleSheetDocumentParser extends BaseNodeParser
  implements XMLConstants, StyleConstants
{
  /**
   * Implementation of NodeParser.endElement()
   */
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    _documentVersion = attrs.getValue(DOCUMENT_VERSION_ATTR);
    // If the document version is ${trinidad-version}, replace it
    // with the version number right out of our manifest
    if ("${trinidad-version}".equals(_documentVersion))
    {
      Class<StyleSheetDocumentParser> implClass = StyleSheetDocumentParser.class;
      Package implPkg = implClass.getPackage();
      if ((implPkg != null) && (implPkg.getImplementationVersion() != null))
      {
        _documentVersion = implPkg.getImplementationVersion().replace('.','_');
      }
      else
      {
        _documentVersion = "unknown-version";
      }
    }
  }

  /**
   * Implementation of NodeParser.endElement()
   */
  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    )
  {
    StyleSheetNode[] styleSheets = _getStyleSheets();
    String documentVersion = _getDocumentVersion();
    long documentTimestamp = _getDocumentTimestamp(context);

    return new StyleSheetDocument(styleSheets,
                                  documentVersion,
                                  documentTimestamp);
  }

  /**
   * Implementation of NodeParser.startChildElement()
   */
  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    if (localName.equals(STYLE_SHEET_NAME))
    {
      return context.getParser(
        StyleSheetNode.class,
        namespaceURI,
        localName
        );
    }
    else if (localName.equals(IMPORT_NAME))
    {
      String href = attrs.getValue(HREF_ATTR);
      try
      {
        _handleImport(context, href);
      }
      catch (IOException e)
      {
        if (_LOG.isWarning())
          _LOG.warning("CANNOT_PARSE_IMPORT", href);
          _LOG.warning(e);
      }

      return this;
    }

    return null;
  }

  /**
   * Implementation of NodeParser.addCompletedChild().
   */
  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    )
  {
    if (child != null)
    {
      if (localName.equals(STYLE_SHEET_NAME))
      {
        if (_styleSheets == null)
          _styleSheets = new Vector<StyleSheetNode>();

        _styleSheets.addElement((StyleSheetNode)child);
      }
    }
  }

  // Return all style sheet nodes - this includes imported style sheets
  // as well as style sheets from this document.  Imported style
  // sheets have lower precedence - they come first.
  private StyleSheetNode[] _getStyleSheets()
  {
    StyleSheetNode[] styleSheets = null;

    if (_imports == null)
    {
      if (_styleSheets != null)
      {
        styleSheets = new StyleSheetNode[_styleSheets.size()];
        _styleSheets.copyInto(styleSheets);
      }
    }
    else
    {
      // If we've got imported documents, copy all style sheets
      // into a Vector first.
      // -= Simon Lessard =-
      // TODO: Check if synchronization is truly required.
      Vector<StyleSheetNode> v = new Vector<StyleSheetNode>();
      for(StyleSheetDocument doc : _imports)
      {
        Iterator<StyleSheetNode> e = doc.getStyleSheets();
        while (e.hasNext())
          v.addElement(e.next());
      }

      // Might as well add the rest of the style sheets in now too
      if (_styleSheets != null)
      {
        for (int i = 0; i < _styleSheets.size(); i++)
          v.addElement(_styleSheets.elementAt(i));
      }

      // Now, copy everything into a single array
      styleSheets = new StyleSheetNode[v.size()];
      v.copyInto(styleSheets);
    }

    return styleSheets;
  }


  // Handle an import - parse the imported XSS document
  private void _handleImport(
    ParseContext context,
    String href
    ) throws SAXParseException, IOException
  {
    if (href == null)
    {
      _LOG.warning("MISSING_REQUIRED_HREF");
      return;
    }

    // Now, parse the imported document
    StyleSheetDocument doc = null;

    try
    {
      doc = (StyleSheetDocument)XMLUtils.parseInclude(context,
                                                    href,
                                                    StyleSheetDocument.class);
    }
    catch (SAXException e)
    {
      _LOG.severe(e);
    }

    if (doc != null)
    {
      if (_imports == null)
        _imports = new ArrayList<StyleSheetDocument>();

      _imports.add(doc);
    }
  }



  // Returns the document version for this style sheet, which
  // includes versions specified by any imported style sheets.
  private String _getDocumentVersion()
  {
    StringBuffer buffer = new StringBuffer();

    // Start with the version from this document
    if (_documentVersion != null)
      buffer.append(_documentVersion);

    // Tack on versions from imported style sheets
    if (_imports != null)
    {
      for(StyleSheetDocument document : _imports)
      {
        String documentVersion = document.getDocumentVersion();
        if (documentVersion != null)
          buffer.append(documentVersion);
      }
    }

    if (buffer.length() > 0)
      return buffer.toString();

    return null;
  }

  // Returns the document timestamp for the style sheet that
  // is currently being parsed, taking into account timestamps
  // of any imported style sheets.
  private long _getDocumentTimestamp(ParseContext context)
  {
    long timestamp = StyleSheetDocument.UNKNOWN_TIMESTAMP;

    // The only way to get the timestamp is through the
    // InputStreamProvider.
    InputStreamProvider provider = XMLUtils.getInputStreamProvider(context);

    if (provider != null)
    {
      // And this only works if we are using a File-based or URL-based InputStream
      Object identifier = provider.getIdentifier();
      if (identifier instanceof File)
      {
        timestamp = ((File)identifier).lastModified();
      }
      else if (identifier instanceof URL)
      {
        try
        {
          timestamp = URLUtils.getLastModified((URL)identifier);
        }
        catch (IOException io)
        {
          _LOG.warning("CANNOT_GET_STYLESHEET_DOCUMENT_TIMESTAMP");
        }

      }
    }

    // Merge in timestamps of imported style sheets
    if (_imports != null)
    {
      for(StyleSheetDocument document : _imports)
      {
        long importTimestamp = document.getDocumentTimestamp();
        if (importTimestamp > timestamp)
          timestamp = importTimestamp;
      }
    }

    return timestamp;
  }

  // -= Simon Lessard =-
  // TODO: Check if synchronization is truly required
  private Vector<StyleSheetNode> _styleSheets;   // Vector of StyleSheetNode
  private ArrayList<StyleSheetDocument> _imports;       // Vector of imported StyleSheetDocument
  private String _documentVersion; // Version identifier for the document

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StyleSheetDocumentParser.class);
}
