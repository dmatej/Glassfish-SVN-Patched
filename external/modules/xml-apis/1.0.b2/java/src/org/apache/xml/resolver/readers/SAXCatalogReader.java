// SAXCatalogReader.java - Read XML Catalog files

/*
 * The Apache Software License, Version 1.1
 *
 *
 * Copyright (c) 2001 The Apache Software Foundation.  All rights 
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:  
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Xalan" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written 
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation and was
 * originally based on software copyright (c) 2001, International
 * Business Machines Corporation., http://www.ibm.com.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

package org.apache.xml.resolver.readers;

import java.lang.Integer;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Enumeration;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.MalformedURLException;
import java.net.UnknownHostException;
import org.apache.xml.resolver.Catalog;
import org.apache.xml.resolver.CatalogEntry;
import org.apache.xml.resolver.CatalogException;
import org.apache.xml.resolver.readers.CatalogReader;
import org.apache.xml.resolver.helpers.Debug;
import org.apache.xml.resolver.helpers.PublicId;
import org.apache.xml.resolver.helpers.Namespaces;

import org.xml.sax.*;

import javax.xml.parsers.*;

/**
 * <p>A SAX-based CatalogReader</p>
 *
 * <p>This class is used to read XML Catalogs using the SAX. This reader
 * has an advantage over the DOM-based reader in that it functions on
 * the stream of SAX events. It has the disadvantage
 * that it cannot look around in the tree.</p>
 *
 * <p>Since the choice of CatalogReaders (in the InputStream case) can only
 * be made on the basis of MIME type, the following problem occurs: only
 * one CatalogReader can exist for all XML mime types. In order to get
 * around this problem, the SAXCatalogReader relies on a set of external
 * CatalogParsers to actually build the catalog.</p>
 *
 * <p>The selection of CatalogParsers is made on the basis of the QName
 * of the root element of the document.</p>
 *
 * @see Catalog
 * @see CatalogReader
 * @see SAXCatalogReader
 * @see TextCatalogReader
 * @see DOMCatalogParser
 *
 * @author Norman Walsh
 * <a href="mailto:Norman.Walsh@Sun.COM">Norman.Walsh@Sun.COM</a>
 *
 * @version 1.0
 */
public class SAXCatalogReader implements CatalogReader, ContentHandler, DocumentHandler {
  /** The SAX Parser Factory */
  protected SAXParserFactory parserFactory = null;

  /** The SAX Parser Class */
  protected String parserClass = null;

  /**
     * <p>Mapping table from QNames to CatalogParser classes.</p>
     *
     * <p>Each key in this hash table has the form "elementname"
     * or "{namespaceuri}elementname". The former is used if the
     * namespace URI is null.</p>
     */
  protected Hashtable namespaceMap = new Hashtable();

  /** The parser in use for the current catalog. */
  private SAXCatalogParser saxParser = null;

  /** Set if something goes horribly wrong. It allows the class to
     * ignore the rest of the events that are received.
     */
  private boolean abandonHope = false;

  /** The Catalog that we're working for. */
  private Catalog catalog;

  /** Set the XML SAX Parser Factory.
   */
  public void setParserFactory(SAXParserFactory parserFactory) {
    this.parserFactory = parserFactory;
  }

  /** Set the XML SAX Parser Class
   */
  public void setParserClass(String parserClass) {
    this.parserClass = parserClass;
  }

  /** Get the parser factory currently in use. */
  public SAXParserFactory getParserFactory() {
    return parserFactory;
  }

  /** Get the parser class currently in use. */
  public String getParserClass() {
    return parserClass;
  }

  /** Set the SAXCatalogParser class for the given namespace/root
     * element type.
     */
  public void setCatalogParser(String namespaceURI,
			       String rootElement,
			       String parserClass) {
    if (namespaceURI == null) {
      namespaceMap.put(rootElement, parserClass);
    } else {
      namespaceMap.put("{"+namespaceURI+"}"+rootElement, parserClass);
    }
  }

  /** Get the SAXCatalogParser class for the given namespace/root
     * element type.
     */
  public String getCatalogParser(String namespaceURI,
				 String rootElement) {
    if (namespaceURI == null) {
      return (String) namespaceMap.get(rootElement);
    } else {
      return (String) namespaceMap.get("{"+namespaceURI+"}"+rootElement);
    }
  }

  /** The constructor */
  public SAXCatalogReader() {
    parserFactory = null;
    parserClass = null;
  }

  /** The constructor */
  public SAXCatalogReader(SAXParserFactory parserFactory) {
    this.parserFactory = parserFactory;
  }

  /** The constructor */
  public SAXCatalogReader(String parserClass) {
    this.parserClass = parserClass;
  }

  /**
   * <p>Parse an XML Catalog file.</p>
   *
   * @param catalog The catalog to which this catalog file belongs
   * @param fileUrl The URL or filename of the catalog file to process
   *
   * @throws MalformedURLException Improper fileUrl
   * @throws IOException Error reading catalog file
   */
  public void readCatalog(Catalog catalog, String fileUrl)
    throws MalformedURLException, IOException,
	   CatalogException {

    URL url = null;

    try {
      url = new URL(fileUrl);
    } catch (MalformedURLException e) {
      url = new URL("file:///" + fileUrl);
    }

    try {
      URLConnection urlCon = url.openConnection();
      readCatalog(catalog, urlCon.getInputStream());
    } catch (FileNotFoundException e) {
      Debug.message(1, "Failed to load catalog, file not found",
		    url.toString());
    }
  }

  /**
   * <p>Parse an XML Catalog stream.</p>
   *
   * @param catalog The catalog to which this catalog file belongs
   * @param is The input stream from which the catalog will be read
   *
   * @throws MalformedURLException Improper fileUrl
   * @throws IOException Error reading catalog file
   * @throws CatalogException A Catalog exception
   */
  public void readCatalog(Catalog catalog, InputStream is) 
    throws IOException, CatalogException {

    // Create an instance of the parser
    if (parserFactory == null && parserClass == null) {
      Debug.message(1, "Cannot read SAX catalog without a parser");
      throw new CatalogException(CatalogException.UNPARSEABLE);
    }

    this.catalog = catalog;

    try {
      if (parserFactory != null) {
	SAXParser parser = parserFactory.newSAXParser();
	SAXParserHandler spHandler = new SAXParserHandler();
	spHandler.setContentHandler(this);
	parser.parse(new InputSource(is), spHandler);
      } else {
	Parser parser = (Parser) Class.forName(parserClass).newInstance();
	parser.setDocumentHandler(this);
	parser.parse(new InputSource(is));
      }
    } catch (ClassNotFoundException cnfe) {
      throw new CatalogException(CatalogException.UNPARSEABLE);
    } catch (IllegalAccessException iae) {
      throw new CatalogException(CatalogException.UNPARSEABLE);
    } catch (InstantiationException ie) {
      throw new CatalogException(CatalogException.UNPARSEABLE);
    } catch (ParserConfigurationException pce) {
      throw new CatalogException(CatalogException.UNKNOWN_FORMAT);
    } catch (SAXException se) {
      Exception e = se.getException();
      // FIXME: there must be a better way
      UnknownHostException uhe = new UnknownHostException();
      FileNotFoundException fnfe = new FileNotFoundException();
      if (e != null) {
	if (e.getClass() == uhe.getClass()) {
	  throw new CatalogException(CatalogException.PARSE_FAILED,
				     e.toString());
	} else if (e.getClass() == fnfe.getClass()) {
	  throw new CatalogException(CatalogException.PARSE_FAILED,
				     e.toString());
	}
      }
      throw new CatalogException(se);
    }
  }

  // ----------------------------------------------------------------------
  // Implement the SAX ContentHandler interface

  /** <p>The SAX <code>setDocumentLocator</code> method. Does nothing.</p> */
  public void setDocumentLocator (Locator locator) {
    if (saxParser != null) {
      saxParser.setDocumentLocator(locator);
    }
  }

  /** <p>The SAX <code>startDocument</code> method. Does nothing.</p> */
  public void startDocument () throws SAXException {
    saxParser = null;
    abandonHope = false;
    return;
  }

  /** <p>The SAX <code>endDocument</code> method. Does nothing.</p> */
  public void endDocument ()throws SAXException {
    if (saxParser != null) {
      saxParser.endDocument();
    }
  }

  /**
   * <p>The SAX <code>startElement</code> method.</p> 
   *
   * <p>The catalog parser is selected based on the namespace of the
   * first element encountered in the catalog.</p>
   */
  public void startElement (String name,
			    AttributeList atts)
    throws SAXException {

    if (abandonHope) {
      return;
    }

    if (saxParser == null) {
      String prefix = "";
      if (name.indexOf(':') > 0) {
	prefix = name.substring(0, name.indexOf(':'));
      }

      String localName = name;
      if (localName.indexOf(':') > 0) {
	localName = localName.substring(localName.indexOf(':')+1);
      }

      String namespaceURI = null;
      if (prefix.equals("")) {
	namespaceURI = atts.getValue("xmlns");
      } else {
	namespaceURI = atts.getValue("xmlns:" + prefix);
      }

      String saxParserClass = getCatalogParser(namespaceURI,
					       localName);

      if (saxParserClass == null) {
	abandonHope = true;
	if (namespaceURI == null) {
	  Debug.message(2, "No Catalog parser for " + name);
	} else {
	  Debug.message(2, "No Catalog parser for "
			+ "{" + namespaceURI + "}"
			+ name);
	}
	return;
      }

      try {
	saxParser = (SAXCatalogParser)
	  Class.forName(saxParserClass).newInstance();

	saxParser.setCatalog(catalog);
	saxParser.startDocument();
	saxParser.startElement(name, atts);
      } catch (ClassNotFoundException cnfe) {
	saxParser = null;
	abandonHope = true;
	Debug.message(2, cnfe.toString());
      } catch (InstantiationException ie) {
	saxParser = null;
	abandonHope = true;
	Debug.message(2, ie.toString());
      } catch (IllegalAccessException iae) {
	saxParser = null;
	abandonHope = true;
	Debug.message(2, iae.toString());
      } catch (ClassCastException cce ) {
	saxParser = null;
	abandonHope = true;
	Debug.message(2, cce.toString());
      }
    } else {
      saxParser.startElement(name, atts);
    }
  }

  /**
   * <p>The SAX2 <code>startElement</code> method.</p> 
   *
   * <p>The catalog parser is selected based on the namespace of the
   * first element encountered in the catalog.</p>
   */
  public void startElement (String namespaceURI,
			    String localName,
			    String qName,
			    Attributes atts)
    throws SAXException {

    if (abandonHope) {
      return;
    }

    if (saxParser == null) {
      String saxParserClass = getCatalogParser(namespaceURI,
					       localName);

      if (saxParserClass == null) {
	abandonHope = true;
	if (namespaceURI == null) {
	  Debug.message(2, "No Catalog parser for " + localName);
	} else {
	  Debug.message(2, "No Catalog parser for "
			+ "{" + namespaceURI + "}"
			+ localName);
	}
	return;
      }

      try {
	saxParser = (SAXCatalogParser)
	  Class.forName(saxParserClass).newInstance();

	saxParser.setCatalog(catalog);
	saxParser.startDocument();
	saxParser.startElement(namespaceURI, localName, qName, atts);
      } catch (ClassNotFoundException cnfe) {
	saxParser = null;
	abandonHope = true;
	Debug.message(2, cnfe.toString());
      } catch (InstantiationException ie) {
	saxParser = null;
	abandonHope = true;
	Debug.message(2, ie.toString());
      } catch (IllegalAccessException iae) {
	saxParser = null;
	abandonHope = true;
	Debug.message(2, iae.toString());
      } catch (ClassCastException cce ) {
	saxParser = null;
	abandonHope = true;
	Debug.message(2, cce.toString());
      }
    } else {
      saxParser.startElement(namespaceURI, localName, qName, atts);
    }
  }

  /** <p>The SAX <code>endElement</code> method. Does nothing.</p> */
  public void endElement (String name) throws SAXException {
    if (saxParser != null) {
      saxParser.endElement(name);
    }
  }

  /** <p>The SAX2 <code>endElement</code> method. Does nothing.</p> */
  public void endElement (String namespaceURI,
			  String localName,
			  String qName) throws SAXException {
    if (saxParser != null) {
      saxParser.endElement(namespaceURI, localName, qName);
    }
  }

  /** <p>The SAX <code>characters</code> method. Does nothing.</p> */
  public void characters (char ch[], int start, int length)
    throws SAXException {
    if (saxParser != null) {
      saxParser.characters(ch, start, length);
    }
  }

  /** <p>The SAX <code>ignorableWhitespace</code> method. Does nothing.</p> */
  public void ignorableWhitespace (char ch[], int start, int length)
    throws SAXException {
    if (saxParser != null) {
      saxParser.ignorableWhitespace(ch, start, length);
    }
  }

  /** <p>The SAX <code>processingInstruction</code> method. Does nothing.</p> */
  public void processingInstruction (String target, String data)
    throws SAXException {
    if (saxParser != null) {
      saxParser.processingInstruction(target, data);
    }
  }

  /** <p>The SAX <code>startPrefixMapping</code> method. Does nothing.</p> */
  public void startPrefixMapping (String prefix, String uri)
    throws SAXException {
    if (saxParser != null) {
      saxParser.startPrefixMapping (prefix, uri);
    }
  }

  /** <p>The SAX <code>endPrefixMapping</code> method. Does nothing.</p> */
  public void endPrefixMapping (String prefix)
    throws SAXException {
    if (saxParser != null) {
      saxParser.endPrefixMapping (prefix);
    }
  }

  /** <p>The SAX <code>skippedentity</code> method. Does nothing.</p> */
  public void skippedEntity (String name)
    throws SAXException {
    if (saxParser != null) {
      saxParser.skippedEntity(name);
    }
  }
}
