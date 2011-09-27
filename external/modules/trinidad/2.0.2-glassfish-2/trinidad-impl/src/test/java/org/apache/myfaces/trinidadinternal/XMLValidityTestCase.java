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
package org.apache.myfaces.trinidadinternal;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;


import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import junit.framework.TestCase;

public abstract class XMLValidityTestCase extends TestCase
{
  public XMLValidityTestCase(
    String testName)
  {
    super(testName);
  }

  protected void executeValidityTest(
     URL    schemaSource,
     String publicId,
     URL    xmlSource) throws Throwable
  {
    // Fail fast on null URLs, since they mean we'll be failing
    // soon enough, but far more cryptically.
    if ((publicId != null) && (schemaSource == null))
      throw new NullPointerException("No source for schema/DTD");

    if (xmlSource == null)
      throw new NullPointerException("No source for XML document");

    ER er = new ER();
    er.registerPublicId(publicId, schemaSource);

    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(false);
    factory.setValidating(true);

    DocumentBuilder docBuilder = factory.newDocumentBuilder();
    // Make sure this test is relevant...
    assertTrue(docBuilder.isValidating());

    docBuilder.setEntityResolver(er);
    docBuilder.setErrorHandler(new Errors());
    docBuilder.parse(new InputSource(xmlSource.openStream()));
  }
  
  static public class ER implements EntityResolver
  {
    public ER()
    {
      _publicIDs = new HashMap<String, URL>();
      _systemIDs = new HashMap<String, URL>();
    }

    public void registerPublicId(String publicId, URL url)
    {
      _publicIDs.put(publicId, url);
    }

    public void registerSystemId(String systemId, URL url)
    {
      _systemIDs.put(systemId, url);
    }

    public InputSource resolveEntity(String publicId, String systemId)
      throws SAXException, IOException
    {
      URL url = (publicId == null) ? null : (URL) _publicIDs.get(publicId);
      if (url == null)
        url = _systemIDs.get(systemId);

      if (url == null)
        return null;

      return new InputSource(url.openStream());
    }

    private Map<String, URL> _publicIDs;
    private Map<String, URL> _systemIDs;
  }

  static public class Errors implements ErrorHandler
  {
    public void error(SAXParseException exception) 
    {
      fatalError(exception);
    }
     
    public void fatalError(SAXParseException exception) 
    {
      String message = "Parser error: " + exception.getMessage();
      message += ", line " + exception.getLineNumber();
      message += ", column " + exception.getColumnNumber();
      fail(message);
    }

    public void warning(SAXParseException exception) 
    {
      fatalError(exception);
    }
  }
}
