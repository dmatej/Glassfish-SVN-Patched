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
package org.apache.myfaces.trinidadinternal.style.xml;

import java.io.File;
import java.io.IOException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import org.apache.myfaces.trinidad.share.io.NameResolver;
import org.apache.myfaces.trinidadinternal.share.io.DefaultNameResolver;

import org.apache.myfaces.trinidadinternal.share.xml.ClassParserFactory;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContextImpl;
import org.apache.myfaces.trinidadinternal.share.xml.ParserFactory;
import org.apache.myfaces.trinidadinternal.share.xml.ParserManager;
import org.apache.myfaces.trinidadinternal.share.xml.XMLProvider;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;

import org.apache.myfaces.trinidadinternal.style.StyleConstants;
import org.apache.myfaces.trinidadinternal.style.xml.parse.ColorNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IncludePropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Utility class for creating  a StyleSheetDocument.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/StyleSheetDocumentUtils.java#0 $) $Date: 10-nov-2005.18:58:00 $
 */
public class StyleSheetDocumentUtils
{
  /**
   * Merges two StyleSheetDocuments to produce a new StyleSheetDocuments
   * which combines styles from both documents.  If styles appear in
   * both documents, the style properties in the second StyleSheetDocument
   * take precedence.
   *
   * @param document1 The base StyleSheetDocument
   * @param document2 A second StyleSheetDocument which overrides
   *                  properties from the base StyleSheetDocument.
   *
   */
  public static StyleSheetDocument mergeStyleSheetDocuments(
    StyleSheetDocument document1,
    StyleSheetDocument document2
    )
  {
    if (document1 == null)
      return document2;
    if (document2 == null)
      return document1;

    // If neither document is null, merge the two documents together.
    // Start by getting the new document version
    String documentVersion = _mergeDocumentVersions(document1, document2);

    // The new document timestamp is just the max of the two old timestamps
    long documentTimestamp = Math.max(document1.getDocumentTimestamp(),
                                      document2.getDocumentTimestamp());

    // Now get the merged set of StyleSheetNodes
    List<StyleSheetNode> styleSheetsList = 
      new ArrayList<StyleSheetNode>();

    _addStyleSheets(styleSheetsList, document1);
    _addStyleSheets(styleSheetsList, document2);

    int styleSheetsCount = styleSheetsList.size();
    StyleSheetNode[] styleSheetsArray = new StyleSheetNode[styleSheetsCount];
    styleSheetsList.toArray(styleSheetsArray);

    return new StyleSheetDocument(styleSheetsArray,
                                  documentVersion,
                                  documentTimestamp);
  }

  // Adds the StyleSheetNodes from the specified StyleSheetDocument
  // into the List of StyleSheetNodes.
  private static void _addStyleSheets(
    List<StyleSheetNode> styleSheets,
    StyleSheetDocument   document
    )
  {
    Iterator<StyleSheetNode> e = document.getStyleSheets();
    while (e.hasNext())
      styleSheets.add(e.next());
  }

  // Merges the StyleSheetDocument versions of two documents
  private static String _mergeDocumentVersions(
    StyleSheetDocument document1,
    StyleSheetDocument document2
    )
  {
    // Both documents should be non-null when calling this
    assert ((document1 != null) && (document2 != null));

    String version1 = document1.getDocumentVersion();
    String version2 = document2.getDocumentVersion();

   if (version1 == null)
     return version2;
   if (version2 == null)
     return version1;

    return version1 + version2;
  }

  private static ParserManager _getDefaultParserManager()
  {
    if (_sDefaultParserManager == null)
    {
      ParserManager manager = new ParserManager();
      _registerFactory(
        manager, StyleSheetDocument.class, "StyleSheetDocument");
      _registerFactory(manager, StyleSheetNode.class, "StyleSheetNode");
      _registerFactory(manager, StyleNode.class, "StyleNode");
      _registerFactory(manager, PropertyNode.class, "PropertyNode");
      _registerFactory(manager, ColorNode.class, "ColorNode");
      _registerFactory(manager,
                       IncludePropertyNode.class,
                       "IncludePropertyNode");

      _registerFactory(manager, String.class, "ValueNode");

      _sDefaultParserManager = manager;
    }

    return _sDefaultParserManager;
  }

  private static void _registerFactory(
    ParserManager manager,
    Class<?> expectedType,
    String baseName
    )
  {
    String className = "org.apache.myfaces.trinidadinternal.style.xml.parse." + baseName + "Parser";
    ParserFactory factory = new ClassParserFactory(className);

    manager.registerFactory(expectedType,
                            StyleConstants.OCELOT_NAMESPACE,
                            factory);
  }

  private static ParserManager _sDefaultParserManager;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    StyleSheetDocumentUtils.class);
}
