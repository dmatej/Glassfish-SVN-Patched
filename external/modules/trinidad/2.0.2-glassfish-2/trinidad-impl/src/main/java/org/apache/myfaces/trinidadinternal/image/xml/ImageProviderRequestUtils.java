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
package org.apache.myfaces.trinidadinternal.image.xml;

import java.io.IOException;
import java.io.PrintWriter;

import java.util.Map;
import java.util.ResourceBundle;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;


import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.share.xml.ParseContextImpl;
import org.apache.myfaces.trinidadinternal.share.xml.ParserFactory;
import org.apache.myfaces.trinidadinternal.share.xml.ParserManager;
import org.apache.myfaces.trinidadinternal.share.xml.TreeBuilder;
import org.apache.myfaces.trinidadinternal.share.xml.XMLProvider;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.image.ImageType;
import org.apache.myfaces.trinidadinternal.image.util.MapArea;
import org.apache.myfaces.trinidadinternal.image.xml.encode.XMLEncoder;
import org.apache.myfaces.trinidadinternal.image.xml.parse.ImageProviderRequestParserFactory;
import org.apache.myfaces.trinidadinternal.image.xml.parse.ColorParserFactory;
import org.apache.myfaces.trinidadinternal.image.xml.parse.FontParserFactory;
import org.apache.myfaces.trinidadinternal.image.xml.parse.ImageMapParserFactory;
import org.apache.myfaces.trinidadinternal.image.xml.parse.TextParserFactory;

/**
 * Utility methods for parsing and encoding ImageProviderRequest XML elements.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/ImageProviderRequestUtils.java#0 $) $Date: 10-nov-2005.19:03:59 $
 */
public class ImageProviderRequestUtils
{
  /**
   * Creates an ImageProviderRequest from an InputSource containing a single
   * image entry.
   */
  public static ImageProviderRequest createImageProviderRequest(
    ImageContext context,
    XMLProvider  provider,
    InputSource  source
    ) throws IOException, SAXException
  {
    return (ImageProviderRequest)_parse(context,
                                        provider,
                                        source,
                                        ImageProviderRequest.class);
  }

  /**
   * Creates an array of ImageProviderRequests from an InputSource
   * containing multiple image entries.
   */
  public static ImageProviderRequest[] createImageProviderRequests(
    ImageContext context,
    XMLProvider  provider,
    InputSource  source
    ) throws IOException, SAXException
  {
    return (ImageProviderRequest[])_parse(context,
                                          provider,
                                          source,
                                          ImageProviderRequest[].class);
  }

  /**
   * Encodes an request based on the requested image type and
   * properties.  If an XMLEncoder can not be found for the
   * requested ImageType, an IllegalArgumentException is thrown.
   *
   * @param context The image context
   * @param namespaceURI The namespace URI of the root element.  If null,
   *   no namespaceURI is written.
   * @param localName The local name of the root element
   * @param type The ImageType of the requested image
   * @param properties The requested image properties
   * @param responseProperties The response properties for the image generated
   *          for this request.  If null, no repsonse properties are encoded.
   * @param The PrintWriter to encode the request to
   */
  public static void encodeImageProviderRequest(
    ImageContext context,
    String       namespaceURI,
    String       localName,
    ImageType    type,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties,
    PrintWriter  writer
    ) throws IllegalArgumentException
  {
    XMLEncoder encoder = (XMLEncoder)type.getProperty(
                                       ImageType.XML_ENCODER_PROPERTY);
    if (encoder == null)
      throw new IllegalArgumentException(_ENCODER_ERROR + type);

    // Don't know if this is necessary, but just in case
    if (responseProperties == null)
      responseProperties = _EMPTY_MAP;


    // Print the root element
    writer.print("<ImageMetadata version=\"2.0\"");
    writer.print(" xmlns=\"");
    writer.print(ImageConstants.TECATE_NAMESPACE);
    writer.println("\">");

    // Write the metadata
    encoder.encodeXML(context,
                      namespaceURI,
                      localName,
                      properties,
                      responseProperties,
                      writer);

    // Close the root element
    writer.println("</ImageMetadata>");

    writer.flush();
  }

  /**
   * Creates an array of MapAreas from an InputSource containing a single
   * imageMap element.
   */
  public static MapArea[] createMapAreas(
    ImageContext context,
    XMLProvider  provider,
    InputSource  source
    ) throws IOException, SAXException
  {
    return (MapArea[])_parse(context,
                             provider,
                             source,
                             MapArea[].class);
  }

  /**
   * Encodes the MapAreas into the XML representation.
   * @param context The ImageContext
   * @param namespaceURI The namespace URI of the root element.  If null,
   *   no namespaceURI is written.
   * @param localName The local name of the root element
   * @param areas The MapAreas to encode
   * @param writer The PrintWriter to which the MapAreas are written
   */
  public static void encodeImageMapAreas(
    ImageContext context,
    String       namespaceURI,
    String       localName,
    MapArea      areas[],
    PrintWriter  writer
    )
  {
    if (areas == null)
      return;

    if (localName == null) 
    {
      throw new NullPointerException("Null localName");
    }

    // Write the start element, including namespace if it is specified.
    writer.print('<');
    writer.print(localName);

    if (namespaceURI != null)
      _encodeAttribute("xmlns", namespaceURI, writer);

    writer.println('>');

    // Write each area
    for (int i = 0; i < areas.length; i++)
    {
      writer.print('<');
      writer.print(XMLConstants.IMAGE_MAP_AREA_NAME);

      MapArea area = areas[i];
      _encodeAttribute(XMLConstants.SHAPE_ATTR, area.getShape(), writer);
      _encodeAttribute(XMLConstants.COORDINATES_ATTR,
                       area.getCoordinatesString(),
                       writer);
      writer.println("/>");
    }

    // Write the end element
    writer.print("</");
    writer.print(localName);
    writer.println('>');
  }

  /**
   * Creates a ParserManager with the default ParserFactory for the
   * TECATE_NAMESPACE.
   */
  public static ParserManager createDefaultManager()
  {
    ParserManager manager = new ParserManager();
    ImageProviderRequestParserFactory.sharedInstance().registerSelf(manager,
                                              ImageConstants.TECATE_NAMESPACE);
    ColorParserFactory.sharedInstance().registerSelf(manager,
                                              ImageConstants.TECATE_NAMESPACE);
    FontParserFactory.sharedInstance().registerSelf(manager,
                                              ImageConstants.TECATE_NAMESPACE);
    TextParserFactory.sharedInstance().registerSelf(manager,
                                              ImageConstants.TECATE_NAMESPACE);
    ImageMapParserFactory.sharedInstance().registerSelf(manager,
                                              ImageConstants.TECATE_NAMESPACE);

    return manager;
  }

  /**
   * Registers a ParserFactory which is used to parse ImageProviderRequest
   * objects for elements in the specified namespace.
   */
  public static void registerParserFactory(
    Class<?> expectedType,
    String namespace,
    ParserFactory factory
    )
  {
    _getDefaultManager().registerFactory(expectedType, namespace, factory);
  }

  /**
   * Unregisters the ParserFactory for the specified namespace.
   */
  public static void unregisterParserFactory(
    Class<?> expectedType,
    String namespace
    )
  {
    _getDefaultManager().unregisterFactory(expectedType, namespace);
  }

  // Encodes a single attribute
  private static void _encodeAttribute(
    String      name,
    String      value,
    PrintWriter writer
    )
  {
    writer.print(' ');
    writer.print(name);
    writer.print("=\"");
    writer.print(value);
    writer.print('"');
  }

  private static ParserManager _getDefaultManager()
  {
    if (_sManager == null)
      _sManager = createDefaultManager();

    return _sManager;
  }

  // Parse the input source and return an object of the expected class
  private static Object _parse(
    ImageContext context,
    XMLProvider  provider,
    InputSource  source,
    Class<?>     expectedClass
    ) throws IOException, SAXException
  {
    // Get the ParserManager
    ParserManager manager = _getDefaultManager();

    // Create the builder.
    TreeBuilder builder = new TreeBuilder(manager, expectedClass);

    // Set up the parse context
    ParseContextImpl parseContext = new ParseContextImpl();

    // Transfer properties from the ImageContext to the ParseContext
    ResourceBundle bundle = (ResourceBundle)context.getProperty(
                              ImageConstants.TECATE_NAMESPACE,
                              XMLConstants.RESOURCE_BUNDLE_PROPERTY);
    if (bundle != null)
    {
      parseContext.setProperty(ImageConstants.TECATE_NAMESPACE,
                               XMLConstants.RESOURCE_BUNDLE_PROPERTY,
                               bundle);

    }

    parseContext.setProperty(ImageConstants.TECATE_NAMESPACE,
                             XMLConstants.LOCALE_CONTEXT_PROPERTY,
                             context.getLocaleContext());

    // Parse it!
    return builder.parse(provider, source, parseContext);
  }

  private ImageProviderRequestUtils() {}

  static private ParserManager _sManager;

  // Error messages
  private static final String _ENCODER_ERROR = "Missing XMLEncoder for ";

  // We use this empty dictionary as a stub for the response properties
  // dictionary when we encode the image properties with the XMLEncoder
  private static final Map<Object, Object> _EMPTY_MAP = new ArrayMap<Object, Object>(0);
}
