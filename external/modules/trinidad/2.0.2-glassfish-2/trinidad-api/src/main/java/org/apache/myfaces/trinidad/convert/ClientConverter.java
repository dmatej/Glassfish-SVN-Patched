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
package org.apache.myfaces.trinidad.convert;

import java.util.Collection;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * <p>
 * Interface implemented by Objects that wish to perform client-side
 * conversion in addition to server-side conversion.  Client side conversion
 * should always be the same as or more lenient than the server-side
 * conversion.
 * </p>
 *
 * <P>
 *     One of the benefits of Apache Trinidad is that it supports client-side versions of converters and validators. This means that errors can be caught on the client and a round trip avoided. This interface can be used to add client-side conversion to a converter.
 *     </P>
 *     <p>
 *     The basic idea of Apache Trinidad client-side conversion is that it works on the client in a very similar way to how it works on the server, except the language on the client is javascript instead of java. Apache Trinidad supports javascript Converter objects that support the methods getAsString() and getAsObject(). A Converter can throw a ConverterException.
 *     </p>      Let's say you've written a javax.faces.convert.Converter implementation and now you want to add client-side conversion. The first thing to do is write a version of the converter in javascript. Here is the javascript code for the converter "interface".
 *     </p>
 *       <p>
 *  <pre><code>
 * /**
 *  * Converter "interface" similar to javax.faces.convert.Converter,
 *  * except that all relevant information must be passed to the constructor
 *  * as the context and component are not passed to the getAsString or getAsObject method
 *  *
 *  * /
 * function Converter()
 * {
 * }
 *
 *  /**
 *  * Convert the specified model object value, into a String for display
 *  *
 *  * @param value Model object value to be converted
 *  * /
 * Converter.prototype.getAsString = function(value){}
 *
 * /**
 *  * Convert the specified string value into a model data object
 *  * which can be passed to validators
 *  *
 *  * @param value String value to be converted
 *  * /
 * Converter.prototype.getAsObject = function(value){}
 * </code></pre>
 * Converters can throw a ConverterException, here is the signature:
 * <ul>
 * <li>ConverterException(detail)
 *  <ul>
 *    <li>detail - Localized detail message text </li>
 *   </ul>
 * </li>
 * </ul>
 * The method
 * <ul>
 *   <li><code>getClientLibrarySource()</code> is expected to return a library
 * that has an implementation of the javascript Converter object.</li>
 *   <li><code>getClientConversion()</code> is expected to return a
 * javascript constructor which will be used to instantiate an instance of the converter.</li>
 *   <li><code>getClientScript()</code> can be used to write out inline js.</li>
 *   <li><code>getClientImportNames()</code> is used to import the built-in scripts provided by Apache Trinidad.</li>
 *  </ul>
 * </p>
 * <p>
 * @see javax.faces.convert.Converter
 */
public interface ClientConverter
{

  /**
   * Gets the URI specifying the location of the js lib resource.
   * Only the first reference to a library will result in its being imported.
   */
  public String getClientLibrarySource(
   FacesContext context);

  /**
   * Supports importing the built-in scripts provided by Apache Trinidad.
   * It can be used to ensure that a Javascript function is available
   * before using it in a Javascript handler.
   * Only the first reference to a script will result in its being imported.
   * <p>If this function returns null "Converter()" will be used.
   * @return a collection of function names
   */
  public Collection<String> getClientImportNames();


  /**
   * Opportunity for the ClientConverter to return script content.
   * For HTML, this will be javascript that will be embedded in a
   * script tag. For HTML this method is expected to return an
   * implementation of the javascript Converter object.
   * <p>Do not rely on this content being ppr updated.
   * <p>This method will be called once per converter instance.
   * Content that should only be written once per request
   * should only be returned once.
   */
  public String getClientScript(
   FacesContext context,
   UIComponent component);

  /**
   * Called to retrieve the appropriate client
   * conversion code for the node and context.
   * For HTML, this will be javascript that will be embedded in a
   * script tag. For HTML this method is expected to return a
   * constructor of the javascript Converter object
   * returned by getClientScript().
   */
  public String getClientConversion(
   FacesContext context,
   UIComponent component);

  static public final String ALERT_FORMAT_KEY =
    "org.apache.myfaces.trinidad.convert.ALERT_FORMAT";
}
