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
package org.apache.myfaces.trinidadinternal.style.util;

import java.awt.Color;

import java.net.URI;

import java.net.URISyntaxException;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ArrayMap;
import org.apache.myfaces.trinidad.style.Style;
import org.apache.myfaces.trinidadinternal.style.CSSStyle;
import org.apache.myfaces.trinidadinternal.style.CoreStyle;
import org.apache.myfaces.trinidadinternal.style.PropertyParseException;
import org.apache.myfaces.trinidadinternal.util.LRUCache;

/**
 * CSS-related utilities. I think as we move away from xss, most of this code will
 * be removed.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/CSSUtils.java#0 $) $Date: 10-nov-2005.18:58:49 $
 */
public class CSSUtils
{

  /**
   * Resolve the propertyValue. For example, if the propertyValue is an url, then we run
   * it through the encodeResourceURL method.
   * @param styleSheetName - The Skin's stylesheet name.
   * @param baseURI - An absolute base URI pointing to the directory which contains the 
   * skin style sheet. You can compute this value by calling getBaseSkinStyleSheetURI and
   * passing in the styleSheetName.
   * @param propertyName - The css property name, like background-image
   * @param propertyValue -The css property value, like url("skins/purple/error.gif");
   * @return the propertyValue, possibly changed to a 'resolved' propertyValue. For instance,
   * if it is an url, the return value will be the encoded url.
   */
  public static String resolvePropertyValue(
    String styleSheetName,
    String baseURI,
    String propertyName,
    String propertyValue)
  {
    // process propertyValue to encode if it is an url
    if (_containsURL(propertyValue))
    {
      String resolvedUrl = _resolveURL(styleSheetName,
                                      baseURI,
                                      propertyValue);
      return resolvedUrl;
    }
    else 
    {
      if (_URI_PROPERTIES.contains(propertyName))
      { 
        // Make sure it's a legit value for an URL
        if (!_SPECIAL_URI_VALUES.contains(propertyValue))
        {
          // TODO: Add a list of property names expecting an URL here, 
          // "content" maybe?
          _LOG.warning("URL_VALUE_EXPECTED_FOR_PROPERTY_IN_STYLE_SHEET", new Object[]{propertyName, styleSheetName, propertyValue});
        }
      }
      return propertyValue;
    }
  }


  /**
   * Given a Skin's stylesheet name that is being parsed,
   * return an absolute base URI pointing to the
   * directory which contains the skin style sheet.  We will use this
   * base URI to ensure that URLs specified in the style sheet
   * (eg. background-image URLs) are resolved appropriately
   * (ie. not relative to the generated style sheet).
   * 
   * @param styleSheetName - The Skin's stylesheet name. E.g., "/skins/purple/purpleSkin.css"
   * @return an absolute base URI pointing to the
  // directory which contains the skin style sheet. e.g., "/trinidad-context/skins/purple"
   */
  public static String getBaseSkinStyleSheetURI(String styleSheetName)
  {
    // The styleSheetName is actually a context-relative URI.
    // We need to strip off the file name and prepend the
    // context path.
    
    // First, get the context path.
    // Note that our options for obtaining the context path at
    // this point are somewhat limited.  We could require that
    // the caller pass this in, though this would require
    // passing this information through many layers for non-obvious
    // reasons.  Instead, we rely on the fact that we can obtain
    // this information through the FacesContext (actually,
    // through the ExternalContext), which we have access to here.
    // This is slightly ugly, since this introduces a dependency
    // from our CSS parsing code on JavaServer Faces, but until
    // we find use cases where our skinning architecture will be
    // applied in a non-Faces environment, this dependency seems
    // accpetable.
    FacesContext facesContext = FacesContext.getCurrentInstance();
    assert(facesContext != null);

    ExternalContext externalContext = facesContext.getExternalContext();
    String contextPath = externalContext.getRequestContextPath();
    assert(contextPath != null);

    int contextPathLength = contextPath.length();

    // Before we combine the context path and styleSheetName name to
    // produce the base URI, make sure that these values are
    // in the expected form
    assert(contextPathLength > 0);
    assert(contextPath.charAt(0) == '/');
    assert(contextPath.charAt(contextPathLength - 1) != '/');
    assert(styleSheetName.length() > 0);
    assert(styleSheetName.charAt(0) != '/');

    // Our internal css files are under /META-INF/adf/styles, though
    // images are accessed via <contextPath>/adf/images.  As such,
    // we also need to strip off the bonus "/META_INF" prefix from
    // the source name.  Otherwise, image requests won't be
    // resolved since /META-INF is not exposed via HTTP.
    if (styleSheetName.startsWith("META-INF/"))
      styleSheetName = styleSheetName.substring(9);

    // Find the start of the file name part of the source name - we don't
    // need this as part of the base URI
    int lastSepIndex = styleSheetName.lastIndexOf('/');
    
    if (lastSepIndex == -1)
      return contextPath;
    else
    {
      StringBuilder buffer = new StringBuilder(
                                    contextPathLength + lastSepIndex + 1);
      buffer.append(contextPath);
      buffer.append("/");
      buffer.append(styleSheetName.substring(0, lastSepIndex));
      return buffer.toString();
    }
  }
  
  public static boolean isAbsoluteURI(String uriString)
  {
    if (uriString == null)
      return false;
    if (uriString.indexOf(':') == -1)
      return false;
    
    URI uri;
    try
    {
      uri = new URI(uriString);
      return uri.isAbsolute();
    }
    catch (URISyntaxException e)
    {
      _LOG.warning("The URI syntax is incorrect, and can not be verified.");
    }
    return false;
  }
  
  /**
   * Convert a relative URI values to an absolute URI value.
   * For example, if the baseURI is "/trinidad-context/skins/purple" and 
   * the uri is "../../skins/purple/xyz.gif", we return 
   * @param styleSheetName - the name of the Skin's stylesheet. We use this in any warning 
   * messages.
   * @param baseURI - absolute base URI pointing to the directory 
   * which contains the skin style sheet. This is used to figure out the absolute uri of the uri
   * parameter.
   * @param uri - a uri. If this is an uri that begins with "../", then
   * we convert it to be an absolute url that has no "../" at the start.

   * @return An uri that does not begin with one or more "../" strings.
   */
  public static String getAbsoluteURIValue(
    String styleSheetName,
    String baseURI,
    String uri)
  {
    String strippedURI = uri;
    String strippedBaseURI = baseURI;

    // Strip off leading "../" segments from the uri
    while (strippedURI.startsWith("../"))
    {
      int lastSepIndex = strippedBaseURI.lastIndexOf('/');
      if (lastSepIndex < 0)
      {
        _LOG.warning("INVALID_IMAGE_URI_IN_STYLE_SHEET", new Object[]{uri, styleSheetName});

        break;
      }

      strippedURI = strippedURI.substring(3);
      strippedBaseURI = strippedBaseURI.substring(0, lastSepIndex);
    }

    StringBuilder builder = new StringBuilder(strippedBaseURI.length() +
                                             strippedURI.length() +
                                             2);
    builder.append(strippedBaseURI);
    builder.append("/");
    builder.append(strippedURI);

    return builder.toString();
  }

  /**
   * Parse an inline style into a CSSStyle object.
   */
  public static CSSStyle parseStyle(String text)
  {
    if ((text == null) || "".equals(text))
      return null;

    // Common mistake: wrapping style attributes in braces.  Drop
    // the braces on their behalf.
    if (text.startsWith("{"))
      text = text.substring(1);
    if (text.endsWith("}"))
      text = text.substring(0, text.length() - 1);

    CSSStyle style = new CSSStyle();
    StringTokenizer tokens = new StringTokenizer(text, ";");
    while (tokens.hasMoreTokens())
    {
      String token = tokens.nextToken();
      int colonPos = token.indexOf(':');
      if (colonPos >= 0)
      {
        String key = token.substring(0, colonPos).trim();
        String value = token.substring(colonPos + 1).trim();
        style.setProperty(key, value);
      }
    }

    return style;
  }


  /**
   * Parses the CSS color value.
   *
   * @param value A CSS color value.
   * @return A Color object which corresponds to the provided value
   * @throws PropertyParseException Thrown if the specified color
   *   value can not be parsed.
   */
  public static Color parseColor(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if ((value == null) || (value.length() == 0))
      return null;

    // Lower-case the value
    value = value.toLowerCase();

    // First check to see if it is a named value
    Color color = (Color)ArrayMap.get(_NAMED_COLORS, value);
    if (color != null)
      return color;

    // Parse #RRGGBB and #RGB values
    if (value.charAt(0) == '#')
    {
      int length = value.length();

      if (length == 7)
      {
        // #RRGGBB
        int rgb = 0;

        try
        {
          rgb = Integer.parseInt(value.substring(1), 16);
        }
        catch (NumberFormatException e)
        {
          throw new PropertyParseException(_INVALID_COLOR + value);
        }

        return _getSharedColor(rgb);
      }
      else if (length == 4)
      {
        // #RGB
        int r = 0;
        int g = 0;
        int b = 0;

        try
        {
          r = Integer.parseInt(value.substring(1, 2), 16);
          g = Integer.parseInt(value.substring(2, 3), 16);
          b = Integer.parseInt(value.substring(3, 4), 16);
        }
        catch (NumberFormatException e)
        {
          throw new PropertyParseException(_INVALID_COLOR + value);
        }

        int rgb = (((r << 20) & 0xf00000) |
                   ((r << 16) & 0x0f0000) |
                   ((g << 12) & 0x00f000) |
                   ((g << 8)  & 0x000f00) |
                   ((b << 4)  & 0x0000f0) |
                    (b & 0x00000f));

        return _getSharedColor(rgb);
      }
      else
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }
    }

    // Handle rgb(r, g, b) values
    if (value.startsWith("rgb"))
    {
      int startIndex = value.indexOf('\u0028');  // Start paren
      int endIndex = value.indexOf('\u0029');    // End paren
      if ((startIndex == -1) || (endIndex == -1) || (endIndex <= startIndex))
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }

      // Tokenize on whitespace or commas
      StringTokenizer tokens = new StringTokenizer(
                                 value.substring(startIndex + 1, endIndex),
                                 " \t,");

      String redToken = null;
      String blueToken = null;
      String greenToken = null;

      try
      {
        redToken = tokens.nextToken();
        greenToken = tokens.nextToken();
        blueToken = tokens.nextToken();
      }
      catch (NoSuchElementException e)
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }

      int red = _parseColorComponent(value, redToken);
      int green = _parseColorComponent(value, greenToken);
      int blue = _parseColorComponent(value, blueToken);

      return _getSharedColor(red, green, blue);
    }

    // Check for system color values.  We can't actually return valid
    // values for these, but we also don't want to throw a
    // PropertyParseException if a system color is specified.
    if (ArrayMap.get(_SYSTEM_COLORS, value) != null)
      return null;

    throw new PropertyParseException(_INVALID_COLOR + value);
  }

  /**
   * Parses a CSS font family value into a list of font family names.
   *
   * @param value A CSS font family value.
   * @return The list of font family names present in the font family property
   * @throws PropertyParseException Thrown if the specified font family
   *   value can not be parsed.
   */
  public static String[] parseFontFamilies(String value)
    throws PropertyParseException
  {
    if ((value == null) || (value.length() == 0))
      return null;

    // TODO: -= Simon Lessard
    Vector<String> v = new Vector<String>();
    StringTokenizer tokens = new StringTokenizer(value, ",\"");
    while (tokens.hasMoreTokens())
    {
      String family = _stripWhitespace(tokens.nextToken());

      if ((family != null) && (family.length() > 0))
        v.addElement(family);
    }

    // Copy the results into an array
    String[] families = new String[v.size()];
    v.copyInto(families);

    return families;
  }

  /**
   * Parses a CSS font size.
   *
   * @param value A CSS font size value.  At the moment, all font
   *   sizes must be specified in points (eg. "12pt").
   * @return An Integer font size suitable for use as an AWT Font size
   * @throws PropertyParseException Thrown if the specified font size
   *   value can not be parsed.
   */
  public static Integer parseFontSize(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if ((value == null) || (value.length() == 0))
      return null;

    value = value.toLowerCase();

    // First, check to see if the size is one of the named values
    Integer fontSize = (Integer)ArrayMap.get(_NAMED_FONTS_SIZES, value);
    if (fontSize != null)
      return fontSize;

    if (_isLength(value))
      return parseLength(value);

    if (_isPercentage(value))
      return _parsePercentage(value);

    throw new PropertyParseException(_INVALID_FONT_SIZE + value);
  }

  /**
   * Parses a CSS font style
   *
   * @param value A CSS font style value.
   * @return An integer font style suitable for use as an AWT Font style
   * @throws PropertyParseException Thrown if the specified font style
   *   value can not be parsed.
   */
  public static Object parseFontStyle(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if ((value == null) || (value.length() == 0))
      return null;

    value = value.toLowerCase();

    if (_NORMAL_STYLE.equals(value))
      return CoreStyle.PLAIN_FONT_STYLE;

    if (_ITALIC_STYLE.equals(value) || _OBLIQUE_STYLE.equals(value))
      return CoreStyle.ITALIC_FONT_STYLE;

    throw new PropertyParseException(_INVALID_FONT_STYLE + value);
  }

  /**
   * Parses a CS font weight
   *
   * @param value A CSS font weight value.  At the moment, all font
   *   weights must be specified as either "bold" or "normal".
   * @return An integer font weight suitable for use as the weight
   *   component of an AWT Font style
   * @throws PropertyParseException Thrown if the specified font weight
   *   value can not be parsed.
   */
  public static Object parseFontWeight(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if ((value == null) || (value.length() == 0))
      return null;

    value = value.toLowerCase();

    if (_NORMAL_WEIGHT.equals(value) || _LIGHTER_WEIGHT.equals(value))
      return CoreStyle.PLAIN_FONT_WEIGHT;

    if (_BOLD_WEIGHT.equals(value) || _BOLDER_WEIGHT.equals(value))
      return CoreStyle.BOLD_FONT_WEIGHT;

    // Check for sizes 100 - 900
    try
    {
      int weight = Integer.parseInt(value);
      if ((weight >= 100) && (weight <= 900) && ((weight % 100) == 0))
      {
        if (weight >= 600)
          return CoreStyle.BOLD_FONT_WEIGHT;

        return CoreStyle.PLAIN_FONT_WEIGHT;
      }
    }
    catch (NumberFormatException e)
    {
      ;
    }

    throw new PropertyParseException(_INVALID_FONT_WEIGHT + value);
  }

  /**
   * Parses a CSS length value.
   *
   * @param value A CSS length value.
   * @return An Integer font size representing the length
   * @throws PropertyParseException Thrown if the specified
   *   value can not be parsed.
   */
  public static Integer parseLength(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if (_isLength(value))
      return _parseLength(value);

    throw new PropertyParseException(_INVALID_LENGTH + value);
  }

  /**
   * Converts the specified Color to a valid CSS color value.
   */
  public static String getColorValue(Color color)
  {
    StringBuffer buffer = new StringBuffer(7);
    buffer.append('#');
    buffer.append(_getHexColorComponent(color.getRed()));
    buffer.append(_getHexColorComponent(color.getGreen()));
    buffer.append(_getHexColorComponent(color.getBlue()));

    return buffer.toString();
  }

  // Dinky utility to return a two digit hexidecimal color component
  private static String _getHexColorComponent(int colorComponent)
  {
    String hex = Integer.toString(colorComponent, 16);

    // Make sure hex value is two digits for "#RRGGBB" format
    if (hex.length() == 1)
      hex = "0" + hex;

    return hex;
  }

  // Parses a color component value.  Either 0-255, or 0-100%
  private static int _parseColorComponent(String value, String comp)
    throws PropertyParseException
  {
    if ((comp == null) || (comp.length() == 0))
      return 0;

    int col = 0;

    if (comp.endsWith("%"))
    {
      double percent = 0;

      // Handle percentage units
      try
      {
        percent = Double.parseDouble(comp.substring(0, comp.length() - 1));
      }
      catch (NumberFormatException e)
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }

      col = (int)((percent/100.0) * 255);
    }
    else
    {
      try
      {
        col = Integer.parseInt(comp);
      }
      catch (NumberFormatException e)
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }
    }

    if (col < 0)
      return 0;

    return (col > 255) ? 255 : col;
  }

  // Tests whether the value is specified in length units
  private static boolean _isLength(String value)
  {
    // Assume we have checked for null already
    assert (value != null);

    return (value.endsWith("in") ||
            value.endsWith("cm") ||
            value.endsWith("mm") ||
            value.endsWith("pt") ||
            value.endsWith("pc") ||
            value.endsWith("em") ||
            value.endsWith("ex") ||
            value.endsWith("px"));
  }

  // Tests whether the value is specified in percentage units
  private static boolean _isPercentage(String value)
  {
    // Assume we have checked for null already
    assert (value != null);

    return value.endsWith("%");
  }

  // Parses length units on an already stripped value
  private static Integer _parseLength(String value)
    throws PropertyParseException
  {
    // Assume we have already checked for length units
    assert (_isLength(value));

    // Parse out the size
    double size = 0;

    try
    {
      size = Double.parseDouble(value.substring(0, value.length() - 2));
    }
    catch (NumberFormatException e)
    {
      throw new PropertyParseException(_INVALID_LENGTH + value);
    }

    // Convert the size to "points".  This conversion isn't really valid,
    // since we don't check the screen resolution.  We just convert using
    // the assuming that 1pt = 1px, which of course isn't really the
    // case, but what are we going to do?
    int points = 0;

    if (value.endsWith("in"))
    {
      points = (int)(72 * size);
    }
    else if (value.endsWith("cm"))
    {
      points = (int)((72 * size)/2.54);
    }
    else if (value.endsWith("mm"))
    {
      points = (int)((72 * size)/25.4);
    }
    else if (value.endsWith("pt"))
    {
      points = (int)size;
    }
    else if (value.endsWith("pc"))
    {
      points = (int)(12 * size);
    }
    else if (value.endsWith("em"))
    {
      // We don't even try to figure out the right em size.  If you don't
      // like it, don't use em units for your image styles.
      points = (int)(12 * size);
    }
    else if (value.endsWith("ex"))
    {
      // We don't even try to figure out the right ex size.  If you don't
      // like it, don't use ex units for your image styles.
      points = (int)(6 * size);

    }
    else if (value.endsWith("px"))
    {
      points = (int)size;
    }
    else
    {
      throw new PropertyParseException(_INVALID_LENGTH + value);
    }

    return points;
  }

  // Parses length units
  private static Integer _parsePercentage(String value)
    throws PropertyParseException
  {
    // Assume we have already checked for percentage units
    assert (_isPercentage(value));

    double percent = 0;

    try
    {
      percent = Double.parseDouble(value.substring(0, value.length() - 1));
    }
    catch (NumberFormatException e)
    {
      throw new PropertyParseException(_INVALID_PERCENTAGE + value);
    }

    // We just assume the percentage is relative to our base font size - 12pt.
    return (int)((percent/100.0) * 12);
  }

  private static Color _getSharedColor(int rgb)
  {
    Color sharedColor = _sColorCache.get(Integer.valueOf(rgb));

    if (sharedColor == null)
    {
      sharedColor = new Color(rgb);
      _sColorCache.put(Integer.valueOf(rgb), sharedColor);
    }

    return sharedColor;
  }

  private static Color _getSharedColor(int r, int g, int b)
  {
    return _getSharedColor(((r << 16) & 0xff0000) |
                           ((g << 8)  & 0x00ff00) |
                            (b        & 0x0000ff));
  }

  // Strips whitespace from start/end of the string
  private static String _stripWhitespace(String str)
  {
    if (str == null)
      return null;

    int length = str.length();
    int startIndex = 0;

    while (startIndex < length)
    {
      if (Character.isWhitespace(str.charAt(startIndex)))
        startIndex++;
      else
        break;
    }

    int endIndex = length;
    while (endIndex > 0)
    {
      if (Character.isWhitespace(str.charAt(endIndex - 1)))
        endIndex--;
      else
        break;
    }

    if ((startIndex == 0) && (endIndex == length))
      return str;

    if (endIndex <= startIndex)
      return null;

    return str.substring(startIndex, endIndex);
  }
  
  /**
   * Resolve the uri that will be output to the generated CSS file
   * @param styleSheetName - the name of the Skin's stylesheet. We use this in any warning 
   * messages.
   * @param baseURI - absolute base URI pointing to the directory 
   * which contains the skin style sheet.
   * @param url - the url is the CSS property value that contains url(). For example, 
   * "url('/skins/purple/abc.png')"
   * @return The resolved uri. It will be run through the externalContext.encodeResourceURL
   * method.
   */  
  private static String _resolveURL(
    String styleSheetName,
    String baseURI,
    String url)
  {
    int endIndex = -1;
    int index = url.indexOf("url(");
    StringBuilder builder = new StringBuilder();
    // this loop takes care of the usecase where there can be more than
    // one url, like this: 
    // background-image: url("/skins/purple/images/btns.gif"), 
    // url("/skins/purple/images/checkdn.gif");
    while(index >= 0)
    {
      // Appends values before url()
      builder.append(url, endIndex + 1, index);
      
      endIndex = url.indexOf(')', index + 3);
      String uri = url.substring(index + 4, endIndex);

      // Trim off 
      int uriLength = uri.length();
      if (uriLength > 0)
      {
        if ((uri.charAt(0) == '\'' && uri.charAt(uriLength - 1) == '\'') ||
            (uri.charAt(0) == '"' && uri.charAt(uriLength - 1) == '"'))
        {
          uri = uri.substring(1, uriLength - 1);
          uriLength = uriLength - 2;
        }
      }

      if(uriLength == 0)
      {
        // url() or url('') found, should not happen.
        _LOG.warning("EMPTY_URL_IN_STYLE_SHEET", styleSheetName);
      }
      
      builder.append("url(");
      // At this point we have the uri -- the part within the url().
      // resolve just that part, and put it back within the url()
      String resolvedURI = _resolveCSSURI(styleSheetName, baseURI, uri);
      builder.append(resolvedURI);

      builder.append(')');      

      
      index = url.indexOf("url(", endIndex);
    }
    
    builder.append(url, endIndex + 1, url.length());

    // Don't change anything
    return builder.toString();
  }
  
  /**
   * Resolve the uri that will be output to the generated CSS file
   * @param styleSheetName - the name of the Skin's stylesheet. We use this in any warning 
   * messages.
   * @param baseURI - absolute base URI pointing to the directory 
   * which contains the skin style sheet.
   * @param uri - a uri.
   * @return The resolved uri. It will be run through the externalContext.encodeResourceURL
   * method.
   */
  private static String _resolveCSSURI (
  String styleSheetName,
  String baseURI,
  String uri)
  {
    // defaults to not converting the uri
    // this handles the case where the uri starts with http:
    String resolvedURI = uri;
    FacesContext facesContext = FacesContext.getCurrentInstance();
    assert(facesContext != null);
    ExternalContext externalContext = facesContext.getExternalContext();
    
    if(uri.charAt(0) == '/')
    {
      int uriLength = uri.length();
      // A transformation is required
      if(uriLength > 1 && uri.charAt(1) == '/')
      {
        // Double slashes, trim one and do not add context root before
        resolvedURI = uri.substring(1, uriLength);
      }
      else
      {
        // Single slash, add context path.
        String contextPath = externalContext.getRequestContextPath();
        
        assert contextPath.charAt(0) == '/';
        //if(contextPath.charAt(0) != '/')
        //{
        //  // Should not happen, but never too prudent
        //  builder.append('/');
        //}
        
        assert contextPath.charAt(contextPath.length() - 1) != '/';
        //if(contextPath.charAt(contextPath.length() - 1) == '/')
        //{
        //  // Should not happen, but better safe than sorry.
        //  builder.append(contextPath, 0, contextPath.length() - 1);
        //}
        //else
        //{
        StringBuilder builder = new StringBuilder(contextPath.length() + uri.length());
        builder.append(contextPath);
        //}
        builder.append(uri);
        resolvedURI = builder.toString();
      }
    }
    else if(_isRelativeURI(uri))
    {
      // Convert relative URI values to absolute, since
      // relative values will be resolved relative to the
      // generated style sheet, not the source CSS file.
      // e.g., if uri is "../../skins/purple/xyz.gif" and baseURI is /trinidad-context/skins/purple 
      // we get "/trinidad-context/skins/purple/xyz.gif"
      // 
      resolvedURI = getAbsoluteURIValue(styleSheetName, baseURI, uri);
    }
    return externalContext.encodeResourceURL(resolvedURI);

  }

  /**
   * Tests whether the specified uri is relative
   * @param uri - a uri
   * @return true if the uri is a relative uri. That is, it doesn't start with '/' and
   * it doesn't have a ':' in the string.
   */
  private static boolean _isRelativeURI(String uri)
  {
    return ((uri.charAt(0) != '/') && (uri.indexOf(':') < 0));
  }


  /**
   * Determines if the specified value contains a CSS url. The URLs are
   * detected by finding usage of url() function.
   * 
   * @param value
   * 
   * @return <code>true</code> if the specified value contains an URL, 
   *         <code>false</code> otherwise.
   */
  private static boolean _containsURL(String value)
  {
    if(value == null)
    {
      return false;
    }
    
    return value.indexOf("url(") >= 0;
  }  
  
  

  // CSS values
  private static final String _NORMAL_STYLE   = "normal";
  private static final String _ITALIC_STYLE   = "italic";
  private static final String _OBLIQUE_STYLE  = "oblique";
  private static final String _NORMAL_WEIGHT  = "normal";
  private static final String _BOLD_WEIGHT    = "bold";
  private static final String _BOLDER_WEIGHT  = "bolder";
  private static final String _LIGHTER_WEIGHT = "lighter";

  // Warning for invalid colors
  private static final String _INVALID_COLOR       = "Invalid color: ";
  private static final String _INVALID_FONT_SIZE   = "Invalid font size: ";
  private static final String _INVALID_FONT_STYLE  = "Invalid font style: ";
  private static final String _INVALID_FONT_WEIGHT = "Invalid font weight: ";
  private static final String _INVALID_LENGTH      = "Invalid length: ";
  private static final String _INVALID_PERCENTAGE  = "Invalid percentage: ";

  // We keep a cache of shared Color instances, hashed by RGB value, so
  // that we don't end up with one Color instance for each color in each
  // cache key in the Tecate image cache.
  private static final Map<Integer, Color> _sColorCache = 
    Collections.synchronizedMap(new LRUCache<Integer, Color>(50));

  // CSS named color values
  private static final Object[] _NAMED_COLORS = new Object[]
  {
    "black",   _getSharedColor(0x000000),
    "white",   _getSharedColor(0xffffff),
    "gray",    _getSharedColor(0x808080),
    "red",     _getSharedColor(0xff0000),
    "green",   _getSharedColor(0x008000),
    "blue",    _getSharedColor(0x0000ff),
    "yellow",  _getSharedColor(0xffff00),
    "aqua",    _getSharedColor(0x00ffff),
    "fuchsia", _getSharedColor(0xff00ff),
    "lime",    _getSharedColor(0x00ff00),
    "maroon",  _getSharedColor(0x800000),
    "navy",    _getSharedColor(0x000080),
    "olive",   _getSharedColor(0x808000),
    "purple",  _getSharedColor(0x800080),
    "silver",  _getSharedColor(0xc0c0c0),
    "teal",    _getSharedColor(0x008080)
  };

  // CSS2 system color names
  private static final Object[] _SYSTEM_COLORS = new Object[]
  {
    "activeborder",         Boolean.TRUE,
    "activecaption",        Boolean.TRUE,
    "appworkspace",         Boolean.TRUE,
    "background",           Boolean.TRUE,
    "buttonface",           Boolean.TRUE,
    "buttonhighlight",      Boolean.TRUE,
    "buttonshadow",         Boolean.TRUE,
    "buttontext",           Boolean.TRUE,
    "captiontext",          Boolean.TRUE,
    "graytext",             Boolean.TRUE,
    "highlight",            Boolean.TRUE,
    "highlighttext",        Boolean.TRUE,
    "inactiveborder",       Boolean.TRUE,
    "inactivecaption",      Boolean.TRUE,
    "inactivecaptiontext",  Boolean.TRUE,
    "infobackground",       Boolean.TRUE,
    "infotext",             Boolean.TRUE,
    "menu",                 Boolean.TRUE,
    "menutext",             Boolean.TRUE,
    "scrollbar",            Boolean.TRUE,
    "threeddarkshadow",     Boolean.TRUE,
    "threedface",           Boolean.TRUE,
    "threedhighlight",      Boolean.TRUE,
    "threedlightshadow",    Boolean.TRUE,
    "threedshadow",         Boolean.TRUE,
    "window",               Boolean.TRUE,
    "windowframe",          Boolean.TRUE,
    "windowtext",           Boolean.TRUE
  };

  // Values for absolute and relative font-size keywords
  private static final Object[] _NAMED_FONTS_SIZES = new Object[]
  {
    // Some comments on the sizes that we have chosen for absolute/relative
    // keywords.  We are using sizes which seem to make sense given that
    // the parsed font-sizes are only used for image generation purposes.
    // So, we could use's IE's odd default values for the absolute size
    // keywords, or we could use Netscape's default values.  Instead, we
    // are using reasonable defaults for image generation.  If you don't
    // like it, don't use absolute size keywords for the image-related
    // styles.  Also, we can't really handle the relative size keywords
    // as they are intended to be used, so we just use some reasonable
    // fixed defaults - it's better than throwing a PropertyParseException.
    // Again, if you don't like these values, don't use these keywords
    // for image-related styles!
    "xx-small", 8,
    "x-small",  9,
    "small",    10,
    "medium",   12,
    "large",    14,
    "x-large",  16,
    "xx-large", 18,
    "smaller",  10,
    "larger",   14
  };
  

  // Set of values that are legal for url() values
  private static final Set<String> _URI_PROPERTIES = new HashSet<String>();
  static
  {
    _URI_PROPERTIES.add("background-image");
    _URI_PROPERTIES.add("cue-after");
    _URI_PROPERTIES.add("cue-before");
    _URI_PROPERTIES.add("list-style-image");
  }

  // Set of values that are legal for url() values
  private static final Set<String> _SPECIAL_URI_VALUES = new HashSet<String>();
  static
  {
    _SPECIAL_URI_VALUES.add("none");
    _SPECIAL_URI_VALUES.add("inherit");
  }  
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CSSUtils.class);

}
