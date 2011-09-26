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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import java.awt.Color;

import java.beans.Beans;
import java.io.File;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Random;

import javax.faces.component.UIComponent;
import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.style.Style;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.share.data.ServletRequestParameters;
import org.apache.myfaces.trinidadinternal.share.url.EncoderUtils;
import org.apache.myfaces.trinidadinternal.share.util.FastMessageFormat;
import org.apache.myfaces.trinidadinternal.style.PropertyParseException;
import org.apache.myfaces.trinidadinternal.style.CoreStyle;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.composite.PoppedAttributeBoundValue;
import org.apache.myfaces.trinidadinternal.ui.composite.RootAttributeBoundValue;
import org.apache.myfaces.trinidadinternal.ui.composite.RootChildBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.AndBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.ComparisonBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.DefaultingBoundValue;
import org.apache.myfaces.trinidad.util.IntegerUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/BaseLafUtils.java#0 $) $Date: 10-nov-2005.18:52:57 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BaseLafUtils implements UIConstants
{

  public static String appendURLArgument(
    String baseURL,
    String key,
    String value
    )
  {
    return EncoderUtils.appendURLArguments(baseURL,
                                           new String[] { key, value });
  }

  public static String appendURLArguments(
    StringBuffer buffer,
    String baseURL,
    String[] keysAndValues
    )
  {

    return EncoderUtils.appendURLArguments(buffer, baseURL, keysAndValues);
  }


  public static String appendURLArguments(
    String baseURL,
    String[] keysAndValues
    )
  {

    return EncoderUtils.appendURLArguments( baseURL, keysAndValues);
  }



  /**
   * Returns the attribute as a String
   */
  public static String getStringAttributeValue(
    UIXRenderingContext context,
    UINode           node,
    AttributeKey     key
    )
  {
    Object attrValue = node.getAttributeValue(context, key);

    return (attrValue != null) ? attrValue.toString() : null;
  }


  /**
   * Returns the name of the current parent form
   */
  public static String getParentFormName(
    UIXRenderingContext context
    )
  {
    return (String)context.getProperty(MARLIN_NAMESPACE, FORM_NAME_PROPERTY);
  }


  public static boolean isRightToLeft(
    UIXRenderingContext context
    )
  {
    return context.getLocaleContext().isRightToLeft();
  }




  public static Object getTransformedID(
    UIXRenderingContext context,
    Object           idObject,
    boolean          isSubID
    )
  {
    if ( idObject != null  )
    {
      // if not in editable mode don't render sub-id's
      if ( isSubID )
        return null;
    }

    return idObject;
  }


  /**
   * Returns the nearest rendered ancestor of the specified child node
   * which has the desired local name.
   * @param context a RenderingContext
   * @param child the node whose ancestor is being sought
   * @param namespaceURI the namespace of the ancestor being sought
   * @param name the local name of the ancestor being sought
   */
  public static UINode getRenderedAncestorByName(
    UIXRenderingContext context,
    UINode           child,
    String           namespaceURI,
    String           name
    )
  {
    int ancestorCount = context.getRenderedAncestorNodeCount();

    for (int i = 1; i <  ancestorCount; i++)
    {
      UINode curr = context.getRenderedAncestorNode(i);

      if ((namespaceURI == curr.getNamespaceURI()) &&
          name.equals(curr.getLocalName()))
      {
        return curr;
      }
    }

    return null;
  }


  /**
   * Convenience function for creating and setting a
   * RootAttributeBoundValue that points to the root of the local
   * rendering node stack with the same
   * key as the attribute will be registered under.
   */
  public static void setRootBoundValue(
    MutableUINode node,
    AttributeKey  attrKey
    )
  {
    node.setAttributeValue(
                    attrKey,
                    RootAttributeBoundValue.getBoundValue(attrKey));
  }


  /**
   * Gets a property stored on the context, using the BLAF namespace.
   */
  public static Object getRenderingProperty(
    UIXRenderingContext context,
    Object           key
    )
  {
    return context.getProperty(MARLIN_NAMESPACE, key);
  }

  /**
   * Gets a property stored on the context, using the Marlin namespace.
   */
  public static Object getRenderingProperty(
    UIXRenderingContext context,
    Object           key,
    Object           defaultValue
    )
  {
    Object value = BaseLafUtils.getRenderingProperty(context, key);
    return (value == null) ? defaultValue : value;
  }


  /**
   * Stores a property on the context, using the BLAF namespace.
   */
  public static void setRenderingProperty(
    UIXRenderingContext context,
    Object           key,
    Object           value
    )
  {
    context.setProperty(MARLIN_NAMESPACE, key, value);
  }

  /**
   * Pushes the attribute value into the rendering context property,
   * storing the old value as a local property to be restored later.
   *
   * @see #pushRenderingProperty
   * @see #popRenderingProperty
   */
  public static void pushAttributeAsRenderingProperty(
    UIXRenderingContext  context,
    UINode            node,
    AttributeKey      attrKey)
  {
    Object value = node.getAttributeValue(context, attrKey);
    pushRenderingProperty(context, attrKey, value);
  }


  /**
   * Pushes the attribute value into the rendering context property,
   * as a String, storing the old value as a local property to be restored
   * later.
   *
   * @see #pushRenderingProperty
   * @see #popRenderingProperty
   */
  public static void pushAttributeAsStringRenderingProperty(
    UIXRenderingContext  context,
    UINode            node,
    AttributeKey      attrKey)
  {
    Object value = node.getAttributeValue(context, attrKey);
    pushRenderingProperty(context, attrKey,
                          (value != null) ? value.toString() : null);
  }


  /**
   * Pushes a new value into the rendering context property,
   * storing the old value as a local property to be restored later.
   *
   * @see #popRenderingProperty
   */
  public static void pushRenderingProperty(
    UIXRenderingContext  context,
    Object            key,
    Object            value)
  {
    pushRenderingProperty(context, key, key, value);
  }

  /**
   * Pushes a new value into the rendering context property,
   * storing the old value as a local property to be restored later.
   *
   * @see #popRenderingProperty
   */
  public static void pushRenderingProperty(
    UIXRenderingContext  context,
    Object            key,
    Object            localKey,
    Object            value)
  {
    // get the original property value and store it as a local property
    // so we can restore it later
    Object propertyValue = context.getProperty(MARLIN_NAMESPACE, key);
    if (propertyValue != _DEFAULT_RENDERING_PROPERTY_VALUE)
      context.setLocalProperty(localKey, propertyValue);

    // set the attribute value as a context property to make the value
    // visible to children without needing to walk the ancestor node tree
    setRenderingProperty(context, key, value);
  }

  /**
   * Pops a previously pushed local value back into the rendering context
   * property, setting the local property value back to null.
   *
   * @see #pushRenderingProperty
   */
  public static void popRenderingProperty(
    UIXRenderingContext  context,
    Object            key)
  {
    popRenderingProperty(context, key, key);
  }

  /**
   * Pops a previously pushed local value back into the rendering context
   * property, setting the local property value back to null.
   *
   * @see #pushRenderingProperty
   */
  public static void popRenderingProperty(
    UIXRenderingContext  context,
    Object            key,
    Object            localKey)
  {
    // restore the context property from the local property value
    Object value = context.getLocalProperty(0, localKey,
                                            _DEFAULT_RENDERING_PROPERTY_VALUE);
    context.setLocalProperty(localKey, null);
    setRenderingProperty(context, key, value);
  }

  /**
   * Returns the value of the specified attribute from the local
   * Renderer stack frame if possible, or from the node, and caches
   * the result
   */
  public static Object getLocalAttribute(
    UIXRenderingContext context,
    UINode           node,
    AttributeKey     attrKey
    )
  {
    Object result = context.getLocalProperty(0, attrKey, _DOES_NOT_EXIST);

    if (result == _DOES_NOT_EXIST)
    {
      // cache the attribute value for the next time
      result = node.getAttributeValue(context, attrKey);

      context.setLocalProperty(attrKey, result);
    }

    return result;
  }


  /**
   * Returns the value of the specified String attribute from the local
   * Renderer stack frame if possible, or from the node, and caches
   * the result
   */
  public static String getLocalTextAttribute(
    UIXRenderingContext context,
    UINode           node,
    AttributeKey     attrKey
    )
  {
    Object result = context.getLocalProperty(0, attrKey, _DOES_NOT_EXIST);

    if (result == _DOES_NOT_EXIST)
    {
      // cache the attribute value for the next time
      result = node.getAttributeValue(context, attrKey);

      if (result != null)
      {
        if (result instanceof Integer)
        {
          result = IntegerUtils.getString(((Integer)result).intValue());
        }
        else
        {
          result = result.toString();
        }
      }

      context.setLocalProperty(attrKey, result);
    }

    return (String) result;
  }


  /**
   * Returns the value of the specified Boolean attribute from the local
   * Renderer stack frame if possible, or from the node, and caches
   * the result
   */
  public static boolean getLocalBooleanAttribute(
    UIXRenderingContext context,
    UINode           node,
    AttributeKey     attrKey,
    boolean          defaultValue
    )
  {
    Object result = context.getLocalProperty(0, attrKey, _DOES_NOT_EXIST);

    if (result == _DOES_NOT_EXIST)
    {
      // cache the attribute value for the next time
      result = node.getAttributeValue(context, attrKey);

      if (result == null)
      {
        result = (defaultValue) ? Boolean.TRUE : Boolean.FALSE;
      }

      context.setLocalProperty(attrKey, result);
    }

    return Boolean.TRUE.equals(result);
  }


  /**
   * Returns the value of the specified named child from the local
   * Renderer stack frame if possible, or from the node, and caches
   * the result
   */
  public static UINode getLocalNamedChild(
    UIXRenderingContext context,
    UINode           node,
    String           childName
    )
  {
    Object result = context.getLocalProperty(0, childName, _DOES_NOT_EXIST);

    if (result == _DOES_NOT_EXIST)
    {
      UINode namedChild = node.getNamedChild(context, childName);

      // cache the named child for the next time
      context.setLocalProperty(childName, namedChild);

      return namedChild;
    }
    else
    {
      return (UINode)result;
    }
  }

  /**
   * Adds up the lengths of each individual String.
   * @param strings Each element must be a String and cannot be null
   * @return the sum total of all the lengths of the Strings
   */
  public static int getLength(String[] strings)
  {
    int len=0;
    for(int i=0, sz=strings.length; i<sz; i++)
    {
      len+=strings[i].length();
    }
    return len;
  }


  /**
   * Generates an ID that will be unique in the current rendering
   * context.
   */
  public static String generateUniqueID(UIXRenderingContext context)
  {
    // We use MutableInt instead of an Integer because
    // we're out of range for IntegerUtils;  this way, we're at
    // least not creating a new object every time...
    MutableInt countObj = (MutableInt) getRenderingProperty(context,
                                                  _UNIQUE_ID_COUNT_PROPERTY);
    int count;
    if (countObj == null)
    {
      countObj = new MutableInt();
      // Start randomly.  We could always start at zero,
      // but that'd break if the user creates multiple RenderingContexts
      // in a same page (e.g., portal pages?).  I'm making a guess
      // that we shouldn't need to generate more than 100 IDs on
      // a page.
      count = _getUniqueIDStartingCount(context);
      context.setProperty(MARLIN_NAMESPACE,
                          _UNIQUE_ID_COUNT_PROPERTY,
                          countObj);
    }
    else
    {
      count = countObj.value;
    }

    countObj.value = count + 1;

    if (count == 0)
      return _UNIQUE_ID_PREFIX;

    StringBuffer buffer = new StringBuffer(_UNIQUE_ID_PREFIX.length() + 5);
    buffer.append(_UNIQUE_ID_PREFIX);
    _appendIntAsReversedChars(buffer, count);
    return new String(buffer);
  }

  private static int _getUniqueIDStartingCount(UIXRenderingContext context)
  {
    if (UIConstants.FACET_PORTLET.equals(
           context.getRendererManager().getFacet()) ||
        (context.getPartialPageContext() != null))
    {
      synchronized (_STARTING_ID_COUNT)
      {
        return (Math.abs(_STARTING_ID_COUNT.nextInt() % 90) + 1) * 100;
      }
    }

    return 0;
  }



  static public char getCharacterAttr(
    UIXRenderingContext context,
    UINode           node,
    AttributeKey     key)
  {
    Object charObj = node.getAttributeValue(context, key);
    return getCharacter(charObj);
  }

  static public final char CHAR_UNDEFINED = (char) -1;

  static public char getCharacter(Object charObj)
  {
    char ch = CHAR_UNDEFINED;
    if (charObj instanceof Character)
    {
      ch = ((Character) charObj).charValue();
    }
    else if (charObj instanceof String)
    {
      String str = (String) charObj;
      if (str.length() == 1)
        ch = str.charAt(0);
    }

    return ch;
  }


  /**
   * Return an URL based on the current Configuration.
   * <p>
   * This method uses the RenderingContext.CONTEXT_URI_PROPERTY to
   * resolve URLs, which means that it never returns URLs to shared
   * installable resources.  As such, this method should only be
   * used to get URLs for resources which are always known to be
   * local to the current application's context path - such as the
   * image cache.  For resources which could be shared across applications,
   * such as images, jsps, jsLibs, getSharedConfiguredURL() should be
   * used instead.
   */
  public static String getConfiguredURL(
    UIXRenderingContext context,
    Object           directoryKey)
  {
    Configuration config = context.getConfiguration();
    String contextURI = (String)context.getProperty(MARLIN_NAMESPACE,
                                   UIXRenderingContext.CONTEXT_URI_PROPERTY);

    String baseURL = config.getURI(directoryKey,
                                   contextURI);

    // Check for design time and modify URL if needed
    if (Beans.isDesignTime())
    {
      baseURL = _generateDesignTimeURL(context,baseURL);
    }

    return baseURL;
  }

  /**
   * This method returns the modified URL for design time to pick up the resources
   * from the Trinidad jar, and dynamic resources from temp cache directory.
   */
  @SuppressWarnings("unchecked")
  private static String _generateDesignTimeURL(
    UIXRenderingContext context,
    String baseURL
    )
  {
    String designTimeURL;
    String tempCacheDirectory;
    String resourceDirectoryPath;

    Configuration configInfo = context.getConfiguration();
    ExternalContext external = context.getFacesContext().getExternalContext();

    String contextURI = external.getRequestContextPath();

    // Ignore javascripts for design time
    boolean isJavascript =
       (baseURL.indexOf((String)Configuration.JSLIBS_DIRECTORY)!=-1);

    if (!baseURL.equals(configInfo.getURI(Configuration.IMAGES_CACHE_DIRECTORY,
                                          contextURI))
        && !baseURL.equals(configInfo.getURI(Configuration.STYLES_CACHE_DIRECTORY,
                                             contextURI))
        && !isJavascript)
    {
       designTimeURL =
         (ClassLoaderUtils.getResource( _BASE_DIRECTORY + baseURL)).toString();
    }
    else
    {
      Map<String, Object> appMap = external.getApplicationMap();
      if (appMap.get("javax.servlet.context.tempdir")==null)
      {
        resourceDirectoryPath = baseURL.substring(baseURL.indexOf('/') + 1);
        tempCacheDirectory    = System.getProperty("java.io.tmpdir");

        designTimeURL = "file:" + File.separator + tempCacheDirectory
                         + resourceDirectoryPath;
      }
      else
      {
        resourceDirectoryPath = baseURL;
        tempCacheDirectory    =
          ((File)appMap.get("javax.servlet.context.tempdir")).getAbsolutePath();
        designTimeURL = tempCacheDirectory + resourceDirectoryPath;
      }
    }

    return designTimeURL;
  }

  /**
   * @return a BoundValue that returns Boolean.TRUE only if the current
   *  UINode has the specified named child and that named child does not
   *  have rendered attribute set to false.
   */
  public static BoundValue createIsRenderedBoundValue(
    String childName
    )
  {
    return createIsRenderedBoundValue(
                                 RootChildBoundValue.getBoundValue(childName));

  }

  /**
   * produces a BoundValue that checks to see if a given UINode must be
   * rendered.
   * @param targetUINodeValue This bound value must return a
   *  UINode (or null). If a UINode is returned and this UINode does not have
   *  its rendered attribute set to false, then the value returned by this
   *  method is true.
   * @return a BoundValue that returns Boolean.TRUE or Boolean.FALSE
   */
  public static BoundValue createIsRenderedBoundValue(
    BoundValue targetUINodeValue
    )
  {
    return new AndBoundValue
               (
                 ComparisonBoundValue.createExistsValue(targetUINodeValue),
                 new DefaultingBoundValue
                 (
                   new PoppedAttributeBoundValue(targetUINodeValue,
                                                 RENDERED_ATTR),
                   Boolean.TRUE
                 )
               );
  }


  protected BaseLafUtils()
  {
  }


  // =-=AEW Move to BaliShare?  Note that
  // this code actually appends the integer _backwards_,
  // because its faster and we only care about uniqueness
  static private void _appendIntAsReversedChars(
    StringBuffer buffer,
    int          count)
  {
    if (count < 0)
    {
      buffer.append('_');
      count = -count;
    }

    while (count > 0)
    {
      count = count - 1;
      buffer.append((char) ('a' + (count % 26)));
      count = count / 26;
    }
  }


  // =-=AEW Move to Bali Share?
  static private final class MutableInt
  {
    public int value;
  }


  /**
   * Returns the total string length of an array of key/value pairs, plus
   * any per pair overhead
   */
  public static int getKeyValueArraySize(
    Object[] keyValues,
    int      perPairOverhead
    )
  {
    if (keyValues == null)
      return 0;

    int keyValueCount = keyValues.length;

    if (keyValueCount == 0)
      return 0;

    if (keyValueCount % 2 != 0)
      throw new IllegalArgumentException("unequal key/value count");

    if (keyValues instanceof String[])
    {
      String[] keyValueStrings = (String[])keyValues;

      int keyValueSize = 0;

      // get the total keyValue length
      for (int i = 0; i < keyValueCount; i += 2)
      {
        String key = keyValueStrings[i];

        if (key != null)
        {
          keyValueSize += key.length();

          String value = keyValueStrings[i + 1];

          if (value != null)
          {
            keyValueSize += value.length();
          }
        }
      }

      // add in any per key/value oeverhead
      keyValueSize += perPairOverhead * keyValueCount;

      return keyValueSize;
    }
    else
    {
      // take a guess
      return keyValues.length * 13;
    }
  }

  /**
   * Encodes an array of key value pairs as a single value appended to
   * the baseName, if any;
   */
  @SuppressWarnings("unchecked")
  public static String encodeCompoundKeyValues(
    Object[] keyValues
    )
  {
    // normal overhead is 1, but add 2 for encoding growth
    int keyValueSize = getKeyValueArraySize(keyValues, 3);

    if (keyValueSize > 0)
    {
      if (keyValues!=null)
      {
      return ServletRequestParameters.encodeCompoundKeyValues(
                                     (Arrays.asList(keyValues)).iterator(),
                                     keyValueSize);
    }
    else
    {
        return ServletRequestParameters.encodeCompoundKeyValues(
                                     Collections.EMPTY_LIST.iterator(),
                                     keyValueSize);
      }
    }
    else
    {
      return null;
    }
  }

  /**
   * Encodes an array of key value pairs as a single value appended to
   * the baseName, if any;
   */
  public static String encodeCompoundKeyValues(
    Object[] keyValues,
    Object[] keyExchanges
    )
  {
    // normal overhead is 1, but add 2 for encoding growth
    int keyValueSize = getKeyValueArraySize(keyValues, 3);


    // normal overhead is 1, but add for encoding growth
    int keyExchangesSize = getKeyValueArraySize(keyExchanges, 3);

    if (keyValueSize > 0 || keyExchangesSize > 0)
    {
        if (keyValues==null)
    {
          keyValues = new Object[0];
    }

        if (keyExchanges==null)
  {
          keyExchanges = new Object[0];
  }

        return ServletRequestParameters.encodeCompoundKeyValues(
                                     (Arrays.asList(keyValues)).iterator(),
                                     keyValueSize,
                                     (Arrays.asList(keyExchanges)).iterator(),
                                     keyExchangesSize);

      }
      else
      {
      return null;
      }
    }




  /**
   * Gets the character encoding of the output.
   */
  public static String getOutputEncoding(UIXRenderingContext context)
  {
    return context.getFacesContext().getResponseWriter().getCharacterEncoding();
  }

  /**
   * Format a string with the specified pattern and parameters, caching
   * the FastMessageFormat on the RenderingContext.
   */
  public static String getFormattedString(
    UIXRenderingContext context,
    String pattern,
    String[] parameters
    )
  {
    // # for perf bug #1301909, store the format on the RenderingContext
    // per rendering pass. Eventually we want to store it across passes
    // based on Locale...
    FastMessageFormat formatter =
      (FastMessageFormat)context.getProperty(MARLIN_NAMESPACE, pattern);

    if (formatter == null)
    {
      formatter = new FastMessageFormat(pattern);
      context.setProperty(MARLIN_NAMESPACE, pattern, formatter);
    }

    return formatter.format(parameters);
  }

  /**
   * Pushes the specified style attributes onto a stack which
   * is used to track the current background color.  Each call
   * to pushStyleAttrs() should be accompanied by a call to
   * popStyleAttr() when the styles are no longer in scope.
   * The background color can be retrieved at any time by calling
   * getBackgroundColor().
   */
  public static void pushStyleAttrs(
    UIXRenderingContext context,
    String           styleClass,
    CoreStyle            inlineStyle
    )
  {
    _getStyleStack(context).push(styleClass, inlineStyle);
  }

  /**
   * Pops the style attributes stack.
   */
  public static void popStyleAttrs(UIXRenderingContext context)
  {
    _getStyleStack(context).pop();
  }

  /**
   * Returns the current background color.
   */
  public static Color getBackgroundColor(UIXRenderingContext context)
  {
    return _getStyleStack(context).getBackgroundColor(context);
  }

  // Convenience method to pull the StyleStack off of the RenderingContext
  private static StyleStack _getStyleStack(UIXRenderingContext context)
  {
    StyleStack stack = (StyleStack)getRenderingProperty(context,
                                                        _STYLE_STACK_KEY);

    if (stack == null)
    {
      stack = new StyleStack();

      // Default the background color to af|body, just in
      // case the client forgot to use a BodyBean.  Note: af|body
      // may not always be the right background color.  For example,
      // if a UINode subtree is rendered into some other content
      // where the background color is already rendered, then af|body
      // might be the wrong color.  But in that case, the root
      // UINode should push the correct style attributes to change
      // the color as needed.
      stack.push("af|body", null);

      setRenderingProperty(context, _STYLE_STACK_KEY, stack);
    }

    return stack;
  }

  // A stack implementation for styles.  I wasn't quite sure what the
  // best implementation for this should be, so I'm abstracting
  // away the implementation using this class.
  //
  // Note: We probably should just use java.util.Stack, but there
  // are two reasons why I decided not to.  First, java.util.Stack
  // is synchronized - we don't need synchronization since we always
  // access the stack from one and only one thread.  But more importantly,
  // we actually need to be able to traverse the stack (without popping it)
  // as we look for the topmost entry with a background color.  We can do
  // this with Stack by treating it like a Vector, but we have to make
  // assumptions about how Stack is implemented, which just seems wrong.
  // Instead we use a little linked list structure which does exactly
  // what we need with no synchronization overhead and should be really
  // fast.
  private static class StyleStack
  {
    // Push style attributes onto the stack
    public void push(
      String styleClass,
      CoreStyle  inlineStyle
      )
    {
      // It really seems like it would be more efficient to
      // pool Entry instances, but Sun says not to!
      _top = new Entry(styleClass, inlineStyle, _top);
    }

    // Pop the stack
    public void pop()
    {
      assert (_top != null);

      Entry oldTop = _top;
      _top = _top.next;

      oldTop.next = null;
    }

    // Returns the current background color
    public Color getBackgroundColor(UIXRenderingContext context)
    {
      // Short-circuit if the top entry already has a background
      // color set.
      if ((_top != null) && (_top.background != null))
        return _top.background;

      // Otherwise, we look through the stack until we find an
      // entry which has a background color.
      Entry entry = _top;

      while (entry != null)
      {
        Color background = entry.background;

        if ((background != null) ||
            (background = _resolveBackground(context, entry)) != null)
        {
          // Once we've got a background, loop back through and
          // set it on each entry at the top of the stack which
          // doesn't already have a background
          entry = _top;
          while ((entry != null) && (entry.background == null))
          {
            entry.background = background;
            entry = entry.next;
          }

          return background;
        }

        entry = entry.next;
      }

      // We should always get a background color - at least from .OraBody
      return null;
    }

    // Determines the background color for the specified entry.
    // If the styles referenced by the entry define a background
    // color, the background color is set on the entry and
    // returned.  Otherwise, the entry is not changed and null
    // is returned.
    private static Color _resolveBackground(
      UIXRenderingContext context,
      Entry            entry
      )
    {
      // First, try to get the background from the inline style
      Color background = _getBackground(entry.inlineStyle);

      // If we don't get a background color from the inline
      // style, check the style class
      if ((background == null) && (entry.styleClass != null))
      {
        /** =-=jmw removed StyleMap
        // We need to look up the style class in the style map
        StyleMap map = context.getStyleContext().getStyleMap();
        if (map != null)
        {
          StyleContext styleContext = context.getStyleContext();
          Style style = map.getStyleByClass(styleContext, entry.styleClass);

          background = _getBackground((CoreStyle)style);
         
        }
         */
      }


      // Update the Entry's background value
      entry.background = background;

      // Preferably, this would be a warning, but it's currently
      // firing a lot!
      _LOG.fine("Could not find background color");
      return background;
    }

    // Gets the background color from a Style object
    private static Color _getBackground(CoreStyle style)
    {
      if (style != null)
      {
        try
        {
          return (Color)style.getParsedProperty(CoreStyle.BACKGROUND_KEY);
        }
        catch (PropertyParseException e)
        {
          // Don't bother logging this - our XSS parsing code should
          // catch this.
          ;
        }
      }

      return null;
    }

    // Little inner class for storing style attrs on the stack
    private static class Entry
    {
      public final String styleClass;
      public final CoreStyle  inlineStyle;
      public       Color  background;
      public       Entry  next;

      public Entry(String styleClass, CoreStyle inlineStyle, Entry next)
      {
        this.styleClass = styleClass;
        this.inlineStyle = inlineStyle;

        this.next = next;
      }

      private Entry()
      {
        this(null, null, null);

        assert false;
      }
    }

    private Entry _top;
  }

  // UIComponent based utilities
  /**
   * Utility method to get the component's label.
   */
  public static Object getComponentLabel(UIComponent component)
  {
    Object o = component.getAttributes().get("label");
    return o;
  }

  private static final Object _DOES_NOT_EXIST = new Object();

  private static final String _UNIQUE_ID_COUNT_PROPERTY = "idCount";
  private static final String _UNIQUE_ID_PREFIX = "M__Id";

  // used to avoid unneccessary storage for rendering property default value
  private static final Object _DEFAULT_RENDERING_PROPERTY_VALUE = null;

  // Key to use for storing style stack on the RenderingContext
  private static final Object _STYLE_STACK_KEY = new Object();

  // The base directory for stored images, styles. The build script places all
  // the resources under this directory.
  private static final String _BASE_DIRECTORY = "META-INF";

  private static final Random _STARTING_ID_COUNT = new Random();
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(BaseLafUtils.class);
}
