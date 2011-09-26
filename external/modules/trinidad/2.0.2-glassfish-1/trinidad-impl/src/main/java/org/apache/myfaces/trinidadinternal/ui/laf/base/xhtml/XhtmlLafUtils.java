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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Stack;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.style.Selector;
import org.apache.myfaces.trinidad.style.Style;
import org.apache.myfaces.trinidad.style.Styles;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.AutoSubmitUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.FormRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.Scriptlet;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.share.data.ServletRequestParameters;
import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.style.ParsedPropertyKey;
import org.apache.myfaces.trinidadinternal.style.CoreStyle;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;
import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.MutableProperty;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/XhtmlLafUtils.java#0 $) $Date: 10-nov-2005.18:54:20 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class XhtmlLafUtils extends BaseLafUtils
{
  protected XhtmlLafUtils()
  {
  }


  /**
   * Copies an attribute from a source node to a destination UINode
   */
  public static void copyAttr(
    UIXRenderingContext context,
    UINode           sourceNode,
    AttributeKey     attrKey,
    MutableUINode    destNode
    )
  {
    Object value = sourceNode.getAttributeValue(context, attrKey);

    if (value != null)
    {
      destNode.setAttributeValue(attrKey, value);
    }
  }

  /**
   * Registers a scriptlet.
   */
  public static  void registerScriptlet(Object key, Scriptlet scriptlet)
  {
    XhtmlUtils.registerScriptlet(key, scriptlet);
  }

  /**
   * Encodes an Iterator key value pairs as a single Javascript Object
   * initializer, creating any needed form values.
   */
  public static String encodeJSEventObject(
    UIXRenderingContext context,
    String           formName,
    Iterator<Object> keyValues,
    int              keyValueSize
    )
  {
    if ((keyValues == null) || !keyValues.hasNext())
      return null;

    if (keyValueSize < 20)
      keyValueSize = 20;

    // add 2 for starting and ending curly quotes
    StringBuilder jsObject = new StringBuilder(keyValueSize + 2);

    // append start of javascript property
    jsObject.append('{');

    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    FormEncoder formEncoder = context.getFormEncoder();

    boolean isFirstKeyValue = true;

    try
    {
      do
      {
        if (isFirstKeyValue)
        {
          isFirstKeyValue = false;
        }
        else
        {
          jsObject.append(',');
        }

        Object key   = keyValues.next();
        Object value = keyValues.next();

        if (key != null)
        {
          String keyString = key.toString();

          jsObject.append('\'');
          jsObject.append(keyString);
          jsObject.append('\'');

          // add hidden field
          FormValueRenderer.addNeededValue(context, formName, keyString);

          jsObject.append(':');
          jsObject.append('\'');

          if (value != null)
          {
            // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
            String encodedValue =
              XhtmlLafUtils.getFormEncodedParameter(formEncoder, formName,
                                                    keyString,
                                                    value);
            jsObject.append(encodedValue);
          }

          jsObject.append('\'');
        }

      } while (keyValues.hasNext());
    }
    catch (NoSuchElementException e)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "NUMBER_OF_KEYS_AND_VALUES_MUCH_MATCH"));
    }

    // append end of javascript property
    jsObject.append('}');

    return jsObject.toString();
  }

  /**
   * Encodes an Iterator key value pairs as a single Javascript Object
   * initializer, creating any needed form values.
   */
  public static String encodeJSEventObject(
    UIXRenderingContext context,
    String           formName,
    Object[]         keyValues
    )
  {
    // overhead of ',' plus single quotes around keys and values is 5
    int keyValueSize = getKeyValueArraySize(keyValues, 5);

    if (keyValueSize > 0)
    {
      return encodeJSEventObject(context,
                                 formName,
                                 Arrays.asList(keyValues).iterator(),
                                 keyValueSize);
    }
    else
    {
      return null;
    }
  }


  /**
   * Resolves the class name to the appropriate Style object
   */
  public static CoreStyle getClassStyle(
    UIXRenderingContext context,
    Object           className
    )
  {
    if (className != null)
    {
      Styles styles = context.getStyleContext().getStyles();
      if (styles != null)
      {
        Map<Selector, Style> map = styles.getSelectorStyleMap();
        if (map != null)
          return (CoreStyle)map.get(Selector.createSelector(className.toString()));
      }
    }

    return null;
  }


  /**
   * Resolves the class name to the appropriate Style object,
   * then get a property from the Style
   */
  public static String getClassStyleProperty(
    UIXRenderingContext context,
    Object           className,
    String           propertyName
    )
  {
      Style classStyle = getClassStyle(context,
                                       className);
      if (classStyle != null)
      {
        return classStyle.getProperties().get(propertyName);
      }

      return null;
  }

  /**
   * Returns the short version of the specified style class.
   */
  public static Object getShortStyleClass(
    UIXRenderingContext context,
    Object           styleClass
    )
  {
    if (styleClass == null)
      return null;

    // =-=jmw @todo for now, map the keys here, but I want to do this
    // differently very soon.
    styleClass = context.getStyleClass(styleClass.toString());

    Object styleClasses = context.getProperty(UIConstants.MARLIN_NAMESPACE,
                                              _STYLE_CLASSES_PROPERTY);

    // If we didn't get the style classes Dictionary yet,
    // try to get it now
    if (styleClasses == null)
    {
      RenderingContext arc = RenderingContext.getCurrentInstance();
      styleClasses = context.getSkin().getStyleClassMap(arc);

      if (styleClasses == null)
        styleClasses = _NULL_STYLE_CLASSES;

      context.setProperty(UIConstants.MARLIN_NAMESPACE,
                          _STYLE_CLASSES_PROPERTY,
                          styleClasses);
    }

    // If we've got a non-null Dictionary, use it to get the
    // short style class
    Object shortStyleClass = null;
    if (styleClasses != _NULL_STYLE_CLASSES)
    {
      String styleClassKey = styleClass.toString();
      shortStyleClass = ((Map)styleClasses).get(styleClassKey);
      if (shortStyleClass != null)
        styleClass = shortStyleClass;
    }
    if (shortStyleClass == null)
    {
      // if we didn't shorten the style classes, then make sure the
      // namespace character '|' is not in the name.
      // we do the same thing in CSSUtils when we write the full selector
      // to the CSS file.
      styleClass = StyleUtils.convertToValidSelector(styleClass.toString());
    }

    return styleClass;
  }

  /**
   * Returns the mapping of a css vertical-align to a valign attribute value.
   */
  public static String mapVerticalAlignToVAlign(
    UIXRenderingContext context,
    String           verticalAlign
    )
  {
    if (verticalAlign != null)
    {
      return _sSupportedVAligns.get(verticalAlign);
    }
    else
    {
      return null;
    }
  }


  /**
   * Closes any tags started by startRenderingStyleElements
   */
  @SuppressWarnings("unchecked")
  public static void endRenderingStyleElements(
    UIXRenderingContext context
    ) throws IOException
  {
    Stack[] styleInfo = _getStyleInfo(context);

    Stack<Integer> styleFlagsStack = styleInfo[_STACK_FLAGS_INDEX];

    // get the flags of the elements started, popping the
    // current styleFlags
    int styleFlags = styleFlagsStack.pop().intValue();
    if (styleFlags != 0)
    {
      ResponseWriter writer = context.getResponseWriter();

      boolean fontEnded = false;

      //
      // close the started elements in reverse order, popping their
      // values off of the stack
      //
      for (int i = _STYLE_INFO_STACK_COUNT - 1; i > 0; i--)
      {
        if ((styleFlags & (1 << i)) != 0)
        {
          // pop off the
          styleInfo[i].pop();

          if (!fontEnded)
          {
            writer.endElement(_STYLE_ELEMENTS[i]);
            // only end font on the first info attribute that needs it
            if (i <= _FOREGROUND_INDEX)
            {
              fontEnded = true;
            }
          }
        }
      }
    }
  }

  /**
   * Renders the combination of inline and class style attributes
   * as elements
   */
  public static void startRenderingStyleElements(
    UIXRenderingContext context,
    CoreStyle            inlineStyle,
    CoreStyle            classStyle
    ) throws IOException
  {
    Stack[] styleInfo = _getStyleInfo(context);

    int flags = 0;

    //
    // if at least one Style object is present begin outputting
    // style elements
    //
    if ((inlineStyle != null) || (classStyle != null))
    {
      // handle BACKGROUND_KEY
      Object fontStyle = _getParsedStyleProperty(inlineStyle,
                                                 classStyle, CoreStyle.FONT_STYLE_KEY);

      Boolean isItalic = (CoreStyle.ITALIC_FONT_STYLE == fontStyle)
                           ? Boolean.TRUE
                           : null;


      Object fontWeight = _getParsedStyleProperty(inlineStyle,
                                                  classStyle, CoreStyle.FONT_WEIGHT_KEY);

      Boolean isBold = (CoreStyle.BOLD_FONT_WEIGHT == fontWeight)
                         ? Boolean.TRUE
                         : null;

      String fontFamilies = null;
      String foreground = null;

      String fontSize = getStyleProperty(inlineStyle,
                                         classStyle,
                                         "font-size");

      foreground = getStyleProperty(inlineStyle,
                                    classStyle,
                                    "color");


      fontFamilies = getStyleProperty(inlineStyle,
                                      classStyle,
                                      "font-family");


      boolean hasFontElement = (fontSize != null)   ||
                               (foreground != null) ||
                               (fontFamilies != null);

      ResponseWriter writer = context.getResponseWriter();

      if (hasFontElement)
      {
        int fontFlags = 0;

        // push on foreground property
        // was calling _pushStackPropertyIfDifferent, but inside table
        // only pushing if different doesn't work. See bug 2944365
        fontFlags |= _pushStackProperty(styleInfo,
                                        _FOREGROUND_INDEX,
                                        foreground);

        // push on font faces property
        // was calling _pushStackPropertyIfDifferent, but inside table
        // only pushing if different doesn't work. See bug 2944365
        fontFlags |= _pushStackProperty(styleInfo,
                                        _FONT_FACE_INDEX,
                                        fontFamilies);

        Object sizeAttr = fontSize;

        if (fontSize != null)
        {
          // try to map the raw css attribute to a font element
          // size attribute
          sizeAttr = _sSizeNameMap.get(fontSize);

          if (sizeAttr == null)
          {
            // try and map the parsed pixel value to a font
            // element
            Integer pixelSize = (Integer)_getParsedStyleProperty(
                                               inlineStyle,
                                               classStyle, CoreStyle.FONT_SIZE_KEY);

            if (pixelSize != null)
            {
              int pixSize = pixelSize.intValue();

              sizeAttr = (pixSize > _SIZE_MAPPING.length)
                           ? _SIZE_MAPPING[_SIZE_MAPPING.length - 1]
                           : _SIZE_MAPPING[pixSize];
            }
          }

          // push on font size property
          // was calling _pushStackPropertyIfDifferent, but inside table
          // only pushing if different doesn't work. See bug 2944365
          fontFlags |= _pushStackProperty(styleInfo,
                                          _FONT_SIZE_INDEX,
                                          sizeAttr);
        }

        //
        // write out the attribute values
        //
        if (fontFlags != 0)
        {
          writer.startElement("font", null);

          if ((fontFlags & (1 << _FOREGROUND_INDEX)) != 0)
          {
            writer.writeAttribute("color", foreground, null);
          }

          if ((fontFlags & (1 << _FONT_FACE_INDEX)) != 0)
          {
            writer.writeAttribute("face", fontFamilies, null);
          }

          if ((fontFlags & (1 << _FONT_SIZE_INDEX)) != 0)
          {
            writer.writeAttribute("size", sizeAttr, null);
          }

          // merge in the font flags
          flags |= fontFlags;
        }
      }

      //
      // handle italic element
      // was calling _pushStackPropertyIfDifferent, but inside table
      // only pushing if different doesn't work. See bug 2944365
      int currFlag = _pushStackProperty(styleInfo,
                                        _ITALIC_INDEX,
                                        isItalic);
      if (currFlag != 0)
      {
        flags |= currFlag;
        writer.startElement(_STYLE_ELEMENTS[_ITALIC_INDEX], null);
      }

      //
      // handle bold element
      //
      // was calling _pushStackPropertyIfDifferent, but inside table
      // only pushing if different doesn't work. See bug 2944365
      currFlag = _pushStackProperty(styleInfo, _BOLD_INDEX, isBold);

      if (currFlag != 0)
      {
        flags |= currFlag;
        writer.startElement(_STYLE_ELEMENTS[_BOLD_INDEX], null);
      }
    }

    // push the flags of the styles actually written
    Stack<Object> stackInfo = _getStyleInfoStack(styleInfo, _STACK_FLAGS_INDEX);
    stackInfo.push(flags);
  }



  /**
   * Returns the value of a parsed style property asking Style1 and then
   * Style2, if Style1 doesn't return a value.
   */
  private static Object _getParsedStyleProperty(
    CoreStyle             style1,
    CoreStyle             style2,
    ParsedPropertyKey stylePropertyKey
    )
  {
    Object value = (style1 != null)
                     ? style1.getParsedProperty(stylePropertyKey)
                     : null;

    if (value != null)
    {
      return value;
    }
    else
    {
      if (style2 != null)
      {
        return style2.getParsedProperty(stylePropertyKey);
      }
      else
      {
        return null;
      }
    }
  }

  /**
   * Returns the value of a style property asking Style1 and then
   * Style2, if Style1 doesn't return a value.
   */
  public static String getStyleProperty(
    CoreStyle  style1,
    CoreStyle  style2,
    String stylePropertyName
    )
  {
    String value = null;
    if (style1 != null)
    {
      value = style1.getProperties().get(stylePropertyName);
    }

    
    if (value != null)
    {
      return value;
    }
    else
    {
      if (style2 != null)
      {
        
        return style2.getProperties().get(stylePropertyName);
      }
      else
      {
        return null;
      }
    }
  }


  public static String createCompoundName(
    UIXRenderingContext context,
    String           formName,
    Object           keyValues
    )
  {
    return createCompoundName(context, formName, keyValues, null);

  }

  public static String createCompoundName(
    UIXRenderingContext context,
    String           formName,
    Object           keyValues,
    Object           keyExchanges
    )
  {
    String compoundName = encodeCompoundKeyValues(
                                (Object[]) keyValues,
                                (Object[]) keyExchanges);

    if (compoundName != null)
    {
      // record that we have encoded compound name
      FormValueRenderer.addNeededValue(
                                context,
                                formName,
                                ServletRequestParameters.HAS_COMPOUND_NAME);
    }

    return compoundName;
  }


   /**
   * Return the chained JavaScript
   */
  public static Object getChainedJS(
    Object evh1,
    Object evh2,
    boolean shortCircuit
    )
  {
    String evh1String = (evh1 == null) ? null : evh1.toString();
    String evh2String = (evh2 == null) ? null : evh2.toString();

    return XhtmlUtils.getChainedJS(evh1String, evh2String, shortCircuit);
  }


  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes with just a String for input.  If a String in and a String out is
   * all that is required, this version is more efficient if the String
   * does not need to be escaped.
   */
  public static String escapeJS(
    String inString
    )
  {
    return XhtmlUtils.escapeJS(inString);
  }



  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes with just a String for input.  If a String in and a String out is
   * all that is required, this version is more efficient if the String
   * does not need to be escaped.
   */
  public static String escapeJS(
    String  inString,
    boolean inQuotes
    )
  {
    return XhtmlUtils.escapeJS(inString, inQuotes);
  }


  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes.
   */
  public static void escapeJS(
    StringBuilder outBuffer,
    String       inString
    )
  {
    XhtmlUtils.escapeJS(outBuffer, inString);
  }


  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes.
   */
  public static void escapeJS(
    StringBuilder outBuffer,
    String       inString,
    boolean      inQuotes)
  {
    XhtmlUtils.escapeJS(outBuffer, inString, inQuotes);
  }

  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes.
   */
  public static void escapeJS(
    StringBuilder outBuffer,
    String       inString,
    boolean      inQuotes,
    int          escapeCount
    )
  {
    XhtmlUtils.escapeJS(outBuffer, inString, inQuotes, escapeCount);
  }

  public static void addOnSubmitRequiredValidator(
    UIXRenderingContext context,
    UINode           node,
    String           requiredMessageKey,
    Object           nodeName
  )throws IOException
  {

    boolean requiredField = Boolean.TRUE.equals(
                         node.getAttributeValue(context, REQUIRED_ATTR));

    if ( requiredField )
    {

      // Bug 2748146: Don't do validation of a disabled field! If the field is
      // disabled, the user can't have updated it (there is one way for the
      // client to hurt themselves here: by changing the disabled state as part
      // of a PPR update after the user has updated the field).
      Object disabled = node.getAttributeValue(context, DISABLED_ATTR);

      if (!Boolean.TRUE.equals(disabled))
      {
        if (nodeName != null)
        {
          UIComponent component = NodeUtils.getUIComponent(context, node);

          if (component == null)
          {
            _LOG.warning("NULL_COMPONENT_FOR_NODE", node.getLocalName());
          }

          boolean unvalidated =
                   Boolean.TRUE.equals(node.getAttributeValue(context,
                                                UIConstants.UNVALIDATED_ATTR));

          FormRenderer.addOnSubmitConverterValidators(component,
                                                      null, //converter
                                                      null, // validator
                                                      nodeName.toString(),
                                                      unvalidated,
                                                      true,  // required
                                                      requiredMessageKey);
        }
        else
          _LOG.warning("NULL_NODE_NAME_NO_VALIDATOR_ADDED", node.getLocalName());
      }
    }
  }

  /**
   * Adds an import of a Javascript library if necessary.
   * Also imports any and all dependencies of that library (again,
   * if needed).
   */
  public static void addLib(
    UIXRenderingContext context,
    Object           libKey
    ) throws IOException
  {
    XhtmlUtils.addLib(context.getFacesContext(),
                      RenderingContext.getCurrentInstance(),
                      libKey);
  }


  /**
   * Write out a script element importing a library.
   */
  public static void writeLibImport(
    UIXRenderingContext context,
    Object           libURL) throws IOException
  {
    XhtmlUtils.writeLibImport(context.getFacesContext(),
                              RenderingContext.getCurrentInstance(),
                              libURL);
  }

  /**
   * Returns the partial targets associated with the specified node
   */
  public static String[] getPartialTargets(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return getPartialTargets(context, node, null);
  }

  /**
   * Returns the partial targets associated with the specified node,
   * including the specified ID.
   */
  public static String[] getPartialTargets(
    UIXRenderingContext context,
    UINode           node,
    Object           id
    )
  {
    // Make sure partial page rendering is supported
    if (!XhtmlLafRenderer.supportsPartialRendering(context))
      return null;

    if (!PartialPageUtils.isPPRActive(context.getFacesContext()))
      return null;

    // If the ID is null, get the ID from the node
    if (id == null)
      id = node.getAttributeValue(context, ID_ATTR);

    // Make sure partial page rendering is enabled
    Object mode = node.getAttributeValue(context, PARTIAL_RENDER_MODE_ATTR);
    if (PARTIAL_RENDER_MODE_SELF.equals(mode))
    {
      // If we don't have an ID, we don't have any targets
      if (id == null)
        return null;

      // We're all set... Use the node's ID as the partial target
      return new String[] { id.toString() };
    }

    return null;
  }

  /**
   * Returns a String value which can be used as the onclick handler for
   * an element which fires partial change events.
   *
   * @param destination The destination URL, which contains any
   *   event information, including the partialTargets parameter.
   */
  public static String getFirePartialChangeHandler(String destination)
  {
    return AutoSubmitUtils.getPartialGetScript(destination);
  }


  /**
   * Returns true if the agent supports transparent images.
   */
  public static boolean supportsTransparentImages(
    UIXRenderingContext context
    )
  {
    TrinidadAgent agent = context.getAgent();

    // =-=ags Temporarily disabling the use of transparent images
    // on ICE due to problems with tiling transparent images that
    // are affecting UIXVE.  This code should be removed once the
    // underlying ICE bug is fixed (supposedly ICE 5.4.1)
    if (TrinidadAgent.Application.ICE == agent.getAgentApplication())
      return false;

    //int encodings = ((Integer)agent.getCapability(
    //                                  AdfFacesAgent.CAP_IMAGE_ENCODINGS)).intValue();

    // If the Agent suports transparent PNG, we are good to go...
    //if ((encodings & AdfFacesAgent.IMAGE_ENCODINGS_CAP_TRANSPARENT_PNG) != 0)
    //  return true;

    if (agent.getCapability(TrinidadAgent.CAP_TRANSPARENT_PNG_TYPE_IMAGE) == Boolean.TRUE)
       return true;

    // Otherwise, check GIF suport...
    Configuration config = context.getConfiguration();

    //return (((encodings & AdfFacesAgent.IMAGE_ENCODINGS_CAP_GIF) != 0) &&
    //          !Boolean.FALSE.equals(config.getProperty(_GIF_ENABLED)));
    return ((agent.getCapability(TrinidadAgent.CAP_GIF_TYPE_IMAGE) == Boolean.TRUE) &&
               !Boolean.FALSE.equals(config.getProperty(_GIF_ENABLED)));

  }

  /**
   * Renders the specified Icon with the provided attributes.
   */
  public static void renderIcon(
    UIXRenderingContext context,
    Icon             icon,
    Object           shortDesc,
    Object           align
    ) throws IOException
  {
    renderIcon(context, icon, shortDesc, align, false);
  }

  /**
   * Renders the specified Icon with the provided attributes.
   */
  public static void renderIcon(
    UIXRenderingContext context,
    Icon             icon,
    Object           shortDesc,
    Object           align,
    boolean          embedded
    ) throws IOException
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    FacesContext fContext = context.getFacesContext();
    OutputUtils.renderIcon(fContext, arc, icon, shortDesc, align, embedded);
  }

  /**
   * Returns the valign vAlign value for aligning image icons
   * vertically with text on the same line.
   */
  public static Object getMiddleIconAlignment(UIXRenderingContext context)
  {
    // =-= AEW I haven't been able to find an image alignment
    // that works well for all browsers.  "absmiddle" looks
    // great in IE, but that's a nonstandard hack. "middle"
    // should work OK everywhere, but looks terrible in both
    // IE and Netscape (but OK in Mozilla) "top"'s OK in Netscape.
    // For now, "top" in Netscape, "absmiddle" everywhere else

    String align = null;
    TrinidadAgent.Application agentApplication = context.getAgent().getAgentApplication();

    if (agentApplication == TrinidadAgent.Application.NETSCAPE)
    {
      align = UIConstants.V_ALIGN_TOP;
    }
    else
    {
      // Previously we used "middle" for all other browsers except
      // for Safari, where "absmiddle" was required for reasonable
      // results.  However, as far as I can tell, for images which
      // are evenly padded on top/bottom, absmiddle also looks
      // best on IE and Mozilla.  So, let's use absmiddle for
      // these browsers too.
      // =-= MLL Update: to address Bug # 3426092, alignment has been set to
      // "middle" to comply with HTML 4.01 Transitional Spec.

      align = UIConstants.V_ALIGN_MIDDLE;
    }

    return align;
  }

  /**
   * Encodes a form value using the supplied transformed name for the
   * currently scoped form.
   */
  public static Object getFormEncodedValue(
    UIXRenderingContext context,
    Object           transName,
    Object           value)
  {
    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    FormEncoder formEncoder = context.getFormEncoder();
    return formEncoder.encodeFormValue(transName, value);
  }

  /**
   * Encodes a client parameter using the supplied transformed name for the
   * specified form.
   */
  public static String getFormEncodedParameter(
    FormEncoder      formEncoder,
    Object           formName,
    Object           transName,
    Object           value)
  {
    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    Object encoded =
      formEncoder.encodeClientParameter(formName, transName, value);
    return (encoded != null) ? encoded.toString() : null;
  }

  /**
   *  Return true if readOnly attribute exists and set to true
   */
  public static boolean isReadOnly(UIXRenderingContext context, UINode node)
  {
    Object readOnly = node.getAttributeValue(context, READ_ONLY_ATTR);
    return Boolean.TRUE.equals(readOnly);
  }
  
  /**
   * This method returns a script for submitting a PPR request.
   * @param formName the form name
   * @param validate indicates whether to validate the form during PPR
   * @param partialTargets the components to be refreshed during PPR
   * @param event represents the event created by the component requesting PPR
   * @param sourceParam id of the the component requesting PPR
   */
  public static String getPartialPageSubmitScript
                               (String formName,
                                String validate, 
                                String partialTargets, 
                                String event,
                                String sourceParam)
  {
    return "_submitPartialChange('" + formName + "'," + validate + ",{"+
                 PARTIAL_TARGETS_PARAM + ":'" + partialTargets + "'," +
                 EVENT_PARAM + ":'" + event + "'," +
                 SOURCE_PARAM + ":'" + sourceParam + "'});return false"; 
  }

  
  /**
   * This method returns a script for submitting the page.
   * @param formName the form name
   * @param validate indicates whether to validate the form
   * @param event represents the event created by the submitting component
   * @param sourceParam id of the the submitting component
   */
  public static String getFullPageSubmitScript
                               (String formName,
                                String validate, 
                                String event,
                                String sourceParam)
  {
    return "submitForm ('" + formName + "'," + validate + ",{"+
            EVENT_PARAM + ":'" + SHOW_EVENT + "'," +
            SOURCE_PARAM + ":'" + sourceParam + "'});return false";
  }
  
  // This utility method is used to strip /**/ style comments out of
  // JavaScript code.  We strip comments out of scripts that are
  // included in the partial page response, since we actually comment
  // out all of these scripts to prevent them from being executed in
  // the hidden iframe.  If no comments are found, returns the provided
  // buffer.
  // Note: This method is only called by BodyRenderer and
  // ScriptBufferingResponseWriter, so we're leaving in
  // package-private.
  static StringBuilder __stripJSComments(StringBuilder buffer)
  {
    // We avoid reallocating the buffer until we actually
    // find a comment.  Actually, we should never find any
    // comments in production code.  This method really shouldn't
    // be needed, but we do all of this work just to be extra safe.
    StringBuilder strippedBuffer = null;

    // We use a simple state machine to track whether or not
    // we are inside a comment or opening/closing a comment.
    int state = _STRIP_STATE_START;

    // The start index of the portion of the string to copy
    int startIndex = 0;

    // The total buffer length
    int length = buffer.length();

    for (int i = 0; i < length; i++)
    {
      char c = buffer.charAt(i);

      switch (state)
      {
        case _STRIP_STATE_START:
          // Check for the opening '/'
          if (c == '/')
            state = _STRIP_STATE_SLASH;
          break;

        case _STRIP_STATE_SLASH:
          // We've seen a potential comment opening '/'.  Check
          // to see if this is really the start of a comment.
          if (c == '*')
          {
            state = _STRIP_STATE_COMMENT;

            // Copy the contents up to the start of the
            // comment into the strippedBuffer.
            if (strippedBuffer == null)
              strippedBuffer = new StringBuilder(length);

            strippedBuffer.append(buffer.substring(startIndex, i - 1));
          }
          else
          {
            state = _STRIP_STATE_START;
          }
          break;

        case _STRIP_STATE_COMMENT:
          // We're inside a comment.  Just for a closing '*'.
          if (c == '*')
            state = _STRIP_STATE_STAR;
          break;

        case _STRIP_STATE_STAR:
          // We've seen a potential comment closing '*'.  Check
          // to see if this is really the end of the comment.
          if (c == '/')
          {
            state = _STRIP_STATE_START;
            startIndex = i + 1;
          }
          else
          {
            state = _STRIP_STATE_COMMENT;
          }
          break;
      }
    }

    // We should never end in any state other than start.  Anything
    // else would indicate an invalid script!
    assert (state == _STRIP_STATE_START);

    // Check for anything left in the pipeline
    if (strippedBuffer != null)
    {
      if (state == _STRIP_STATE_START)
        strippedBuffer.append(buffer.substring(startIndex, length));

      return strippedBuffer;
    }

    // If there were no comments, just return the original buffer.
    return buffer;
  }

  /**
   * Stores the current RenderingContext away so that it can be
   * accessed a RenderingContext that is wrapped by the current
   * RenderingContext.  This method is meant to serve a very
   * specific and limited purpose related to partial page rendering.
   * Our partial page rendering implementation needs to get the
   * ID of each UINode as each node is rendered in order to check
   * whether the node is a partial target.  This code lives in
   * a RenderingContext.pushRenderedChild() implementation in the
   * BodyRenderer$PartialRenderingContext class.  We are able to
   * get IDs within this method by calling UINode.getAttributeValue(),
   * but since we don't have access to the current RenderingContext,
   * we end up passing the PartialRenderingContext.  This works fine
   * in most cases, but fails for UINodes in tables where the client
   * is using RenderingContext.transformName() to generate unique
   * IDs.  The problem is that the client's BoundValue needs access
   * to the TableRenderingContext - not the PartialRenderingContext -
   * in order to generate a row-specific ID.  In order to support
   * this specific case, we stash away the TableRenderingContext
   * so that the PartialRenderingContext - can get at it and
   * pass it to client BoundValues.
   *
   * Note: This is a short-term solution only.  Clients should not
   * have to explicitly call transformName() to generate unique
   * IDs.  The real solution is to support automatic ID transformation -
   * and to add a generic mechanism for tracking the rendering traversal
   * which provides access to the current RenderingContext.
   */
  @SuppressWarnings("unchecked")
  static void __pushCurrentRenderingContext(
    UIXRenderingContext context
    )
  {
    assert (context != null);

    // We use a Stack to store the RenderingContext, but...
    //
    // - Do we really support nested tables?! (In detail disclosure maybe?)
    // - We really don't need synchronization!
    // - Initial stack capacity is 10 - larger than we need!
    //
    // But I don't think performance is going to be an issue -
    // so let's just make sure this code is safe...

    Stack<UIXRenderingContext> stack = 
      (Stack<UIXRenderingContext>)context.getProperty(MARLIN_NAMESPACE,
                                                      _CURRENT_RENDERING_CONTEXT_PROPERTY);
    if (stack == null)
    {
      stack = new Stack<UIXRenderingContext>();
      context.setProperty(MARLIN_NAMESPACE,
                          _CURRENT_RENDERING_CONTEXT_PROPERTY,
                          stack);
    }

    stack.push(context);
  }

  /**
   * Reverses the last call to __pushCurrentRenderingContext()
   * by popping the current RenderingContext off of the stack.
   */
  @SuppressWarnings("unchecked")
  static void __popCurrentRenderingContext(
    UIXRenderingContext context
    )
  {
    Stack<UIXRenderingContext> stack = 
      (Stack<UIXRenderingContext>)context.getProperty(MARLIN_NAMESPACE,
                                                      _CURRENT_RENDERING_CONTEXT_PROPERTY);

    // Null stack here is a programmer error...
    assert (stack != null);

    stack.pop();
  }

  /**
   * Retrieves the current RenderingContext as specified
   * by the last call to __pushCurrentRenderingContext().
   */
  @SuppressWarnings("unchecked")
  static UIXRenderingContext __getCurrentRenderingContext(
    UIXRenderingContext context
    )
  {
    Stack<UIXRenderingContext> stack = 
      (Stack<UIXRenderingContext>)context.getProperty(MARLIN_NAMESPACE,
                                                      _CURRENT_RENDERING_CONTEXT_PROPERTY);

    if ((stack == null) || (stack.empty()))
      return null;

    return stack.peek();
  }


  // maps fonts size names to font element size strings
  static private HashMap<String, String> _sSizeNameMap = 
    new HashMap<String, String>(13);

  //
  // mapping of size names to font element size strings
  //
  static private final String[] _FONT_SIZE_NAME_MAPPING = new String[]
  {
    "xx-small", "1",
    "x-small",  "2",
    "small",    "3",
    "medium",   "4",
    "large",    "5",
    "x-large",  "6",
    "xx-large", "7",
    "smaller",  "-1",
    "larger",   "+1",
  };


  //
  // Supportyed vertical align css values
  //
  static private final String[] _VALIGNS_SUPPORTED = new String[]
  {
    "baseline",
    "top",
    "center",
    "middle",
    "bottom",
  };


  //
  // mapping of point sizes to font element size attributes
  //
  private static final String[] _SIZE_MAPPING =  new String[]
  {
    "1", // 0 pt
    "1", // 1 pt
    "1", // 2 pt
    "1", // 3 pt
    "1", // 4 pt
    "1", // 5 pt
    "1", // 6 pt
    "1", // 7 pt
    "1", // 8 pt
    "2", // 9 pt
    "3", // 10 pt
    "4", // 11 pt
    "4", // 12 pt
    "4", // 13 pt
    "5", // 14 pt
    "5", // 15 pt
    "6", // 16 pt
    "6", // 17 pt
    "7", // 18 pt
  };

  /**
   * Pushes a value onto a stack on the RenderingContext, returning the
   * mask value to OR into teh result.
   */
  // =-=gc You'd think you could only bother pushing if when the value
  // is different, but that didn't work inside tables.  See bug 2944365
  private static int _pushStackProperty(
    Stack[] styleInfo,
    int     stackIndex,
    Object  value
    )
  {
    if (value != null)
    {
      Stack<Object> styleInfoStack = _getStyleInfoStack(styleInfo, stackIndex);

      // push new value
      styleInfoStack.push(value);

      // set flag bit
      return 1 << stackIndex;
    }

    // same value
    return 0;
  }

  /**
   * Returns the Stack for a Style info, creating it, if necessary.
   */
  @SuppressWarnings("unchecked")
  private static Stack<Object> _getStyleInfoStack(
    Stack[] styleInfo,
    int     stackIndex
    )
  {
    Stack<Object> styleInfoStack = styleInfo[stackIndex];

    if (styleInfoStack == null)
    {
      // create new stack
      styleInfoStack = new Stack<Object>();

      // push on initial default
      styleInfoStack.push(_STYLE_DEFAULTS[stackIndex]);

      // save away new stack
      styleInfo[stackIndex] = styleInfoStack;
    }

    return styleInfoStack;
  }


  /**
   * Returns the style info array of style Stacks, creating the object if
   * necessary.
   */
  private static Stack[] _getStyleInfo(
    UIXRenderingContext context
    )
  {
    XhtmlLafUtils.StyleInfo styleInfo = (StyleInfo)
       getRenderingProperty(context, _STYLE_INFO_PROPERTY);

    if (styleInfo == null)
    {
      styleInfo = new XhtmlLafUtils.StyleInfo();
      setRenderingProperty(context, _STYLE_INFO_PROPERTY, styleInfo);
    }

    return styleInfo.getValue();
  }

  //
  // MutableProperty implementation that wraps up the array-of-stacks
  //
  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  private final static class StyleInfo implements MutableProperty
  {
    public StyleInfo()
    {
      this(new Stack[_STYLE_INFO_STACK_COUNT]);
    }

    public StyleInfo(Stack[] value)
    {
      _value = value;
    }

    public Stack[] getValue()
    {
      return _value;
    }

    @Override
    public Object clone()
    {
      int length = _value.length;
      Stack[] newValue = new Stack[length];
      for (int i = 0; i < length; i++)
      {
        if (_value[i] != null)
        {
          newValue[i] = (Stack) _value[i].clone();
        }
      }

      return new XhtmlLafUtils.StyleInfo(newValue);
    }

    private final Stack[] _value;
  }


  // Constants used by __stripJSComments()
  private static final int _STRIP_STATE_START   = 0;  // Start state
  private static final int _STRIP_STATE_SLASH   = 1;  // Open '/' is seen
  private static final int _STRIP_STATE_COMMENT = 2;  // Inside comment
  private static final int _STRIP_STATE_STAR    = 3;  // Closing '*' is seen


  //
  // Keys for RenderingContext stack properties
  //
  private static final Object _STYLE_INFO_PROPERTY = new Object();

  private static final int _STACK_FLAGS_INDEX = 0;
  private static final int _FONT_FACE_INDEX = 1;
  private static final int _FONT_SIZE_INDEX = 2;
  private static final int _FOREGROUND_INDEX = 3;
  private static final int _ITALIC_INDEX = 4;
  private static final int _BOLD_INDEX = 5;

  private static final int _STYLE_INFO_STACK_COUNT = _BOLD_INDEX + 1;

  private static final String[] _STYLE_ELEMENTS =
  {
    null,   // flags (dummy)
    "font", // font face
    "font", // font size
    "font", // foreground
    "i",    // italic
    "b",    // bold
  };

  private static final Object[] _STYLE_DEFAULTS =
  {
    null,           // flags (dummy)
    "",             // font face (dummy)
    "3",            // font size
    "#000000",      // foreground
    Boolean.FALSE,  // italic
    Boolean.FALSE,  // bold
  };

  /** HashMap mapping css vertical-align to the valign attribute values */
  private static Map<String, String> _sSupportedVAligns = null;

  /**
   * Initialize the library information.
   */
  static
  {
    //
    // initialize mapping of font size names to font element sizes
    //
    for (int i = 0; i < _FONT_SIZE_NAME_MAPPING.length; i += 2)
    {
      _sSizeNameMap.put(_FONT_SIZE_NAME_MAPPING[i],
                       _FONT_SIZE_NAME_MAPPING[i+1]);
    }


    //
    // initialize the set of supported css vertical alignments
    //
    _sSupportedVAligns = new HashMap<String, String>(13);

    for (int i = 0; i < _VALIGNS_SUPPORTED.length; i++)
    {
      _sSupportedVAligns.put(_VALIGNS_SUPPORTED[i], _VALIGNS_SUPPORTED[i]);
    }
  }

  // Configuration property to test whether GIF support is enabled.
  private static final String _GIF_ENABLED = "gifEnabled";

  // Key for storing current RenderingContext
  private static final Object _CURRENT_RENDERING_CONTEXT_PROPERTY =
    new Object();

  // Key for storing the short style classes Dictionary
  private static final Object _STYLE_CLASSES_PROPERTY = new Object();

  // Value for indicating that we have a null short style classes Dictionary
  private static final Object _NULL_STYLE_CLASSES = new Object();

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(XhtmlLafUtils.class);
}
