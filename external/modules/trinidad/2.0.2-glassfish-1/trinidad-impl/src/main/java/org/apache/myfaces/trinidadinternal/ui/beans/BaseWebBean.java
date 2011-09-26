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
package org.apache.myfaces.trinidadinternal.ui.beans;

import org.apache.myfaces.trinidadinternal.ui.BaseMutableUINode;
import org.apache.myfaces.trinidadinternal.ui.MutableUINode;

/**
 * The BaseWebBean class is the root superclass for all UIX
 * WebBeans.  Subclasses of this method do not add any functionality,
 * only many, many convenience methods.  Clients that know the
 * namespace, localname, and names for attributes and named children
 * could, in fact, use nothing but instances of this class.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/beans/BaseWebBean.java#0 $) $Date: 10-nov-2005.18:57:39 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BaseWebBean extends BaseMutableUINode
{
  /**
   * Creates a BaseWebBean.
   */
  public BaseWebBean(
    String namespace,
    String localName)
  {
    super(namespace, localName);
  }

  /**
   * Sets the page-wide unique client ID of this node.  The string set
   * must comply with the
   * <a href="http://www.w3.org/TR/2000/WD-xml-2e-20000814#NT-TokenizedType">
   * XML id specification</a>--namely it must begin
   * with a [a-z][A-z] and after that can contain as many of
   * [a-z][A-Z][0-9][._-:] as desired.
   * <p>
   * This property is typically only needed when writing client-side
   * JavaScript.
   */
  @Override
  public final void setID(String newID)
  {
    // stub placeholder for when we get rid of bogus need for implementation
    // on BaseMutableUINode.
    setAttributeValue(ID_ATTR, newID);
  }

  /**
   * Returns the page-wide unique client ID of this node.  The string returned
   * must comply with the
   * <a href="http://www.w3.org/TR/2000/WD-xml-2e-20000814#NT-TokenizedType">
   * XML id specification</a>--namely it must begin
   * with a [a-z][A-z] and after that can contain as many of
   * [a-z][A-Z][0-9][._-:] as desired.
   * <p>
   * This property is typically only needed when writing client-side
   * JavaScript.
   */
  public final String getID()
  {
    return resolveString(getAttributeValue(ID_ATTR));

  }


  /**
   * Sets the CSS style class of the bean.
   */
  public final void setStyleClass(String newStyleClass)
  {
    setAttributeValue(STYLE_CLASS_ATTR, newStyleClass);
  }


  /**
   * Gets the CSS style class of the bean.
   */
  public final String getStyleClass()
  {
    return resolveString(getAttributeValue(STYLE_CLASS_ATTR));
  }


  /**
   * Sets the short description of the bean.  This text
   * is commonly used by user agents to display tooltip help
   * text.
   */
  public final void setShortDesc(String newShortDesc)
  {
    setAttributeValue(SHORT_DESC_ATTR, newShortDesc);
  }


  /**
   * Gets the short description of the bean.  This text
   * is commonly used by user agents to display tooltip help
   * text.
   */
  public final String getShortDesc()
  {
    return resolveString(getAttributeValue(SHORT_DESC_ATTR));
  }


  /**
   * Sets an inline CSS Style  on the bean.  See
   * <a href="http://www.w3.org/TR/html4/present/styles.html#h-14.2.2">
   * section 14.2.2 of the HTML specification</a> for more
   * information.
   */
  public final void setInlineStyle(String newInlineStyle)
  {
    setAttributeValue(INLINE_STYLE_ATTR, newInlineStyle);
  }


  /**
   * Gets the inline Style from the bean.
   */
  public final String getInlineStyle()
  {
    return resolveString(getAttributeValue(INLINE_STYLE_ATTR));
  }


  /**
   * Sets an onclick Javascript handler.
   */
  public final void setOnClick(String newOnClick)
  {
    setAttributeValue(ON_CLICK_ATTR, newOnClick);
  }


  /**
   * Gets an onclick Javascript handler.
   */
  public final String getOnClick()
  {
    return resolveString(getAttributeValue(ON_CLICK_ATTR));
  }


  /**
   * Sets an ondoubleclick Javascript handler.
   */
  public final void setOnDoubleClick(String newOnDoubleClick)
  {
    setAttributeValue(ON_DOUBLE_CLICK_ATTR, newOnDoubleClick);
  }


  /**
   * Gets an ondoubleclick Javascript handler.
   */
  public final String getOnDoubleClick()
  {
    return resolveString(getAttributeValue(ON_DOUBLE_CLICK_ATTR));
  }


  /**
   * Sets an onmousedown Javascript handler.
   */
  public final void setOnMouseDown(String newOnMouseDown)
  {
    setAttributeValue(ON_MOUSE_DOWN_ATTR, newOnMouseDown);
  }


  /**
   * Gets an onmousedown Javascript handler.
   */
  public final String getOnMouseDown()
  {
    return resolveString(getAttributeValue(ON_MOUSE_DOWN_ATTR));
  }


  /**
   * Sets an onmouseup Javascript handler.
   */
  public final void setOnMouseUp(String newOnMouseUp)
  {
    setAttributeValue(ON_MOUSE_UP_ATTR, newOnMouseUp);
  }


  /**
   * Gets an onmouseup Javascript handler.
   */
  public final String getOnMouseUp()
  {
    return resolveString(getAttributeValue(ON_MOUSE_UP_ATTR));
  }


  /**
   * Sets an onmouseover Javascript handler.
   */
  public final void setOnMouseOver(String newOnMouseOver)
  {
    setAttributeValue(ON_MOUSE_OVER_ATTR, newOnMouseOver);
  }


  /**
   * Gets an onmouseover Javascript handler.
   */
  public final String getOnMouseOver()
  {
    return resolveString(getAttributeValue(ON_MOUSE_OVER_ATTR));
  }


  /**
   * Sets an onmousemove Javascript handler.
   */
  public final void setOnMouseMove(String newOnMouseMove)
  {
    setAttributeValue(ON_MOUSE_MOVE_ATTR, newOnMouseMove);
  }


  /**
   * Gets an onmousemove Javascript handler.
   */
  public final String getOnMouseMove()
  {
    return resolveString(getAttributeValue(ON_MOUSE_MOVE_ATTR));
  }


  /**
   * Sets an onmouseout Javascript handler.
   */
  public final void setOnMouseOut(String newOnMouseOut)
  {
    setAttributeValue(ON_MOUSE_OUT_ATTR, newOnMouseOut);
  }


  /**
   * Gets an onmouseout Javascript handler.
   */
  public final String getOnMouseOut()
  {
    return resolveString(getAttributeValue(ON_MOUSE_OUT_ATTR));
  }


  /**
   * Sets an onkeypress Javascript handler.
   */
  public final void setOnKeyPress(String newOnKeyPress)
  {
    setAttributeValue(ON_KEY_PRESS_ATTR, newOnKeyPress);
  }


  /**
   * Gets an onkeypress Javascript handler.
   */
  public final String getOnKeyPress()
  {
    return resolveString(getAttributeValue(ON_KEY_PRESS_ATTR));
  }


  /**
   * Sets an onkeydown Javascript handler.
   */
  public final void setOnKeyDown(String newOnKeyDown)
  {
    setAttributeValue(ON_KEY_DOWN_ATTR, newOnKeyDown);
  }


  /**
   * Gets an onkeydown Javascript handler.
   */
  public final String getOnKeyDown()
  {
    return resolveString(getAttributeValue(ON_KEY_DOWN_ATTR));
  }


  /**
   * Sets an onkeyup Javascript handler.
   */
  public final void setOnKeyUp(String newOnKeyUp)
  {
    setAttributeValue(ON_KEY_UP_ATTR, newOnKeyUp);
  }


  /**
   * Gets an onkeyup Javascript handler.
   */
  public final String getOnKeyUp()
  {
    return resolveString(getAttributeValue(ON_KEY_UP_ATTR));
  }


  /**
   * Sets whether the bean is rendered.  When set to false,
   * no output will be delivered for this bean.
   */
  @Override
  public void setRendered(
    boolean rendered
    )
  {
    // Stub for backwards compatibility
    super.setRendered(rendered);
  }


  /**
   * Sets whether the bean is rendered.  When set to false,
   * no output will be delivered for this bean.
   */
  @Override
  public boolean isRendered()
  {
    // Stub for backwards compatibility
    return super.isRendered();
  }


  /**
   * Sets whether the bean is rendered.  When set to false,
   * no output will be delivered for this bean.
   */
  public static void setRendered(MutableUINode node, boolean rendered)
  {
    node.setAttributeValue(RENDERED_ATTR,
                      Boolean.valueOf(rendered));
  }


  /**
   * Sets whether the bean is rendered.  When set to false,
   * no output will be delivered for this bean.
   */
  public static boolean isRendered(MutableUINode node)
  {
    return resolveBoolean(node.getAttributeValue(null,RENDERED_ATTR),
                          true);
  }


  /**
   * Sets the language of the bean.  Language is specified
   * by the same format used in HTML and defined by
   * <a href="http://www.ietf.org/rfc/rfc1766.txt">RFC1766</a>,
   * e.g., <code>en</code>, <code>fr-CA</code>, etc.
   */
  public static void setLanguage(MutableUINode node, String newLanguage)
  {
    node.setAttributeValue(LANGUAGE_ATTR, newLanguage);
  }


  /**
   * Gets the language of the bean.
   */
  public static String getLanguage(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, STYLE_CLASS_ATTR));
  }


  /**
   * Sets the reading direction of the bean.  Allowable
   * values are <code>rtl</code> and <code>ltr</code>.  See
   * <a href="http://www.w3.org/TR/html4/struct/dirlang.html#h-8.2">
   * section 8.2 of the HTML specification</a>
   * for more information on how this attribute should be processed.
   */
  public static void setDirection(MutableUINode node, String newDirection)
  {
    node.setAttributeValue(DIRECTION_ATTR, newDirection);
  }


  /**
   * Gets the reading direction of the bean.
   */
  public static String getDirection(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, DIRECTION_ATTR));
  }

  /**
   * Sets the page-wide unique client ID of this node.  The string set
   * must comply with the
   * <a href="http://www.w3.org/TR/2000/WD-xml-2e-20000814#NT-TokenizedType">
   * XML id specification</a>--namely it must begin
   * with a [a-z][A-z] and after that can contain as many of
   * [a-z][A-Z][0-9][._-:] as desired.
   * <p>
   * This property is typically only needed when writing client-side
   * JavaScript.
   */
  public static void setID(MutableUINode node, String newID)
  {
    node.setAttributeValue(ID_ATTR, newID);
  }

  /**
   * Returns the page-wide unique client ID of this node.  The string returned
   * must comply with the
   * <a href="http://www.w3.org/TR/2000/WD-xml-2e-20000814#NT-TokenizedType">
   * XML id specification</a>--namely it must begin
   * with a [a-z][A-z] and after that can contain as many of
   * [a-z][A-Z][0-9][._-:] as desired.
   * <p>
   * This property is typically only needed when writing client-side
   * JavaScript.
   */
  public static String getID(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ID_ATTR));
  }


  /**
   * Sets the CSS style class of the bean.
   */
  public static void setStyleClass(MutableUINode node, String newStyleClass)
  {
    node.setAttributeValue(STYLE_CLASS_ATTR, newStyleClass);
  }


  /**
   * Gets the CSS style class of the bean.
   */
  public static String getStyleClass(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, STYLE_CLASS_ATTR));
  }


  /**
   * Sets the short description of the bean.  This text
   * is commonly used by user agents to display tooltip help
   * text.
   */
  public static void setShortDesc(MutableUINode node, String newShortDesc)
  {
    node.setAttributeValue(SHORT_DESC_ATTR, newShortDesc);
  }


  /**
   * Gets the short description of the bean.  This text
   * is commonly used by user agents to display tooltip help
   * text.
   */
  public static String getShortDesc(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, SHORT_DESC_ATTR));
  }


  /**
   * Sets an onclick Javascript handler.
   */
  public static void setOnClick(MutableUINode node, String newOnClick)
  {
    node.setAttributeValue(ON_CLICK_ATTR, newOnClick);
  }


  /**
   * Gets an onclick Javascript handler.
   */
  public static String getOnClick(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_CLICK_ATTR));
  }


  /**
   * Sets an ondoubleclick Javascript handler.
   */
  public static void setOnDoubleClick(MutableUINode node, String newOnDoubleClick)
  {
    node.setAttributeValue(ON_DOUBLE_CLICK_ATTR, newOnDoubleClick);
  }


  /**
   * Gets an ondoubleclick Javascript handler.
   */
  public static String getOnDoubleClick(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_DOUBLE_CLICK_ATTR));
  }


  /**
   * Sets an onmousedown Javascript handler.
   */
  public static void setOnMouseDown(MutableUINode node, String newOnMouseDown)
  {
    node.setAttributeValue(ON_MOUSE_DOWN_ATTR, newOnMouseDown);
  }


  /**
   * Gets an onmousedown Javascript handler.
   */
  public static String getOnMouseDown(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_MOUSE_DOWN_ATTR));
  }


  /**
   * Sets an onmouseup Javascript handler.
   */
  public static void setOnMouseUp(MutableUINode node, String newOnMouseUp)
  {
    node.setAttributeValue(ON_MOUSE_UP_ATTR, newOnMouseUp);
  }


  /**
   * Gets an onmouseup Javascript handler.
   */
  public static String getOnMouseUp(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_MOUSE_UP_ATTR));
  }


  /**
   * Sets an onmouseover Javascript handler.
   */
  public static void setOnMouseOver(MutableUINode node, String newOnMouseOver)
  {
    node.setAttributeValue(ON_MOUSE_OVER_ATTR, newOnMouseOver);
  }


  /**
   * Gets an onmouseover Javascript handler.
   */
  public static String getOnMouseOver(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_MOUSE_OVER_ATTR));
  }


  /**
   * Sets an onmousemove Javascript handler.
   */
  public static void setOnMouseMove(MutableUINode node, String newOnMouseMove)
  {
    node.setAttributeValue(ON_MOUSE_MOVE_ATTR, newOnMouseMove);
  }


  /**
   * Gets an onmousemove Javascript handler.
   */
  public static String getOnMouseMove(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_MOUSE_MOVE_ATTR));
  }


  /**
   * Sets an onmouseout Javascript handler.
   */
  public static void setOnMouseOut(MutableUINode node, String newOnMouseOut)
  {
    node.setAttributeValue(ON_MOUSE_OUT_ATTR, newOnMouseOut);
  }


  /**
   * Gets an onmouseout Javascript handler.
   */
  public static String getOnMouseOut(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_MOUSE_OUT_ATTR));
  }


  /**
   * Sets an onkeypress Javascript handler.
   */
  public static void setOnKeyPress(MutableUINode node, String newOnKeyPress)
  {
    node.setAttributeValue(ON_KEY_PRESS_ATTR, newOnKeyPress);
  }


  /**
   * Gets an onkeypress Javascript handler.
   */
  public static String getOnKeyPress(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_KEY_PRESS_ATTR));
  }


  /**
   * Sets an onkeydown Javascript handler.
   */
  public static void setOnKeyDown(MutableUINode node, String newOnKeyDown)
  {
    node.setAttributeValue(ON_KEY_DOWN_ATTR, newOnKeyDown);
  }


  /**
   * Gets an onkeydown Javascript handler.
   */
  public static String getOnKeyDown(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_KEY_DOWN_ATTR));
  }


  /**
   * Sets an onkeyup Javascript handler.
   */
  public static void setOnKeyUp(MutableUINode node, String newOnKeyUp)
  {
    node.setAttributeValue(ON_KEY_UP_ATTR, newOnKeyUp);
  }


  /**
   * Gets an onkeyup Javascript handler.
   */
  public static String getOnKeyUp(MutableUINode node)
  {
    return resolveString(node.getAttributeValue(null, ON_KEY_UP_ATTR));
  }


  /**
   * Utility method for rendered code that transforms Boolean->boolean.
   */
  public static Object resolveObject(
    Object value,
    Object defaultValue
    )
  {
    return (value != null)
             ? value
             : defaultValue;
  }

  /**
   * Utility method for rendered code that transforms Boolean->boolean.
   * @deprecated in favor of the version taking an Object
   */
  @Deprecated
  public static boolean resolveBoolean(
    Boolean value,
    boolean defaultValue
    )
  {
    return (value != null)
             ? value.booleanValue()
             : defaultValue;
  }

  /**
   * Utility method for rendered code that transforms Boolean->boolean.
   * @deprecated in favor of the version taking an Object
   */
  @Deprecated
  public static boolean resolveBoolean(
    Boolean value
    )
  {
    return resolveBoolean(value, false);
  }


  /**
   * Utility method for rendered code that transforms Object->boolean.
   */
  public static boolean resolveBoolean(
    Object  value,
    boolean defaultValue
    )
  {
    if (defaultValue)
      return !Boolean.FALSE.equals(value);
    else
      return Boolean.TRUE.equals(value);
  }

  /**
   * Utility method for rendered code that transforms Object->boolean.
   */
  public static boolean resolveBoolean(
    Object value
    )
  {
    return Boolean.TRUE.equals(value);
  }

  /**
   * Utility method for rendered code that transforms Integer->int.
   * @deprecated in favor of the version taking an Object
   */
  @Deprecated
  public static int resolveInteger(
    Integer value
    )
  {
    return resolveInteger(value, 0);
  }

  /**
   * Utility method for rendered code that transforms Integer->int.
   * @deprecated in favor of the version taking an Object
   */
  @Deprecated
  public static int resolveInteger(
    Integer value,
    int     defaultValue
    )
  {
    return (value != null)
             ? value.intValue()
             : defaultValue;
  }


  /**
   * Utility method for rendered code that transforms Number->int.
   */
  public static int resolveInteger(
    Object value
    )
  {
    return resolveInteger(value, 0);
  }

  /**
   * Utility method for rendered code that transforms Number->int.
   */
  public static int resolveInteger(
    Object value,
    int     defaultValue
    )
  {
    return (value != null)
             ? ((Number)value).intValue()
             : defaultValue;
  }


  /**
   * Utility method for rendered code that transforms Long->long.
   * @deprecated in favor of the version taking an Object
   */
  @Deprecated
  public static long resolveLong(
    Long value
    )
  {
    return resolveLong(value, 0);
  }

  /**
   * Utility method for rendered code that transforms Long->long.
   * @deprecated in favor of the version taking an Object
   */
  @Deprecated
  public static long resolveLong(
    Long  value,
    long  defaultValue
    )
  {
    return (value != null)
             ? value.longValue()
             : defaultValue;
  }


  /**
   * Utility method for rendered code that transforms Number->long.
   */
  public static long resolveLong(
    Object value
    )
  {
    return resolveLong(value, 0);
  }

  /**
   * Utility method for rendered code that transforms Number->long.
   */
  public static long resolveLong(
    Object value,
    long   defaultValue
    )
  {
    return (value != null)
             ? ((Number)value).longValue()
             : defaultValue;
  }


  /**
   * Utility method for rendered code that transforms Character->character.
   */
  public static char resolveCharacter(
    Character value
    )
  {
    return resolveCharacter(value, '\u0000');
  }

  /**
   * Utility method for rendered code that transforms Long->long.
   */
  public static char resolveCharacter(
    Character  value,
    char       defaultValue
    )
  {
    return (value != null)
             ? value.charValue()
             : defaultValue;
  }


  /**
   * Utility method for rendered code that transforms Object->String.
   */
  public static String resolveString(
    Object  value
    )
  {
    return (value != null)
             ? value.toString()
             : null;
  }


  /**
   * Utility method for rendered code that transforms Object->String.
   */
  public static String resolveString(
    Object  value,
    String  defaultValue
    )
  {
    return (value != null)
             ? value.toString()
             : defaultValue;
  }
}
