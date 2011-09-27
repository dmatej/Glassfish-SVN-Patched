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
package org.apache.myfaces.trinidadinternal.share.xml.beans;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.xml.sax.Attributes;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;


/**
 * The node parser for UIX Beans.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/beans/BeanParser.java#0 $) $Date: 10-nov-2005.18:59:18 $
 */
public class BeanParser extends BaseNodeParser
{
  /**
   * Creates a BeanParser based on a bean definition.
   * @param beanDef the bean definition
   */
  public BeanParser(
    BeanDef  beanDef)
  {
    if (beanDef == null)
      throw new NullPointerException();

    _beanDef = beanDef;
  }

  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs)
  {
    try
    {
      Object bean = _beanDef.createBean(namespaceURI, localName);

      int length = attrs.getLength();
      if (length > 0)
      {
        boolean otherNamespaces = _parseAttributes(context, bean, attrs, true);
        if (otherNamespaces)
          _parseAttributes(context, bean, attrs, false);
      }

      _namespaceURI = namespaceURI;
      _bean = bean;

      // Default properties:  act as if the envelope element
      // for the property is inserted around all the child elements
      PropertyDef defaultPropertyDef = _beanDef.getDefaultPropertyDef();
      if (defaultPropertyDef != null)
      {
        // "String" default properties simply mean that
        // any text content will get set as that string - _not_ that
        // there is a bonus envelope element
        if (defaultPropertyDef.getPropertyType() != String.class)
        {
          _currentPropDef = defaultPropertyDef;
          startEnvelopeChildProperty(context,
                                     namespaceURI,
                                     defaultPropertyDef.getName(),
                                     null,
                                     defaultPropertyDef);
        }
      }
    }
    catch (Exception e)
    {
      if (_LOG.isWarning())
        _LOG.warning(e);
    }
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs)
  {
    if (_bean == null)
      return null;

    // Inside an envelope property: we know what type we're looking for
    if (_currentPropDef != null)
    {
      if (_currentPropDefUsed)
      {
        _LOG.warning("ONLY_ONE_CHILD_ELEMENT_ALLOWED");
        return null;
      }

      // An array - get a parser for the component type of the array
      if (_currentPropDefIsArray)
      {
        Class<?> cls = _currentPropDef.getPropertyType().getComponentType();
        return context.getParser(cls,
                                 namespaceURI,
                                 localName);
      }
      // Not an array - get a parser for the type
      else
      {
        // Mark the property definition as "used" - only one child
        // element is allowed
        _currentPropDefUsed = true;
        return context.getParser(_currentPropDef.getPropertyType(),
                                 namespaceURI,
                                 localName);
      }
    }

    PropertyDef def = null;

    // Same namespace as the element itself: see if it matches up
    // with a standard (not-namespaced) property definition
    if (namespaceURI == _namespaceURI)
      def = _beanDef.getPropertyDef(localName);

    // OK, that didn't work - try a PropertyDef specific to the element.
    if (def == null)
      def = _beanDef.getElementPropertyDef(namespaceURI,
                                           localName,
                                           attrs);
    if (def != null)
      return startChildProperty(context,
                                 namespaceURI,
                                 localName,
                                 attrs,
                                 def);

    return null;
  }

  @Override
  public void endChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName)
  {
    // Finishing up an array
    if (_currentPropDefIsArray)
    {
      assert ((_currentPropDef != null) && (_currentArray != null));

      // Extract the list into an array
      Object[] array = _getArray(_currentPropDef, _currentArray);
      // And set the array on the property definition
      _currentPropDef.setValue(context, _bean, array);

      // GC the array early
      _currentArray = null;
    }

    // But always clear the property definition
    _currentPropDef = null;
  }

  @Override
  public void addText(
    ParseContext context,
    char[]       text,
    int          start,
    int          length)
  {
    // We support text in only the following case:
    //   (1) There is a default property definition
    //   (2) That default property definition is a String
    PropertyDef defaultPropertyDef = _beanDef.getDefaultPropertyDef();
    if ((defaultPropertyDef != null) &&
        (defaultPropertyDef.getPropertyType() == String.class))
    {
      String s = new String(text, start, length);
      if (_defaultPropertyText == null)
        _defaultPropertyText = s;
      else
        _defaultPropertyText = _defaultPropertyText + s;
    }
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child)
  {
    if (child == null)
    {
      if (_currentPropDefInline)
        _currentPropDef = null;

      return;
    }

    assert ((_currentPropDef != null) && (_bean != null));

    // For arrays, just add the child
    if (_currentPropDefIsArray)
    {
      // Inline arrays - add the child to the correct list
      if (_currentPropDefInline)
      {
        _inlineArrays.get(_currentPropDef).add(child);
        _currentPropDef = null;
      }
      else
      {
        _currentArray.add(child);
      }
    }
    // For non-arrays, set the value and we're done with this property
    else
    {
      _currentPropDef.setValue(context, _bean, child);

      // Inline properties won't get an "endChildElement()" call,
      // so clean up immediately
      if (_currentPropDefInline)
        _currentPropDef = null;
    }
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName)
  {
    if (_bean == null)
      return null;

    // For default properties, close the virtual element
    PropertyDef defaultPropertyDef = _beanDef.getDefaultPropertyDef();
    if (defaultPropertyDef != null)
    {
      // See above for the meaning of default properties for string
      // values
      if (defaultPropertyDef.getPropertyType() != String.class)
      {
        endChildElement(context, namespaceURI, defaultPropertyDef.getName());
      }
      else
      {
        if (_defaultPropertyText != null)
        {
          defaultPropertyDef.setValue(context, _bean, _defaultPropertyText);
        }
      }
    }

    // If we have any inline arrays, set those
    if (_inlineArrays != null)
    {
      for(Map.Entry<PropertyDef, List<Object>> entry : _inlineArrays.entrySet())
      {
        PropertyDef def = entry.getKey();
        Object[] array = _getArray(def, entry.getValue());
        def.setValue(context, _bean, array);
      }
    }

    return _beanDef.finishBean(_bean);
  }


  /**
   * Called when an XML element is being parsed as a bean property.
   * @see #startInlineChildProperty
   * @see #startEnvelopeChildProperty
   * @param context the parsing context
   * @param namespaceURI the namespace of the element
   * @param localName the local name of the element
   * @param attrs the attributes attached to the element
   * @param def the property definition
   */
  final protected NodeParser startChildProperty(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs,
    PropertyDef def)
  {
    if (isInlineChildProperty(context, namespaceURI, localName, def))
    {
      return startInlineChildProperty(context, namespaceURI, localName,
                                       attrs, def);
    }
    else
    {
      return startEnvelopeChildProperty(context, namespaceURI, localName,
                                         attrs, def);
    }
  }


  /**
   * Called to parse an XML element an inline bean property.
   * Inline child properties do not use an envelope element - they
   * are parsed directly off of the current element.
   *
   * @param context the parsing context
   * @param namespaceURI the namespace of the element
   * @param localName the local name of the element
   * @param attrs the attributes attached to the element
   * @param def the property definition
   */
  protected NodeParser startInlineChildProperty(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs,
    PropertyDef def)
  {
    Class<?> cls = def.getPropertyType();
    boolean isArray = cls.isArray();
    if (isArray)
      cls = cls.getComponentType();

    NodeParser parser = context.getParser(cls,
                                          namespaceURI,
                                          localName);
    if (parser != null)
    {
      _currentPropDef = def;
      _currentPropDefUsed = true;
      _currentPropDefInline = true;

      _currentPropDefIsArray = isArray;

      // Inline arrays - we store a map of each inline array,
      // and buffer up one ArrayList for each definition.
      if (isArray)
      {
        if (_inlineArrays == null)
        {
          _inlineArrays = new HashMap<PropertyDef, List<Object>>(3);
        }

        // =-=AEW  This assumes that either BeanDef is returning
        // the same PropertyDef each time, or PropertyDef implements
        // equals() correctly.
        if (_inlineArrays.get(def) == null)
        {
          _inlineArrays.put(def, new ArrayList<Object>());
        }
      }
    }

    return parser;
  }



  /**
   * Called to parse an XML element an inline bean property.
   * Envelope child properties use an envelope element that
   * names the property, but does not define its value.  Instead,
   * the envelope contains one further child XML element that
   * defines the value.  Most types are parsed in this fashion.
   *
   * @param context the parsing context
   * @param namespaceURI the namespace of the element
   * @param localName the local name of the element
   * @param attrs the attributes attached to the element
   * @param def the property definition
   */
  protected NodeParser startEnvelopeChildProperty(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs,
    PropertyDef def)
  {
    _currentPropDef = def;
    _currentPropDefUsed = false;
    _currentPropDefInline = false;

    // Detect when we're starting an array
    _currentPropDefIsArray = def.getPropertyType().isArray();
    if (_currentPropDefIsArray)
    {
      _currentArray = new ArrayList<Object>();
    }

    return this;
  }


  /**
   * Returns whether a given property definition
   * is parsed inline.  By default, this version always
   * returns false.
   * <p>
   * @param context the parsing context
   * @param def the property definition
   */
  protected boolean isInlineChildProperty(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    PropertyDef  def)
  {
    return _beanDef.isInlineChildProperty(namespaceURI, localName, def);
  }


  /**
   * An accessor function for retrieving the bean defintion.
   */
  final protected BeanDef getBeanDef()
  {
    return _beanDef;
  }


  /**
   * An accessor function for retrieving the bean currently being built.
   */
  final protected Object getBean()
  {
    return _bean;
  }



  //
  // Parse the XML attributes.  Called first to parse
  // the default namespace, then again to try all other
  // namespaces.
  //
  private boolean _parseAttributes(
    ParseContext context,
    Object       bean,
    Attributes   attrs,
    boolean      defaultNamespace)
  {
    boolean otherNamespaces = false;

    int length = attrs.getLength();
    for (int i = 0; i < length; i++)
    {
      String attrNamespace = attrs.getURI(i);
      // Check if the namespace is correct for this pass
      if ("".equals(attrNamespace) ^ !defaultNamespace)
      {
        String attrLocalName = attrs.getLocalName(i);

        // Get the property
        PropertyDef propertyDef;
        if (defaultNamespace)
          propertyDef = _beanDef.getPropertyDef(attrLocalName);
        else
          propertyDef = _beanDef.getPropertyDef(attrNamespace,
                                                  attrLocalName);

        // If we find an property def, parse the text and
        // set the value.
        if (propertyDef != null)
        {
          String valueText = attrs.getValue(i);
          try
          {
            Object value = propertyDef.parseText(context, attrNamespace,
                                                 valueText);
            propertyDef.setValue(context, bean, value);
          }
          catch (IllegalArgumentException iae)
          {
            if (_LOG.isWarning())
            {
              if (defaultNamespace)
              {
                _LOG.warning("CANNOT_PARSE_ATTRIBUTE_VALUE", attrLocalName);
                _LOG.warning(iae);
              }
              else
              {
                _LOG.warning("CANNOT_PARSE_ATTRIBUTE_VALUE_NAMESPACE", new Object[] {attrLocalName, attrNamespace});
                _LOG.warning(iae);
              }
            }
          }
        }
        else
        {
          logUnknownAttribute(context, attrNamespace, attrLocalName);
        }
      }
      else
      {
        otherNamespaces = true;
      }
    }

    return otherNamespaces;
  }


  /**
   * Records that an attribute was unknown.
   * @param context the parsing context
   * @param namespaceURI the namespace of the attribute, or an
   *   empty string for no namespace
   * @param localName the local name of the attribute
   */
  protected void logUnknownAttribute(
    ParseContext context,
    String       namespaceURI,
    String       localName)
  {
    if (_LOG.isWarning())
    {
      if ("".equals(namespaceURI))
        _LOG.warning("UNKNOWN_ATTRIBUTE", localName);
      else
        _LOG.warning("UNKNOWN_ATTRIBUTE_NAMESPACE", new Object[]{localName, namespaceURI});
    }
  }


  //
  // Convert an ArrayList into an array of the correct Java type
  //
  static private Object[] _getArray(
    PropertyDef def,
    List<Object> list)
  {
    Object[] array = (Object[])
      Array.newInstance(def.getPropertyType().getComponentType(),
                        list.size());

    list.toArray(array);
    return array;
  }

  // The bean definition
  private final BeanDef      _beanDef;

  // The bean being built
  private       Object       _bean;

  // The namespace of the top-level element
  private       String       _namespaceURI;

  // The current property definition being worked on
  private       PropertyDef  _currentPropDef;

  // If true, then the current property has already been set,
  // and additional elements should be treated as errors
  private       boolean      _currentPropDefUsed;

  // If true, the current property is an inline element (that is,
  // one without an envelope wrapper)
  private       boolean      _currentPropDefInline;

  // If true, the current property is actually an array
  // that is being buffered up
  private       boolean      _currentPropDefIsArray;

  // The list of the current property definition - used
  // only for "envelope" elements
  private       List<Object> _currentArray;

  // A map of all the "inline" array properties
  private       Map<PropertyDef, List<Object>> _inlineArrays;

  // Text being accumulated for the default property
  private       String       _defaultPropertyText;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(BeanParser.class);
}
