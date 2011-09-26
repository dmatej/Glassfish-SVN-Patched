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

import org.xml.sax.Attributes;

/**
 * Class for defining a bean to the parser engine.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/beans/BeanDef.java#0 $) $Date: 10-nov-2005.18:59:18 $
 */
public abstract class BeanDef
{
  /**
   * Creates a BeanDef.
   */
  public BeanDef()
  {
  }

  /**
   * Gets an property definition.
   *
   * @param  name  the property name
   * @return the property definition, or null if no such property exists
   */
  abstract public PropertyDef getPropertyDef(String name);


  /**
   * Gets an property definition for a namespaced property.
   *
   * @param  namespace the namespace
   * @param  name  the property name
   * @return the property definition, or null if no such property exists
   */
  abstract public PropertyDef getPropertyDef(String namespace, String name);



  /**
   * Gets an property definition for a child element.
   *
   * @param  namespace the namespace
   * @param  name  the property name
   * @param  attrs the XML attributes
   * 
   * @return the property definition, or null if no such property exists
   */
  abstract public PropertyDef getElementPropertyDef(
    String namespace, String name, Attributes attrs);



  /**
   * Gets the "default" property definition.
   * @return the default property definition, or null if no such
   *         property exists
   */
  abstract public PropertyDef getDefaultPropertyDef();


  /**
   * Returns true if the PropertyDef is defined with an inline child element.
   */
  abstract public boolean isInlineChildProperty(
    String namespace, String name, PropertyDef def);

  /**
   * Creates a bean, specifying the namespace URI and localName
   *
   * @param  namespaceURI  the namespace URI of the bean
   * @param  localName     the local name of the bean
   * @return the new bean instance
   */
  abstract public Object createBean(
    String    namespaceURI,
    String    localName) throws ClassNotFoundException,
                                InstantiationException, 
                                IllegalAccessException;


  /**
   * "Finishes" a bean by transforming it into the final
   * target object.  This will be called when the
   * @param bean the bean created by createBean()
   * @return the object that should be returned by the parser
   */
  abstract public Object finishBean(Object bean);
}
