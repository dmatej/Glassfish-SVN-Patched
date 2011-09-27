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

import javax.el.ValueExpression;
import javax.faces.component.UIComponent;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.FacesBeanImpl;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

final class ConverterUtils 
{
  private ConverterUtils()
  {
  }
  
  static Object getComponentLabel(UIComponent component)
  { 
      Object label = component.getAttributes().get("label");
      if ( null == label)
       label = component.getValueBinding("label");
       
      return label;
  }
  
  static boolean equals(Object o1, Object o2)
  {
    return ( o1 == o2 || (o1 != null && o1.equals(o2)));
  }
  
  static FacesBean getFacesBean(final FacesBean.Type type)
 {
   FacesBeanImpl bean = new FacesBeanImpl()
                           {
                             @Override
                             public FacesBean.Type getType()
                             {
                               return type;
                             }
                           };
   return bean;
 } 
 
  static void setValueExpression(FacesBean bean, String name, ValueExpression expression)
  {   
    PropertyKey key = _getPropertyKey(bean, name, true);
    bean.setValueExpression(key, expression);
  }
 
  static ValueExpression getValueExpression(FacesBean bean, String name)
  {
    PropertyKey key = _getPropertyKey(bean, name, true);
    return bean.getValueExpression(key);
  }


  static void setValueBinding(FacesBean bean, String name, ValueBinding binding)
  {   
    PropertyKey key = _getPropertyKey(bean, name, true);
    bean.setValueBinding(key, binding);
  }
 
  static ValueBinding getValueBinding(FacesBean bean, String name)
  {
    PropertyKey key = _getPropertyKey(bean, name, true);
    return bean.getValueBinding(key);
  }
  
  private static PropertyKey _getPropertyKey(
    FacesBean bean, 
    String name,  
    boolean isStrict)
  {   
    _assertNotNull(name, "attribute cannot be null");
    FacesBean.Type type = bean.getType();
    PropertyKey key = type.findKey(name);
    if (isStrict && key == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "INVALID_ATTRIBUTE_NAME", name));
    else 
     return key;
  }
  
  private static void _assertNotNull(Object object, String message)
  {
    if (object == null)
    {
       if (message == null)
         throw new NullPointerException();
       else 
         throw new NullPointerException(message);
    }
  }
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ConverterUtils.class);
}
