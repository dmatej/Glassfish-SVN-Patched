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
package org.apache.myfaces.trinidadinternal.renderkit;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

import javax.el.ELResolver;
import javax.el.ExpressionFactory;
import javax.el.ELContext;
import javax.el.MapELResolver;
import javax.el.MethodExpression;
import javax.el.MethodInfo;
import javax.el.ValueExpression;

import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.application.NavigationHandler;
import javax.faces.application.ProjectStage;
import javax.faces.application.StateManager;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.el.MethodBinding;
import javax.faces.el.PropertyResolver;
import javax.faces.el.ValueBinding;
import javax.faces.el.VariableResolver;
import javax.faces.event.ActionListener;
import javax.faces.event.SystemEvent;
import javax.faces.validator.Validator;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.bean.util.StateUtils;

public class MApplication extends Application 
{
  static public MApplication sharedInstance()
  {
    return _INSTANCE;
  }
  
  private MApplication()
  {
  }
  
  @Override
  public ProjectStage getProjectStage()
  {
    return ProjectStage.UnitTest;
  }

  @Override
  public void publishEvent(FacesContext context,
                           Class<? extends SystemEvent> systemEventClass,
                           Class<?> sourceBaseType,
                           Object source)
  {
    // do nothing
  }
  
  @Override
  public void publishEvent(FacesContext context,
                           Class<? extends SystemEvent> systemEventClass,
                           Object source)
  {
    //do nothing
  }

  @Override
  public ActionListener getActionListener()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public void setActionListener(ActionListener listener)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public Locale getDefaultLocale()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public void setDefaultLocale(Locale locale)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public String getDefaultRenderKitId()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public void setDefaultRenderKitId(String renderKitId)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public String getMessageBundle()
  {
    // The spec prevents throwing a UnsupportedOperationException and we have class 
    // reading that property. Therefore it's better to return null which is a valid 
    // return value.
    return null;
  }

  @Override
  public void setMessageBundle(String bundle)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public NavigationHandler getNavigationHandler()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public void setNavigationHandler(NavigationHandler handler)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public PropertyResolver getPropertyResolver()
  {
    return _propertyResolver;
  }

  @Override
  public void setPropertyResolver(PropertyResolver resolver)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public VariableResolver getVariableResolver()
  {
    return _variableResolver;
  }

  @Override
  public void setVariableResolver(VariableResolver resolver)
  {
    _variableResolver = resolver;
  }

  @Override
  public ViewHandler getViewHandler()
  {
    return _viewHandler;
  }

  @Override
  public void setViewHandler(ViewHandler viewHandler)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public StateManager getStateManager()
  {
    return _stateManager;
  }

  @Override
  public void setStateManager(StateManager stateManager)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public void addComponent(String type, String className)
  {
    _components.put(type, className);
  }

  @Override
  public UIComponent createComponent(String type) throws FacesException
  {
    String s = _components.get(type);
    if (s == null)
      throw new IllegalArgumentException("No component for type " + type);
    try
    {
      Class<?> c = Class.forName(s);
      return (UIComponent) c.newInstance();
    }
    catch (Exception e)
    {
      throw (FacesException)
        (new FacesException("Could not create component of type " +
                            type + " with class " + s).initCause(e));
    }
  }

  @Override
  public UIComponent createComponent(ValueBinding binding, FacesContext context, String type)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Iterator<String> getComponentTypes()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public void addConverter(String id, String className)
  {
    _converterIdMap.put(id,className);
  }

  @SuppressWarnings("unchecked")
  @Override
  public void addConverter(Class type, String className)
  {
    _converterTypeMap.put(type,className);
  }

  @Override
  public Converter createConverter(String className)
  {
    //copy the RI code
    Converter converter = null;
    try
    {
      converter = (Converter) _mapLookUp(className, _converterIdMap);
      return converter;
    }
    catch (Exception e)
    {
      _LOG.log(e.getMessage());
      throw new FacesException(e.getMessage());
    }
    
  }
  
  private Object _mapLookUp(
      Object key, 
      Map<Object, Object> map) 
        throws InstantiationException, 
               IllegalAccessException, 
               ClassNotFoundException
  {
    Class<?> klass;
    Object value = map.get(key);
    if (value == null)
      return null;
    
    if (value instanceof String)
    {      
      klass = Class.forName((String)value);
      map.put(key, klass);
    }
    else
    {
      klass = (Class) value;
    }
    
    return klass.newInstance();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Converter createConverter(Class type)
  {
    Converter converter = null;
    try
    {
      converter = (Converter) _mapLookUp(type, _converterTypeMap);
      if (converter != null)
        return converter;
    }
    catch (Exception e)
    {
      _LOG.log(e.getMessage());
    }
    Class[] interfaces = type.getInterfaces();
    for (int i = 0; i < interfaces.length; i++)
    {
      converter = createConverter(interfaces[i]);
      if (converter != null)
        return converter;
    }
    
    Class<?> superclass = type.getSuperclass();
    if (superclass != null)
    {
      converter = createConverter(superclass);
      if (superclass != null)
        return converter;
    }
    return converter;
  }

  @Override
  public Iterator<String> getConverterIds()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public Iterator<Class<?>> getConverterTypes()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public MethodBinding createMethodBinding(String expr, Class[] paramTypes)
  {
    return null;
  }

  @Override
  public Iterator<Locale> getSupportedLocales()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @SuppressWarnings("unchecked")
  @Override
  public void setSupportedLocales(Collection locales)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public void addValidator(String id, String className)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public Validator createValidator(String validatorId)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Iterator<String> getValidatorIds()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public ValueBinding createValueBinding(String expression)
  {
    if (!expression.startsWith("#{") ||
        !expression.endsWith("}"))
      throw new UnsupportedOperationException("Haven't implemented that much yet!");
    return new MValueBinding(expression);
  }

 public Object evaluateExpressionGet(FacesContext context, String expression, Class expectedType) 
  {
      // TODO handle coercion
    return createValueBinding(expression).getValue(context);
  }

  public ExpressionFactory getExpressionFactory()
  {
    return _exprFactory;
  }

  @Override
  public ELResolver getELResolver()
  {
    return new MELResolver(
      getVariableResolver(), getPropertyResolver());
  }

  private ExpressionFactory _exprFactory = new ExpressionFactory()
  {
    public Object coerceToType(Object obj, Class<?> targetType)
    {
      return obj;
    }

    public MethodExpression createMethodExpression(
      ELContext context,
      String expression,
      Class<?> expectedReturnType,
      Class<?>[] expectedParamTypes) 
    {
      return new NotSupportedMethodExpression(expression);
    }

    public ValueExpression createValueExpression(
       ELContext context, String expression, Class<?> expectedType)
    {
      // TODO handle coercion
      ValueBinding vb = MApplication.this.createValueBinding(expression);
      return new ValueBindingValueExpression(vb);
    }
    
    
    public ValueExpression createValueExpression(
      Object instance, Class<?> expectedType)
    {
      throw new UnsupportedOperationException("Not implemented yet");
    }
  };


  /**
   * A very rudimentary implementation, to support
   * renderers that might create (but not evaluate) MethodExpressions
   */
  private class NotSupportedMethodExpression extends MethodExpression
    implements Serializable
  {
    public NotSupportedMethodExpression(String expression)
    {
      _expression = expression;
    }

    @Override
    public MethodInfo getMethodInfo(ELContext elContext)
    {
      throw new UnsupportedOperationException("Not implemented yet");
    }

    @Override
    public Object invoke(ELContext elContext, Object[] objects)
    {
      throw new UnsupportedOperationException("Not implemented yet");
    }

    @Override
    public String getExpressionString()
    {
      return _expression;
    }

    @Override
    public boolean isLiteralText()
    {
      return false;
    }


    @Override
    public boolean equals(Object object)
    {
      return (object == this);
    }

    @Override
    public int hashCode()
    {
      return System.identityHashCode(this);
    }

    private final String _expression;
  }

  private Map<String, String> _components = new HashMap<String, String>();
  private ViewHandler _viewHandler = new MViewHandler();
  private VariableResolver _variableResolver = new MVariableResolver();
  private PropertyResolver _propertyResolver = new MPropertyResolver();
  static private MApplication _INSTANCE = new MApplication();
  private Map<Object, Object> _converterIdMap = new HashMap<Object, Object>();
  private Map<Object, Object> _converterTypeMap = new HashMap<Object, Object>();
  private StateManager _stateManager = new MStateManager();
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StateUtils.class);
}
