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
package org.apache.myfaces.trinidad.change;

import java.util.HashMap;

import javax.el.ValueExpression;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * The base class for all ChangeManagers.
 * A ChangeManager should manage accumulation of Changes and also
 *  take care of their persistence.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/ChangeManager.java#0 $) $Date: 10-nov-2005.19:09:58 $
 */
public abstract class ChangeManager
{
  public static void registerDocumentFactory(
    String targetClassName,
    String converterClassName)
  {
    if ((targetClassName == null) || (targetClassName.length() == 0))
      throw new IllegalArgumentException(_LOG.getMessage(
        "TARGET_CLASS_NAME_MUST_BE_PROVIDED"));

    if ((converterClassName == null) || (converterClassName.length() == 0))
      throw new IllegalArgumentException(_LOG.getMessage(
        "CONVERTER_CLASS_NAME_MUST_BE_PROVIDED"));

    synchronized (_CLASSNAME_TO_CONVERTER_NAME_MAP)
    {
      _CLASSNAME_TO_CONVERTER_NAME_MAP.put(targetClassName, converterClassName);
    }
  }

  /**
   * Use the conversion rules to attempt to retrieve the equivalent
   * document change for a ComponentChange
   * @param change to convert
   */
  protected static DocumentChange createDocumentChange(
    ComponentChange change)
  {
    Class<? extends ComponentChange> changeClass = change.getClass();

    Object converterObject = null;
    DocumentChangeFactory converter = null;

    synchronized (_CLASS_TO_CONVERTER_MAP)
    {
      converterObject = _CLASS_TO_CONVERTER_MAP.get(changeClass);
    }

    if (converterObject != null)
    {
      converter = (DocumentChangeFactory)converterObject;
    }
    else
    {
      String converterName = null;

      synchronized (_CLASSNAME_TO_CONVERTER_NAME_MAP)
      {
       converterName = 
                  _CLASSNAME_TO_CONVERTER_NAME_MAP.get(changeClass.getName());
      }

      if (converterName != null)
      {
        try
        {
          ClassLoader contextClassLoader =
            Thread.currentThread().getContextClassLoader();

          Class<?> converterClass = contextClassLoader.loadClass(converterName);
          if (DocumentChangeFactory.class.isAssignableFrom(converterClass))
          {
            converter = (DocumentChangeFactory)converterClass.newInstance();

            synchronized (_CLASS_TO_CONVERTER_MAP)
            {
              _CLASS_TO_CONVERTER_MAP.put(changeClass, converter);
            }
          }
          else
          {
            // log warning because class isn't correct type
            _LOG.warning("CONVERSION_CLASS_TYPE", new Object[] {converterClass, DocumentChangeFactory.class}); // NOTRANS
          }
        }
        catch (Throwable e)
        {
          _LOG.warning("UNABLE_INSTANTIATE_CONVERTERCLASS", converterName); // NOTRANS
          _LOG.warning(e);
        }

	// if the registered converter class name doesn't work remove
	// it from _CLASSNAME_TO_CONVERT_NAME_MAP
        if (converter == null)
        {
          // this entry doesn't work, so remove it
          _CLASSNAME_TO_CONVERTER_NAME_MAP.remove(converterName);

          return null;
        }
      }
    }

    // return the converted object
    if (converter != null)
      return converter.convert(change);
    
    return null;
  }

  /**
   * Add a ComponentChange to this current request for a specified component.
   * When called we will disallow changes if the component or its any ancestor 
   * is a stamped component by UIXIterator. 
   *
   * @throws IllegalArgumentException if any of the supplied parameters were to
   *          be null.
   */
  public abstract void addComponentChange(
    FacesContext facesContext,
    UIComponent uiComponent,
    ComponentChange change);
  
  /**
   * Replace an AttributeComponentChange if it's present. 
   * 
   * @param facesContext
   * @param uiComponent
   * @param attributeComponentChange
   * @return the old change instance
   */
  public AttributeComponentChange replaceAttributeChangeIfPresent(FacesContext facesContext,
    UIComponent uiComponent,
    AttributeComponentChange attributeComponentChange)
  {    
    _LOG.warning("Must be implemented by subclass");
    return null;
  }  

  /**
   * Add a DocumentChange to this current request for a specified component.
   * When called we will allow changes even if the component or its any ancestor 
   * is a stamped component by UIXIterator.
   * 
   * @throws IllegalArgumentException if any of the supplied parameters were to
   *          be null.
   */
  public void addDocumentChange(
      FacesContext facesContext,
      UIComponent uiComponent,
      DocumentChange change)
  {
    if (facesContext == null || uiComponent == null || change == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "CANNOT_ADD_CHANGE_WITH_FACECONTEXT_OR_UICOMPONENT_OR_NULL"));
  }

  /**
   * Applies all the ComponentChanges added so far for the current view.
   * Developers should not need to call this method. Internal implementation
   * will call it as the component tree is built and is ready to take changes.
   * @param facesContext The FacesContext instance for the current request.
   */
  public void applyComponentChangesForCurrentView(FacesContext facesContext)
  {
    throw new UnsupportedOperationException("Subclassers must implement");
  }

  /**
   * Applies the ComponentChanges added so far for components underneath
   * the specified NamingContainer.
   * Developers should not need to call this method. Internal implementation
   * will call it as the component tree is built and is ready to take changes.
   * @param facesContext The FacesContext instance for the current request.
   * @param root The NamingContainer that contains the component subtree
   * to which ComponentChanges should be applied.  If null, all changes are
   * applied.
   * @throws IllegalArgumentException if the root NamingContainer is not a
   *   UIComponent instance.
   */
  public void applyComponentChangesForSubtree(
    FacesContext facesContext,
    NamingContainer root
    )
  {
    throw new UnsupportedOperationException("Subclassers must implement");
  }

  /**
   * Apply non-cross-component changes to a component in its original location.  This is typically
   * only called by tags that need to ensure that a newly created component instance is
   * as up-to-date as possible.
   * @param context
   * @param component Component to apply the simple changes to
   */
  public void applySimpleComponentChanges(FacesContext context, UIComponent component)
  {
    throw new UnsupportedOperationException("Subclassers must implement");    
  }
  
  private static class AttributeConverter extends DocumentChangeFactory
  {
    @Override
    public DocumentChange convert(ComponentChange compChange)
    {
      if (compChange instanceof AttributeComponentChange)
      {
        AttributeComponentChange change = (AttributeComponentChange)compChange;

        Object value = change.getAttributeValue();

        // =-= bts TODO add registration of attribute converters
        String valueString = null;
        if ((value == null) ||
            (value instanceof CharSequence) ||
            (value instanceof Number) ||
            (value instanceof Boolean))
        {
          valueString = (value != null)? value.toString() : null;
        }
        else if (value instanceof ValueExpression)
        {
          valueString = ((ValueExpression)value).getExpressionString();
        }
        else if (value instanceof ValueBinding)
        {
          valueString = ((ValueBinding)value).getExpressionString();
        }
        
        if (valueString != null)
          return new AttributeDocumentChange(change.getAttributeName(),
                                             valueString);
      }

      // no conversion possible
      return null;
    }
  }

  private static HashMap<String, String> _CLASSNAME_TO_CONVERTER_NAME_MAP =
    new HashMap<String, String>();
  
  private static HashMap<Class<? extends ComponentChange>, DocumentChangeFactory> _CLASS_TO_CONVERTER_MAP = 
    new HashMap<Class<? extends ComponentChange>, DocumentChangeFactory>();

  static private final TrinidadLogger _LOG = 
     TrinidadLogger.createTrinidadLogger(ChangeManager.class);

  static
  {
    // register the attribute converter
    _CLASS_TO_CONVERTER_MAP.put(AttributeComponentChange.class,
                                new AttributeConverter());
  }

}
