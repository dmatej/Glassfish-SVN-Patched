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
package org.apache.myfaces.trinidad.component;

import java.util.Collections;
import java.util.Set;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBeanImpl;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.util.CollectionUtils;

/**
 * FacesBeanImpl subclass that implements UIXFacesBean.  UIXComponentBase subclasses that want to
 * change the behavior of their associated FacesBean are encouraged to subclass this class in preference
 * to implementing the FacesBean and UIXFaceBean contracts directly.  In addition, while
 * UIXComponentBase will work with any UIXFacesBean, it is optimized to work with
 * UIXFacesBeanImpl.
 * @see org.apache.myfaces.trinidad.bean.FacesBean
 * @see org.apache.myfaces.trinidad.bean.FacesBeanImpl
 * @see org.apache.myfaces.trinidad.component.UIXFacesBean
 * @see org.apache.myfaces.trinidad.component.UIXComponentBase
 */
public class UIXFacesBeanImpl extends FacesBeanImpl implements UIXFacesBean
{
  public UIXFacesBeanImpl()
  {
  }

  public final Type getType()
  {
    return _type;
  }

  /**
   * @return the UIXComponent that this UIXFacesBean as initialized with
   */
  public final UIXComponent getComponent()
  {
    return _component;
  }

  /**
   * Subclassers most call super with the component and type
   * @param component UIXComponentBase to bind to this UIXFacesBean
   * @param type
   * @throws IllegalStateException if init() called a second time with a different component or if
   * the Type changes for one non-null Type to another
   * @throws IllegalArgumentException if component is not a UIXComponentBase
   * @throws NullPointerException of component is null
   */
  public void init(
    UIXComponent component,
    Type type)
  {
    // component can only be specified once
    if ((_component != null) && (_component != component))
      throw new IllegalStateException("FacesBean Component Changed");
 
    // type can only be specified once
    if ((_type != null) && (_type != type))
      throw new IllegalStateException("FacesBean Type Changed");
       
    // UIXFacesBeanImpl only works with UIXComponentBase
    if (!(component instanceof UIXComponentBase))
      throw new IllegalArgumentException(component.getClass() +" is not a UIXComponentBase");
    
    if (component == null)
      throw new NullPointerException("UIXFacesBean must have a component");

    _type = type;
    _component = component;
  }  

  @Override
  public Set<PropertyKey> keySet()
  {
    // override to make sure that the id key is in the returned set.
    Set<PropertyKey> baseSet = super.keySet();
    
    if (baseSet.isEmpty())
      return _ID_KEY_SET;
    else
      return CollectionUtils.compositeSet(baseSet, _ID_KEY_SET);
  }

  @Override
  public void setPropertyImpl(PropertyKey key, Object value)
  {
    // delegate sets of the id back to the UIXComponent
    if (key == UIXComponentBase.ID_KEY)
    {
      _component.setId((String)value);
    }
    else
    {
      super.setPropertyImpl(key, value);
    }
  }

  @Override
  protected Object getLocalPropertyImpl(PropertyKey key)
  {
    if (key == UIXComponentBase.ID_KEY)
    {
      return _component.getId();
    }
    else
    {
      return super.getLocalPropertyImpl(key);
    }
  }

  @Override
  public Object saveState(FacesContext context)
  {
    // save the id in addition to the rest of the FacesBean state
    Object[] addIdState = new Object[2];
    
    addIdState[0] = _component.getId();
    addIdState[1] = super.saveState(context);
    
    return addIdState;
  }

  @Override
  public void restoreState(FacesContext context, Object state)
  {
    // restore the id in addition to the rest of the FacesBean state
    Object[] addIdState = (Object[])state;
    assert addIdState.length == 2;
    
    _component.setId((String)addIdState[0]);
    super.restoreState(context, addIdState[1]);
  }
  
  // Set containing the ID Key
  private static final Set<PropertyKey> _ID_KEY_SET =Collections.singleton(UIXComponentBase.ID_KEY);
  
  private Type _type;
  private UIXComponent _component;
}
