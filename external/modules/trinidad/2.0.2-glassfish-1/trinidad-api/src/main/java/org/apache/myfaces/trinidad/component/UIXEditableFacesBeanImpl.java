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

import org.apache.myfaces.trinidad.bean.PropertyKey;

/**
 * UIXFacesBeanImpl subclass that implements the local value contract needed for ValueHolders.
 * UIXEditableValue subclasses that wish to modify their FacesBean behavior should subclass
 * this class.
 * @see org.apache.myfaces.trinidad.bean.FacesBeanImpl
 * @see org.apache.myfaces.trinidad.component.UIXEditableValue
 */
public class UIXEditableFacesBeanImpl extends UIXFacesBeanImpl
{
  public UIXEditableFacesBeanImpl()
  {
  }

  /**
   * Subclassers most call super with the component and type
   * @param component UIXEditableValue to bind to this UIXFacesBean
   * @param type
   * @throws IllegalStateException if init() called more than once
   * @throws IllegalArgumentException if component is not a UIXEditableValue
   * @throws NullPointerException of component or type is null
   */
  @Override
  public void init(
    UIXComponent component,
    Type type)
  {
    // UIXFacesBeanImpl only works with UIXComponentBase
    if (!(component instanceof UIXEditableValue))
      throw new IllegalArgumentException(component.getClass() +" is not a UIXEditableValue");

    super.init(component, type);
  }
  

  @Override
  public void setProperty(PropertyKey key, Object value)
  {
    super.setProperty(key, value);
    if (key == UIXEditableValue.VALUE_KEY)
      setProperty(UIXEditableValue.LOCAL_VALUE_SET_KEY, Boolean.TRUE);
  }
}
