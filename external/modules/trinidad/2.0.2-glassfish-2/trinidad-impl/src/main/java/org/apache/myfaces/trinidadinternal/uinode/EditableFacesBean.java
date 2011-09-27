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
package org.apache.myfaces.trinidadinternal.uinode;

import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXEditableValue;

import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.collection.AttributeMap;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.uinode.bind.ConverterBoundValue;
import org.apache.myfaces.trinidadinternal.uinode.bind.EntriesBoundValue;
import org.apache.myfaces.trinidadinternal.uinode.bind.PropertyBoundValue;

/**
 * 
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class EditableFacesBean extends UINodeFacesBean
{

 /**
  * @todo what about UIXEditableValue.VALIDATOR_KEY?
  */
  @Override
  protected AttributeMap createAttributeMap(String componentFamily)
  {
    AttributeMap attrMap = super.createAttributeMap(componentFamily);
    attrMap.setAttribute(UIConstants.CONVERTER_ATTR,
                         getConverterBoundValue());
                                                      
                                                          
    attrMap.setAttribute(UIConstants.VALIDATORS_ATTR,
                  new EntriesBoundValue(this,  
                                        UIXEditableValue.VALIDATORS_KEY));
                          
    BoundValue unvalidatedBV = new PropertyBoundValue(this,
                                             UIXEditableValue.IMMEDIATE_KEY);
    attrMap.setAttribute(UIConstants.UNVALIDATED_ATTR, unvalidatedBV);                                 
    return  attrMap;
  }
  
  @Override
  public void setProperty(PropertyKey key, Object value)
  {
    super.setProperty(key, value);
    if (key == UIXEditableValue.VALUE_KEY)
      setProperty(UIXEditableValue.LOCAL_VALUE_SET_KEY, Boolean.TRUE);
  }
  
  protected BoundValue getConverterBoundValue()
  {
    return new ConverterBoundValue(getUIXComponent());  
  }
}
