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
package org.apache.myfaces.trinidadinternal.uinode.nav;

import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandLink;

import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.collection.AttributeMap;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.uinode.FireActionBoundValue;
import org.apache.myfaces.trinidadinternal.uinode.UINodeFacesBean;
import org.apache.myfaces.trinidadinternal.uinode.bind.PropertyBoundValue;

/**
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class CommandLinkFacesBean extends UINodeFacesBean
{
  @Override
  protected AttributeMap createAttributeMap(String componentFamily)
  {
    AttributeMap attrMap = super.createAttributeMap(componentFamily);

    BoundValue unvalidatedBV = new PropertyBoundValue(this,
                                             CoreCommandLink.IMMEDIATE_KEY);
    BoundValue partialBV = new PropertyBoundValue(this,
                                CoreCommandLink.PARTIAL_SUBMIT_KEY);                                                                 

    attrMap.setAttribute(UIConstants.PRIMARY_CLIENT_ACTION_ATTR,
                         getPrimaryClientActionBoundValue(getUIXComponent(),
                                                  unvalidatedBV,
                                                  partialBV));

    return attrMap;
  }
  
  protected BoundValue getPrimaryClientActionBoundValue(
    UIXComponent component,
    BoundValue   unvalidatedBV,
    BoundValue   partialBV
  )
  {
    BoundValue bv = new FireActionBoundValue(component, 
                                             unvalidatedBV,
                                             partialBV);
    return bv;
  }
}
