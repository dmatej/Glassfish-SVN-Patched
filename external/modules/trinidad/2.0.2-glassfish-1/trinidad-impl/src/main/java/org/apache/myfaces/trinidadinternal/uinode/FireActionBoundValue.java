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

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.action.FirePartialAction;
import org.apache.myfaces.trinidadinternal.ui.collection.Parameter;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

import org.apache.myfaces.trinidadinternal.ui.action.FireAction;

import org.apache.myfaces.trinidad.component.UIXComponent;

/**
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class FireActionBoundValue implements BoundValue
{
  public FireActionBoundValue(
   UIXComponent component,
   BoundValue   unvalidatedBV
   )
  {
    this(component, unvalidatedBV, null, null);
  }

  public FireActionBoundValue(
   UIXComponent component,
   BoundValue   unvalidatedBV,
   BoundValue   partialSubmitBV
   )
  {
    this(component, unvalidatedBV, partialSubmitBV, null);
  }

  public FireActionBoundValue(
   UIXComponent component,
   BoundValue   unvalidatedBV,
   BoundValue   partialSubmitBV,
   String       part
   )
  {
    if ( component == null )
      throw new IllegalArgumentException();

    _component = component;
    _unvalidatedBV = unvalidatedBV;
    _partialSubmitBV = partialSubmitBV;
    _part = part;
  }

  @SuppressWarnings("unchecked")
  public Object getValue(UIXRenderingContext context)
  {
    FacesContext fContext = (context == null) ?
      FacesContext.getCurrentInstance() : context.getFacesContext();

    String clientID = _component.getClientId(fContext);
    if ( _clientID == null || !_clientID.equals(clientID))
    {
      _clientID = clientID;

      Object isPartial = null;
      if (_partialSubmitBV != null)
        isPartial = _partialSubmitBV.getValue(context);

      if (Boolean.TRUE.equals(isPartial))
        _fireAction = new FirePartialAction(null, true);
      else
        _fireAction = new FireAction(null, true);

      _fireAction.setSource(clientID);
      _fireAction.setUnvalidatedBinding(_unvalidatedBV);

      if (_component.getChildCount() > 0 || (_part != null))
      {
        // Get an array of parameters to pass to the fireAction object
        List<UIComponent> kids = _component.getChildren();
        ArrayList<Parameter> params = new ArrayList<Parameter>(kids.size());
        
        for ( int i = 0; i < kids.size(); i++ )
        {
          Object kid = kids.get(i);
          
          if (kid instanceof UIParameter)
          {
            Object value = ((UIParameter)kid).getValue();
            if (value != null)
            {
              Parameter p = new Parameter();
              p.setKey(((UIParameter)kid).getName());
              p.setValue(value.toString());
              params.add(p);
            }
          }
        }

        if (_part != null)
        {
          Parameter p = new Parameter();
          p.setKey("part");
          p.setValue(_part);
          params.add(p);
        }

        Parameter [] paramArray = params.toArray(_sPARAMS_ARRAY);
        _fireAction.setParameters(paramArray);
      }
    }

    return _fireAction;
  }

  private UIXComponent _component;
  private String _clientID;
  private String _part;
  private FireAction _fireAction;
  private BoundValue _unvalidatedBV;
  private BoundValue _partialSubmitBV;
  private static final Parameter[] _sPARAMS_ARRAY = new Parameter[0];
}
