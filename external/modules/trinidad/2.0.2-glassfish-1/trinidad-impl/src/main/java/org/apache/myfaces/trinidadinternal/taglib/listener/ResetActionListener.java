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
package org.apache.myfaces.trinidadinternal.taglib.listener;

import java.io.Serializable;

import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.component.UIForm;
import javax.faces.component.EditableValueHolder;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.component.UIXEditableValue;
import org.apache.myfaces.trinidad.component.UIXForm;
import org.apache.myfaces.trinidad.component.UIXSubform;
import org.apache.myfaces.trinidad.context.RequestContext;

/**
 * An action listener that will reset all the editable values
 * in the enclosing form.
 *
 */
public class ResetActionListener
  implements ActionListener, Serializable
{
  public ResetActionListener()
  {
  }

  public void processAction(ActionEvent event)
  {
    UIComponent form = _getContainingForm(event.getComponent());
    _resetChildren(form);
  }

  public boolean isTransient()
  {
    return false;
  }

  public void setTransient(boolean newTransientValue)
  {
    throw new UnsupportedOperationException();
  }

  private UIComponent _getContainingForm(UIComponent component)
  {
    UIComponent previous = component;
    UIComponent parent = component.getParent();

    while (parent != null)
    {
      if ((parent instanceof UIForm)
          || (parent instanceof UIXForm)
          || (parent instanceof UIXSubform))
        return parent;

      previous = parent;
      parent = parent.getParent();
    }
    return previous;
  }

  @SuppressWarnings("unchecked")
  private void _resetChildren(UIComponent comp)
  {
    Iterator<UIComponent> kids = comp.getFacetsAndChildren();

    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      
      if (kid instanceof UIXEditableValue)
      {
        ((UIXEditableValue) kid).resetValue();
        RequestContext.getCurrentInstance().addPartialTarget(kid);
      }
      else if (kid instanceof EditableValueHolder)
      {
        _resetEditableValueHolder((EditableValueHolder) kid);
        RequestContext.getCurrentInstance().addPartialTarget(kid);
      }
      else if (kid instanceof UIXCollection)
      {
        ((UIXCollection) kid).resetStampState();
        RequestContext.getCurrentInstance().addPartialTarget(kid);
      }

      _resetChildren(kid);
    }
  }

  private void _resetEditableValueHolder(EditableValueHolder evh)
  {
    evh.setValue(null);
    evh.setSubmittedValue(null);
    evh.setLocalValueSet(false);
    evh.setValid(true);
  }

  private static final long serialVersionUID = 1L;
}
