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

import javax.el.ValueExpression;

import javax.faces.component.StateHolder;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.FacesBeanImpl;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.RequestContext;

/**
 * This action listener returns a value from a dialog or process.
 */
public class ReturnActionListener extends FacesBeanImpl
  implements ActionListener, StateHolder
{
  static public final FacesBean.Type TYPE = new FacesBean.Type();
  static public final PropertyKey VALUE_KEY =
    TYPE.registerKey("value");
  static
  {
    TYPE.lock();
  }

  public ReturnActionListener()
  {
  }

  public void processAction(ActionEvent event)
  {
    Object value = getProperty(VALUE_KEY);
    RequestContext adf = RequestContext.getCurrentInstance();
    adf.returnFromDialog(value, null);
  }

  public void setValue(ValueExpression value)
  {
    if (value.isLiteralText())
      setProperty(VALUE_KEY, value.getValue(null));
    else
      setValueExpression(VALUE_KEY, value);
  }

  @Override
  public Type getType()
  {
    return TYPE;
  }

  public boolean isTransient()
  {
    return false;
  }

  public void setTransient(boolean newTransientValue)
  {
    throw new UnsupportedOperationException();
  }

}
