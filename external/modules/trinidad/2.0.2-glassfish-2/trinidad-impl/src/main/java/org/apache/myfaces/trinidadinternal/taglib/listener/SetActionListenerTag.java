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
import javax.faces.application.Application;
import javax.faces.component.ActionSource;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.webapp.UIComponentClassicTagBase;
import javax.servlet.jsp.JspException;

import org.apache.myfaces.trinidad.event.SetActionListener;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.webapp.ELContextTag;
import org.apache.myfaces.trinidad.webapp.TrinidadTagSupport;

/**
 * JavaServer Faces version 1.2 a <code>setPropertyActionListener</code>, which provides the 
 * same functionality. In JSF 1.2 days this class should be <code>deprecated</code>.
 *
 */
public class SetActionListenerTag extends TrinidadTagSupport
{
  public void setFrom(ValueExpression from)
  {
    _from = from;
  }

  public void setTo(ValueExpression to)
  {
    _to = to;
  }

  @Override
  public int doStartTag() throws JspException
  {
   UIComponentClassicTagBase tag = UIComponentClassicTagBase.getParentUIComponentClassicTagBase(pageContext);
    if (tag == null)
    {
      throw new JspException(_LOG.getMessage(
        "SETACTIONLISTENER_MUST_INSIDE_UICOMPONENT_TAG"));
    }

    // Only run on the first time the tag executes
    if (!tag.getCreated())
      return SKIP_BODY;

    UIComponent component = tag.getComponentInstance();
    if (!(component instanceof ActionSource))
    {
      throw new JspException(_LOG.getMessage(
        "SETACTIONLISTENER_MUST_INSIDE_UICOMPONENT_TAG"));
    }

    ELContextTag parentELContext = (ELContextTag)
       findAncestorWithClass(this, ELContextTag.class);

    Application application =
      FacesContext.getCurrentInstance().getApplication();

    SetActionListener listener = new SetActionListener();
    if (_from != null)
    {
      listener.setValueExpression("from", _from);
      if (_to.isLiteralText())
        throw new JspException(_LOG.getMessage(
          "SETACTIONLISTENERS_TO_MUST_BE_EL_EXPRESSION"));

      if (_to != null)
        listener.setValueExpression("to", _to);
    }

    ((ActionSource) component).addActionListener(listener);

    return super.doStartTag();
  }

  @Override
  public void release()
  {
    super.release();
    _from = null;
    _to = null;
  }

  private ValueExpression _from;
  private ValueExpression _to;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    SetActionListenerTag.class);
}
