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
package org.apache.myfaces.trinidadinternal.taglib;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentClassicTagBase;
import javax.servlet.jsp.JspException;

import org.apache.myfaces.trinidad.component.UIXComponentRef;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.webapp.TrinidadTagSupport;

public class ComponentDefTag extends TrinidadTagSupport
{
  public ComponentDefTag()
  {
  }


  private String _var;
  public void setVar(String var)
  {
    _var = var;
  }

  @Override
  public int doStartTag() throws JspException
  {
    UIComponentClassicTagBase tag = UIComponentClassicTagBase.getParentUIComponentClassicTagBase(pageContext);
    if (tag == null)
    {
      throw new JspException(_LOG.getMessage(
        "COMPOENENTDEF_CANNOT_RUN_AS_STANDALONE"));
    }

    // Only run on the first time the tag executes
    if (tag.getCreated())
    {
      UIComponent component = tag.getComponentInstance();
      if (!(component instanceof UIXComponentRef))
      {
        throw new JspException(_LOG.getMessage(
          "COMPONENTDEF_MUST_BE_INCLUDED_AS_CHILD_OF"));
      }

      if (_var != null)
      {
        ((UIXComponentRef) component).setVar(_var);
      }
    }

    return EVAL_PAGE;
  }

  @Override
  public void release()
  {
    super.release();
    _var = null;
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ComponentDefTag.class);
  private static final long serialVersionUID = 1L;
}
