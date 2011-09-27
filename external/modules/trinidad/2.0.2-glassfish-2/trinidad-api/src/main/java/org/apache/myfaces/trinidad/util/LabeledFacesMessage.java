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
package org.apache.myfaces.trinidad.util;

import javax.el.ValueExpression;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

/**
 * Extension to FacesMessage which keeps track of the label on the component
 * that generated the message.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/util/LabeledFacesMessage.java#0 $) $Date: 10-nov-2005.19:08:38 $
 */
public class LabeledFacesMessage extends FacesMessage
{
  public LabeledFacesMessage()
  {
  }

  /**
   * Creates a LabeledFacesMessage without a pre-set label.
   * @param severity the severity of the message
   * @param summary the message summary
   * @param detail the message detail
   */
  public LabeledFacesMessage(
    FacesMessage.Severity severity,
    String summary,
    String detail)
  {
    super(severity, summary, detail);
  }


  /**
   * Creates a LabeledFacesMessage with a label.
   * @param severity the severity of the message
   * @param summary the message summary
   * @param detail the message detail
   * @param label the message label - either a String or a ValueBinding
   */
  public LabeledFacesMessage(
    FacesMessage.Severity severity,
    String summary,
    String detail,
    Object label)
  {
    super(severity, summary, detail);
    _label = label;
  }

  /**
   * Returns the label, which can be either a String or a ValueBinding.
   */
  public Object getLabel()
  {
    return _label;
  }

  /**
   * Sets the label, which can be either a String or a ValueBinding.
   */
  public void setLabel(Object label)
  {
    _label = label;
  }

  /**
   * Gets a string representation of the label.  If the label
   * is a ValueBinding, the expression is evaluated and the string
   * value returned.
   */
  public String getLabelAsString(FacesContext context)
  {
    Object label = getLabel();

    if (label instanceof ValueExpression)
    {
      label = ((ValueExpression) label).getValue(context.getELContext());          
    }
    else if (label instanceof ValueBinding)
    {
      label = ((ValueBinding) label).getValue(context);
    }

    if (label == null)
      return null;

    return label.toString();
  }


  private Object _label;
  private static final long serialVersionUID = 1L;
}
