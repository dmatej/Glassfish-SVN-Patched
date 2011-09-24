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

import javax.faces.application.FacesMessage;

import org.apache.myfaces.trinidad.util.LabeledFacesMessage;

/**
 * Wraps faces messages thrown by third party converters so that they appear as
 * LabeledFacesMessages.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/component/FacesMessageWrapper.java#0 $) $Date: 10-nov-2005.19:09:45 $
 */
class FacesMessageWrapper extends LabeledFacesMessage
{
  public FacesMessageWrapper(FacesMessage wrapped, Object label)
  {
    _wrapped = wrapped;
    setLabel(label);
  }

  @Override
  public String getDetail()
  {
    return _wrapped.getDetail();
  }

  @Override
  public FacesMessage.Severity getSeverity()
  {
    return _wrapped.getSeverity();
  }

  @Override
  public String getSummary()
  {
    return _wrapped.getSummary();
  }

  @Override
  public void setDetail(String detail)
  {
    _wrapped.setDetail(detail);
  }

  @Override
  public void setSeverity(FacesMessage.Severity severity)
  {
    _wrapped.setSeverity(severity);
  }

  @Override
  public void setSummary(String summary)
  {
    _wrapped.setSummary(summary);
  }

  private FacesMessage _wrapped;
  private static final long serialVersionUID = 1L;
}
