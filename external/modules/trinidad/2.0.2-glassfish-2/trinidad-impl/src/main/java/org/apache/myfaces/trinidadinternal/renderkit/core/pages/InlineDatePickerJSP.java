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
package org.apache.myfaces.trinidadinternal.renderkit.core.pages;

import java.io.IOException;
import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.core.CoreDocument;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;


/**
 * Entry point for the embedded calendar functionality.
 * <p>
 * Parameters:
 * <ul>
 * <li>Java locale to use
 * <li>tzOffset: the time zone offset in hours
 * <li>minValue, maxValue: min and max dates
 * <li>value: current value
 * <li>scrolledValue: first visible value
 * </ul>
 * <p>
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/pages/InlineDatePickerJSP.java#0 $) $Date: 10-nov-2005.19:03:35 $
 */
class InlineDatePickerJSP
{
  @SuppressWarnings("unchecked")
  static public void service(FacesContext context)
    throws IOException
  {
    Map<String, String> requestParams = 
      context.getExternalContext().getRequestParameterMap();

    RenderingContext arc = RenderingContext.getCurrentInstance();
    arc.getPartialPageContext().addPartialTarget(
              requestParams.get(XhtmlConstants.SOURCE_PARAM));

    CoreDocument doc = new CoreDocument();
    context.getViewRoot().getChildren().add(doc);
    doc.getChildren().add(CalendarUtils.createChooseDate(context));
  }
}
