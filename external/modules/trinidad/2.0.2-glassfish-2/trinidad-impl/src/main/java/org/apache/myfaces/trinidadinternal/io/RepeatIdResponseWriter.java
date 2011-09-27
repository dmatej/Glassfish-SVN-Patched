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
package org.apache.myfaces.trinidadinternal.io;

import java.io.IOException;
import java.io.Writer;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

/**
 * ResponseWriter implementation for adding a suffix to 'id' and 'for'
 * attributes.  Use this when you repeat a component so you do not
 * get repeating ids on the page.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/io/RepeatIdResponseWriter.java#0 $) $Date: 10-nov-2005.19:03:50 $
 * @todo Move to renderkit.core
 */
public class RepeatIdResponseWriter extends ResponseWriterDecorator
{
  static public ResponseWriter install(FacesContext context)
  {
    ResponseWriter oldRW = context.getResponseWriter();
    
    ResponseWriter newRW = new RepeatIdResponseWriter(oldRW);
    context.setResponseWriter(newRW);
    
    return oldRW;
  }

  static public void remove(FacesContext context, ResponseWriter oldRW)
  {
    context.setResponseWriter(oldRW);
  }

  private RepeatIdResponseWriter(ResponseWriter out)
  {
    super(out);
  }
  
  @Override
  public ResponseWriter cloneWithWriter(Writer writer)
  {
    return new RepeatIdResponseWriter(
      getResponseWriter().cloneWithWriter(writer));
  }

  @Override
  public void writeAttribute(String     name,
                             Object     value,
                             String     property) throws IOException
  {
    super.writeAttribute(name,
                         _transformIdAndForValues(name, value),
                         property);
  }

  /**
   * if the name is "id" or "for" transform the value by 
   * adding a suffix. 
   * Also, if the id is currently being rendered as a partial
   * target, then add the transformed id to the list of partial targets.
   * 
   * @param name name of attribute. e.g., id
   * @param value value of attribute
   * @return the value with the suffix appended if id or for, otherwise
   *         the value untouched.
   */
  private Object _transformIdAndForValues(String name, Object value)
  {
    if (value == null)
      return null;
    
    if ("id".equals(name) || "for".equals(name))
    {
      String transformedId = value + ID_SUFFIX;
      return transformedId;
    } 
    else
    {
      return value;
    }
  }

  private static String ID_SUFFIX = "_adfr";  
}
