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
package org.apache.myfaces.trinidadinternal.share.data;

import java.util.Iterator;


/**
 * Class wrapping up access to parameters.  This allows
 * the values and contents of these parameters to be modified from their
 * values in the underlying parameter scheme.  Clients using Servlets
 * will use the ServletRequestParameters implementation of this
 * class to wrap access to the ServletRequests's parameters.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/data/RequestParameters.java#0 $) $Date: 10-nov-2005.19:00:16 $
 */
public abstract class RequestParameters
{
  /**
   * Returns the value of a request parameter as a <code>String</code>,
   * or <code>null</code> if the parameter does not exist. Request parameters
   * are extra information sent with the request.  For HTTP servlets,
   * parameters are contained in the query string or posted form data.
   *
   * <p>You should only use this method when you are sure the
   * parameter has only one value. If the parameter might have
   * more than one value, use {@link #getParameterValues}.
   *
   * <p>If you use this method with a multivalued
   * parameter, the value returned is equal to the first value
   * in the array returned by <code>getParameterValues</code>.
   *
   * @param name   a <code>String</code> specifying the
   *      name of the parameter
   *
   * @return    a <code>String</code> representing the
   *      single value of the parameter
   *
   * @see     #getParameterValues
   *
   */
  public String getParameter(String name)
  {
    String[] values = getParameterValues(name);

    if ((values != null) && (values.length > 0))
    {
      return values[0];
    }
    else
    {
      return null;
    }
  }


  /**
   *
   * Returns an <code>Iterator</code> of <code>String</code>
   * objects containing the names of the parameters contained
   * in this request. If the request has
   * no parameters, the method returns an
   * empty <code>Iterator</code>.
   *
   * @return    an <code>Iterator</code> of <code>String</code>
   *      objects, each <code>String</code> containing
   *       the name of a request parameter; or an
   *      empty <code>Iterator</code> if the
   *      request has no parameters
   *
   */
  public abstract Iterator<String> getParameterNames();

  /**
   * Returns an array of <code>String</code> objects containing
   * all of the values the given request parameter has, or
   * <code>null</code> if the parameter does not exist.
   *
   * <p>If the parameter has a single value, the array has a length
   * of 1.
   *
   * @param name  a <code>String</code> containing the name of
   *      the parameter whose value is requested
   *
   * @return    an array of <code>String</code> objects
   *      containing the parameter's values
   *
   * @see    #getParameter
   *
   */
  public abstract String[] getParameterValues(String name);
}
