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
package org.apache.myfaces.trinidad.model;

import java.util.List;
import java.util.Calendar;
import java.util.Date;

import javax.faces.context.FacesContext;


/**
 * Defines the interface for providing a list of individual dates within a
 * given range. This interface is used in date picker components
 * (e.g. chooseDate, selectInputDate).
 */
public interface DateListProvider
{
  /**
   * This method will generate a {@link java.util.List List} of individual
   * {@link java.util.Date Date} objects. This is often used to list the dates
   * which will be rendered as disabled in a datePicker component. The {@link
   * java.util.Date Date}s must be in the context of the given base {@link
   * java.util.Calendar Calendar}.
   *
   * @param context The Faces context
   * @param base The base {@link java.util.Calendar Calendar} object from which
   *             the start and end dates are taken. All returned
   *             {@link java.util.Date Date} objects should be gotten from this
   *             base by a series of {@link java.util.Calendar#set set} and
   *             {@link java.util.Calendar#getTime getTime} calls.
   * @param rangeStart The start of the range for which dates are being
   *                   requested.
   * @param rangeEnd The end of the range for which dates are being requested.
   */
  public List <Date> getDateList(FacesContext context,
                                 Calendar base,
                                 Date rangeStart,
                                 Date rangeEnd);
}
