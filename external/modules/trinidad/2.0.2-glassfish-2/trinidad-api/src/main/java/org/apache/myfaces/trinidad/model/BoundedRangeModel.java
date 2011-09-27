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

/**
 * Model that represents a bounded range, 0 <- value <- maximum if value > -1 and
 * maximum > -1.  'value' or 'maximum' being -1 indicates that they are unknown.
 * This model is used by the 'progress' components.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/model/BoundedRangeModel.java#0 $) $Date: 10-nov-2005.19:08:49 $
 */

public abstract class BoundedRangeModel 
{
  /**
   * Gets the maximum value for this model.
   * @return the maximum value. A return of -1 indicates that maximum is unknown.
   */
  public abstract long getMaximum();

  /**
   * Gets the current value for this model.
   * @return the current value. A return of -1 indicates that value is unknown.
   */
  public abstract long getValue();
}
