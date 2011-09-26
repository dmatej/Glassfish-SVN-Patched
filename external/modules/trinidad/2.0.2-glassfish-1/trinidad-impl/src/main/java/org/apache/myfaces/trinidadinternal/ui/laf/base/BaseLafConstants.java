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
package org.apache.myfaces.trinidadinternal.ui.laf.base;


/**
 * Constants used throughout the laf Rendering.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/BaseLafConstants.java#0 $) $Date: 10-nov-2005.18:52:56 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface BaseLafConstants
{
  /** Unicode character for non-breaking space */
  public static final char NBSP_CHAR = 0xA0;

  /** String containing Unicode character for non-breaking space */
  public static final String NBSP_STRING = String.valueOf(NBSP_CHAR);

  public static final char URI_DELIMITER = '/';

  /** The Integer 0 */
  public static final Number ZERO = Integer.valueOf(0);

  /** The Integer 1 */
  public static final Number ONE  = Integer.valueOf(1);

  /** The Integer 2 */
  public static final Number TWO  = Integer.valueOf(2);

  /** Value indicating the value was not found in the collection */
  public static final Object NOT_FOUND = new Object();
}
