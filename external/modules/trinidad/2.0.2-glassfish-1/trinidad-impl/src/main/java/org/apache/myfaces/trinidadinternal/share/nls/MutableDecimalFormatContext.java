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
package org.apache.myfaces.trinidadinternal.share.nls;

/**
 * The MutableDecimalFormatContext class contains all number format parameters,
 * which can be changed again after initialisation.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/nls/MutableDecimalFormatContext.java#0 $) $Date: 10-nov-2005.19:00:04 $
 */
public final class MutableDecimalFormatContext extends DecimalFormatContext
{
  public MutableDecimalFormatContext(
    DecimalFormatContext dfc)
  {
    _groupingSeparator = dfc.getGroupingSeparator();
    _decimalSeparator = dfc.getDecimalSeparator();
  }

  /**
   * Returns the character used to separate number groupings.
   */
  @Override
  public char getGroupingSeparator()
  {
    return _groupingSeparator;
  }


  /**
   * Sets the character used to separate number groupings.
   */
  public void setGroupingSeparator(char groupingSeparator)
  {
    _groupingSeparator = groupingSeparator;
  }


  /**
   * Returns the character used as a decimal separator.
   */
  @Override
  public char getDecimalSeparator()
  {
    return _decimalSeparator;
  }


  /**
   * Sets the character used as a decimal separator.
   */
  public void setDecimalSeparator(char decimalSeparator)
  {
    _decimalSeparator = decimalSeparator;
  }

  private char _groupingSeparator;
  private char _decimalSeparator;
}

