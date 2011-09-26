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
package org.apache.myfaces.trinidadinternal.share.url;

/**
 * Class for the encoding of form values that are processed by the
 * server during postback.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/url/NullFormEncoder.java#0 $) $Date: 10-nov-2005.18:59:26 $
 */
public class NullFormEncoder extends FormEncoder
{
  /**
   * Indicate that the form is now being encoded.
   */
  @Override
  public void startForm(
    Object formName)
  {
    // nop
  }

  /**
   * Indicate that the form is finished.
   */
  @Override
  public void endForm()
  {
    // nop
  }

  /**
   * Encode a form value.
   */
  @Override
  public Object encodeFormValue(
    Object inputName,
    Object inputValue)
  {
    // nop
    return inputValue;
  }

  /**
   * Encode a client parameter.
   */
  @Override
  public Object encodeClientParameter(
    Object formName,
    Object inputName,
    Object inputValue)
  {
    // nop
    return inputValue;
  }

  static final public NullFormEncoder sharedInstance()
  {
    return _INSTANCE;
  }

  /**
   * No instances other than shared instance.
   */
  private NullFormEncoder()
  {
  }

  static final private NullFormEncoder _INSTANCE = new NullFormEncoder();
}
