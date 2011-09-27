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
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/url/FormEncoder.java#0 $) $Date: 10-nov-2005.18:59:26 $
 */
abstract public class FormEncoder
{
  /**
   * Indicate that the form is now being encoded.
   */
  abstract public void startForm(
    Object formName);

  /**
   * Indicate that the form is finished.
   */
  abstract public void endForm();

  /**
   * Register a form parameter by name.
   */
  public void registerFormParameter(
    Object inputName)
  {
  }

  /**
   * Encode a form value.
   */
  abstract public Object encodeFormValue(
    Object inputName,
    Object inputValue);

  /**
   * Encode a client parameter.
   */
  abstract public Object encodeClientParameter(
    Object formName,
    Object inputName,
    Object inputValue);

}
