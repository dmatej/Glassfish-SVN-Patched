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
 * Interface for the encoding of URLs.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/url/URLEncoder.java#0 $) $Date: 10-nov-2005.18:59:28 $
 */
public interface URLEncoder
{
  /**
   * Given the logical name of a parameter, return the parameter
   * key that should be used in the URL.  This function should not
   * be used for parameter values.
   */
  public String encodeParameter(String key);

  /**
   * Encode an URL.
   */
  public String encodeURL(String url);

  /**
   * @return this is an URL to the current page. This is useful, for example,
   *  in a form submission; if the destination of a form has not been set, this
   *  default URL can be used to submit the form back to the page it originated
   *  from.
   */
  public String getDefaultURL(); 
}
