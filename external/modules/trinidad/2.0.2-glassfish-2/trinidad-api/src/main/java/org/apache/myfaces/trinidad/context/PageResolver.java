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
package org.apache.myfaces.trinidad.context;

/**
 *  A page resolver is responsible for determining the physical page to
 *  be used for a logical view ID.
 *  <p>
 *  By default, the physical page and logical view ID will be identical.
 *  To override this, provide a file on the classpath at
 *  <code>/META-INF/services/org.apache.myfaces.trinidad.context.PageResolver</code>
 *  with the name of the alternative implementation.  (There's no current
 *  support for decoration, and this general approach may be revisited
 *  in the future.)
 *  </p>
 */
public abstract class PageResolver
{  
  /**
   * Constructor.
   */
  protected PageResolver()
  {
  }
  
  /**
   * Performs mapping from a logical view ID to the physical page URI.
   * @param viewId  the logical view ID.
   * @return the URI of the page.
   */
  public abstract String getPhysicalURI(String viewId);
  
  /**
   * Encodes an action URI.
   */
  public abstract String encodeActionURI(String actionURI);
}
