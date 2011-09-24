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


import java.util.Map;

import javax.faces.context.FacesContext;

/**
 * Abstract implementation for code that provides page flow scopes;  this
 * may be overridden.
 * <p>
 *  To override this, provide a file on the classpath at
 *  <code>/META-INF/services/org.apache.myfaces.trinidad.context.PageFlowScopeProvider</code>
 *  with the name of the alternative implementation.  (There's no current
 *  support for decoration, and this general approach may be revisited
 *  in the future.)
 *  </p>
 */
public abstract class PageFlowScopeProvider
{  
  /**
   */
  protected PageFlowScopeProvider()
  {
  }

  /**
   * Returns the current PageFlowScope, including any calls
   * to <code>pushPageFlowScope()</code> or <code>popPageFlowScope()</code>.
   * 
   * @param context the current FacesContext
   */
  public abstract Map<String, Object> getPageFlowScope(FacesContext context); 

  /**
   * Pushes a new process scope onto the stack.
   *
   * @param context the current FacesContext
   * @param copyParent if true, all values from the parent process
   *   scope will be copied into the new process scope.
   * @return the new scope
   */
  public abstract Map<String, Object> pushPageFlowScope(
    FacesContext context,
    boolean copyParent);

  /**
   * Pushes a new process scope onto the stack.
   *
   * @param context the current FacesContext
   * @param discardScope if true, the scope will be immediately destroyed.
   *   if false, the scope may be available (for back button use, for 
   *   example), but this is at the discretion of the implementation,
   *   which may aggressively destroy page flow scopes in some circumstances.
   */
  public abstract Map<String, Object> popPageFlowScope(FacesContext context, boolean discardScope);

  /**
   * Encode the page flow scope into the current URL for processing
   * in later requests.
   *
   * @param context the current FacesContext
   * @param url an URL (which may already contain query parameters)
   */
  public abstract String encodeCurrentPageFlowScopeURL(
     FacesContext context,
     String url);
}
