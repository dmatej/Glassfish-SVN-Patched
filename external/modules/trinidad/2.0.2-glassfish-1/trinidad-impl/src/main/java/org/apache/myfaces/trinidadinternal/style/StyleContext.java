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
package org.apache.myfaces.trinidadinternal.style;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.style.Styles;

/**
 * The StyleContext interface is used to provide information
 * about the target end user environment.  It also provides
 * access to general-purpose facilities.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/StyleContext.java#0 $) $Date: 10-nov-2005.18:57:57 $
 */
public interface StyleContext
{
  /**
   * Returns the end user's locale.
   */
  public LocaleContext getLocaleContext();

  /**
   * Returns the end user's Agent.
   */
  public TrinidadAgent getAgent();

  public String getGeneratedFilesPath();
  public boolean checkStylesModified();

  public boolean disableStandardsMode();

  public StyleProvider getStyleProvider();
  public StyleProvider getStyleProvider(boolean recompute);
  public Styles getStyles();
  public AccessibilityProfile getAccessibilityProfile();
  public boolean isPortletMode();
  public boolean isDisableStyleCompression();
  public boolean isDirty();
  
  /**
   * @return true if the current request is secure (an https request), false otherwise
   */
  public boolean isRequestSecure();
}
