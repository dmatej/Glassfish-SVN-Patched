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
package org.apache.myfaces.trinidadinternal.ui;

import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;
import org.apache.myfaces.trinidadinternal.share.xml.ParserManager;

/**
 * A UIExtension encapsulates a single bundle of functionality
 * to be added to UIX.
 * <p>
 * UIExtensions may be registered manually on LookAndFeels or
 * ParserManagers, but many developers will add extensions to a
 * LookAndFeelManager.  Developers using the UIX Servlet
 * can use its BaseUIPageBroker (or a subclass)
 *
 * @see org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager#registerUIExtension
 * @see org.apache.myfaces.trinidadinternal.uix22.servlet.ui.BaseUIPageBroker#registerUIExtension
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/UIExtension.java#0 $) $Date: 10-nov-2005.18:50:24 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface UIExtension
{
  /**
   * Callback to register parsing functionality on a ParserManager.
   */
  public void registerSelf(ParserManager manager);

  /**
   * Callback to register rendering functionality on a look-and-feel.
   */
  public void registerSelf(LookAndFeel laf);
}
