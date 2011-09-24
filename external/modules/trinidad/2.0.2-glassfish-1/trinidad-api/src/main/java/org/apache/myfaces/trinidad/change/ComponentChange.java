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
package org.apache.myfaces.trinidad.change;

import java.io.Serializable;

import javax.faces.component.UIComponent;

/**
 * Base class for all ComponentChanges. ComponentChanges are
 * changes that act on the JSF Component Hierarchy.
 *  ComponentChanges are automatically applied during subsequent
 *  creation of the view, in the 
 *  same order in which they were added.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/ComponentChange.java#0 $) $Date: 10-nov-2005.19:09:59 $
 */
abstract public class ComponentChange implements Serializable
{
  /**
   * Apply this change to the specied component
   * @param uiComponent the UIComponent to apply the change to
   */
  public abstract void changeComponent(UIComponent uiComponent);

  private static final long serialVersionUID = 1L;
}