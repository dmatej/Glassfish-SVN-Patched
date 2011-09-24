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
package org.apache.myfaces.trinidad.render;

import javax.faces.component.UIComponent;

import javax.faces.context.FacesContext;


/**
 * A producer of ClientRowKeyManagers.
 * Typically this interface will be implemented by Renderers of stamping 
 * components (like UIXCollection subclasses) that need to provide
 * string-row-keys that can be used to identify data rows on the client.
 */
public interface ClientRowKeyManagerFactory
{
  /**
   * Create a new ClientRowKeyManager for the given UIComponent
   */
  public ClientRowKeyManager createClientRowKeyManager(
    FacesContext context, 
    UIComponent component);
}
