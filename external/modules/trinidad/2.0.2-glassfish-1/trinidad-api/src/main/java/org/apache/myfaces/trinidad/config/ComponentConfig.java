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
package org.apache.myfaces.trinidad.config;

/**
 * This class represents the metadata of a component definition in
 * a faces-config type file.
 */
public abstract class ComponentConfig 
{
  protected ComponentConfig()
  {
  }
  
  /**
   * Gets the description of this component
   */
  public abstract String getDescription();
  
  /**
   * Gets the display-name of this component
   */
  public abstract String getDisplayName();
  
  /**
   * Gets the component-type of this component
   */
  public abstract String getComponentType();
  
}
