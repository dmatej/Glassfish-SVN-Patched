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
package org.apache.myfaces.trinidadinternal.image;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidad.context.LocaleContext;

import org.apache.myfaces.trinidadinternal.style.StyleContext;

/**
 * The ImageContext is used to encapsulate information about the
 * the environment in which image requests are made.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageContext.java#0 $) $Date: 10-nov-2005.19:03:53 $
 */
public interface ImageContext
{
  /**
   * Returns the end user's locale.
   */
  public LocaleContext getLocaleContext();

  /**
   * Returns the end user's Agent.
   */
  public TrinidadAgent getAgent();

  /**
   * Returns a Configuration object that will be used to
   * locate paths and return global properties.
   */
  public Configuration getConfiguration();

  /**
   * Gets a property stored on the context.
   */
  public Object getProperty(String namespace, Object key);

  /**
   * Stores a property on the context.
   */
  public void setProperty(String namespace, Object key, Object value);

  /**
   * Returns a StyleContext that can be used to look up styles while
   * rendering images with this ImageContext.
   */
  public StyleContext getStyleContext();
}
