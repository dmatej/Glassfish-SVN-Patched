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
package org.apache.myfaces.trinidadinternal.style.util;


/**
 * MutableFontProxy is a FontProxy subclass which allows FontProxy values
 * to be modified.  This class is provided to allow clients (such as the
 * UIX Components Renderers) to reuse a single FontProxy object to
 * represent different fonts over a short period of time.  Most clients
 * should use the immutable FontProxy class.
 *
 * @see org.apache.myfaces.trinidadinternal.style.util.FontProxy
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/MutableFontProxy.java#0 $) $Date: 10-nov-2005.18:58:51 $
 */
public class MutableFontProxy extends FontProxy
{
  /**
   * Creates a MutableFontProxy.
   * @param name The logical name of the font
   * @param style The font style, using the java.awt.Font bitmask constants.
   * @param size he point size of the font
   */
  public MutableFontProxy(String name, int style, int size)
  {
    super(name, style, size);
  }

  /**
   * Sets the name of the font.
   */
  public void setName(String name)
  {
    __setName(name);
  }

  /**
   * Sets the size of the font
   */
  public void setSize(int size)
  {
    __setSize(size);
  }

  /**
   * Sets the style of the font, using AWT Font style flags.
   */
  public void setStyle(int style)
  {
    __setStyle(style);
  }

}

