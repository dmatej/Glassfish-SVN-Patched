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

import java.awt.Font;

/**
 * FontProxy serves as a substitute for java.awt.Font in non-graphical
 * environments.  In particular, FontProxy can be used in "headless"
 * Unix environments, where access to a X server may not be available.
 * (AWT requires an X server connection in order to instantiate a Font.)
 * FontProxy can be used to prove font metadata without actually creating
 * an AWT Font.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/FontProxy.java#0 $) $Date: 10-nov-2005.18:58:50 $
 */
public class FontProxy
{
  /**
   * Creates a FontProxy.
   * @param name The logical name of the font
   * @param style The font style, using the java.awt.Font bitmask constants.
   * @param size he point size of the font
   */
  public FontProxy(String name, int style, int size)
  {
    _name = name;
    _style = style;
    _size = size;

    _hashCode = _hashCode();
  }

  /**
   * Creates a FontProxy from a Font
   */
  public FontProxy(Font font)
  {
    this(font.getName(), font.getStyle(), font.getSize());
  }

  /**
   * Returns the font name.
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Returns the font style
   */
  public int getStyle()
  {
    return _style;
  }

  /**
   * Returns the font size
   */
  public int getSize()
  {
    return _size;
  }

  /**
   * Gets a Font from a FontProxy.
   */
  public Font getFont()
  {
    return new Font(_name, _style, _size);
  }

  /**
   * Tests for equality.
   */
  @Override
  public boolean equals(Object o)
  {
    if (this == o)
      return true;

    if (o instanceof FontProxy)
    {
      FontProxy stub = (FontProxy)o;

      return (
        (_style == stub._style) &&
        (_size == stub._size)   &&
        (((_name == null) && (stub._name == null)) || // _name maybe null
         ((_name != null) && _name.equalsIgnoreCase(stub._name)))
        );
    }

    return false;
  }

  /**
   * Returns a hashcode for this FontProxy.
   */
  @Override
  public int hashCode()
  {
    return _hashCode;
  }

  @Override
  public String toString()
  {
    return 
     getClass().getName() + 
     "[name="  + _name    +
     ",style=" + _style   +
     ",size="  + _size    + 
     "]";
  }

  // Package-private methods for MutableFontProxy
  void __setName(String name)
  {
    _name = name;

    _hashCode = _hashCode();
  }

  void __setSize(int size)
  {
    _size = size;

    _hashCode = _hashCode();
  }

  void __setStyle(int style)
  {
    _style = style;

    _hashCode = _hashCode();
  }

  private int _hashCode()
  {
    int hashCode = _style ^ _size;

    String name = _name;

    if (name == null)
      hashCode ^= _NULL_NAME_HASH;
    else
      hashCode ^= name.toLowerCase().hashCode();
      
    return hashCode;
  }

  private String _name;
  private int    _style;
  private int    _size;

  private int    _hashCode;

  // Just some random hash code for null-name fonts
  private static final int _NULL_NAME_HASH = 1001;
}

