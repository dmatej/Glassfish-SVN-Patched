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
package org.apache.myfaces.trinidad.bean;

import org.apache.myfaces.trinidad.bean.PropertyKey;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Tests the PropertyKey class.
 */
public class PropertyKeyTest extends TestCase
{
  /**
   * Returns a test suite of all PropertyKey tests.
   */
  public static final Test suite()
  {
    return new TestSuite(PropertyKeyTest.class);
  }
  

  /**
   * Tests that property keys with primitive types have the corresponding
   * Java Language Specification default value (boxed).
   */
  public void testPrimitiveDefaults()
  {
    PropertyKey key;
    
    key = new PropertyKey("boolean", boolean.class);
    assertSame(Boolean.FALSE, key.getDefault());
    key = new PropertyKey("byte", byte.class);
    assertEquals(new Byte((byte)0), key.getDefault());
    key = new PropertyKey("char", char.class);
    assertEquals(new Character('\0'), key.getDefault());
    key = new PropertyKey("double", double.class);
    assertEquals(new Double(0.0), key.getDefault());
    key = new PropertyKey("float", float.class);
    assertEquals(new Float(0.0f), key.getDefault());
    key = new PropertyKey("int", int.class);
    assertEquals(new Integer(0), key.getDefault());
    key = new PropertyKey("long", long.class);
    assertEquals(new Long(0L), key.getDefault());
    key = new PropertyKey("short", short.class);
    assertEquals(new Short((short)0), key.getDefault());
  }

  /**
   * Tests that property keys with boxed primitive types still have null
   * as the default, rather than the boxed primitive default.
   */
  public void testBoxedPrimitiveDefaults()
  {
    PropertyKey key;
    
    key = new PropertyKey("Boolean", Boolean.class);
    assertNull(key.getDefault());
    key = new PropertyKey("Byte", Byte.class);
    assertNull(key.getDefault());
    key = new PropertyKey("Character", Character.class);
    assertNull(key.getDefault());
    key = new PropertyKey("Double", Double.class);
    assertNull(key.getDefault());
    key = new PropertyKey("Float", Float.class);
    assertNull(key.getDefault());
    key = new PropertyKey("Integer", Integer.class);
    assertNull(key.getDefault());
    key = new PropertyKey("Long", Long.class);
    assertNull(key.getDefault());
    key = new PropertyKey("Short", Short.class);
    assertNull(key.getDefault());
  }

  public void testDefaultSameType()
  {
    new PropertyKey("String", String.class, "default");
  }

  public void testDefaultSubType()
  {
    new PropertyKey("Number", Number.class, new Integer(101));
  }

  public void testDefaultWrongType()
  {
    try
    {
      new PropertyKey("Long", Long.class, new Integer(101));
      fail();
    }
    catch (IllegalStateException e)
    {
      // expected
    }
  }
}
