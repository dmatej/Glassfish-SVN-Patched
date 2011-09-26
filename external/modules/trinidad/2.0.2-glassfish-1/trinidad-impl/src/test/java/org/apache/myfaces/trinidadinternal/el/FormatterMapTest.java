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
package org.apache.myfaces.trinidadinternal.el;

import java.util.Map;

import junit.framework.TestCase;

/**
 * Unit tests for FormatterMap.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/test/java/oracle/adfinternal/view/faces/el/FormatterMapTest.java#1 $) $Date: 16-aug-2005.15:15:39 $
 */
public class FormatterMapTest extends TestCase
{
  public FormatterMapTest(
    String testName)
  {
    super(testName);
  }

  public void testFormatterMap()
  {
    Map<Object, Map<Object, String>> test = FormatterMap.sharedInstance();
    Map<Object, String> subMap = test.get("Foo{0}Bar");
    Object result = subMap.get("Test");
    assertEquals(result, "FooTestBar");

    result = subMap.get(null);
    assertEquals(result, "FooBar");

    result = subMap.get("");
    assertEquals(result, "FooBar");

    subMap = test.get("FooBar");
    result = subMap.get("Test");
    assertEquals(result, "FooBar");

    result = subMap.get(null);
    assertEquals(result, "FooBar");

    result = subMap.get("");
    assertEquals(result, "FooBar");

    subMap = test.get(null);
    result = subMap.get("Test");
    assertEquals(result, null);

    result = subMap.get(null);
    assertEquals(result, null);

    result = subMap.get("");
    assertEquals(result, null);
    
  }
}
