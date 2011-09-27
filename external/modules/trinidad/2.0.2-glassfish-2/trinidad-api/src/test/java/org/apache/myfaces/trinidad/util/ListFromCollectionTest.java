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
package org.apache.myfaces.trinidad.util;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.util.TreeSet;
import java.util.List;

import org.apache.myfaces.trinidad.util.ListFromCollection;

public class ListFromCollectionTest extends TestCase
{
  public static final Test suite()
  {
    return new TestSuite(ListFromCollectionTest.class);
  }
  
  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public ListFromCollectionTest(
    String testName)
  {
    super(testName);
  }

  public void testGet()
  {
    TreeSet<Integer> tree = new TreeSet<Integer>();
    for (int i = 0; i < 250; i++)
      tree.add(new Integer(i));

    ListFromCollection lfc = new ListFromCollection();
    lfc.setSize(100);

    List<?> list = lfc.getList().get(tree);
    assertEquals(tree.size(), list.size());
    assertEquals(new Integer(5), list.get(5));
    assertEquals(new Integer(155), list.get(155));
    assertEquals(new Integer(0), list.get(0));
    assertEquals(new Integer(99), list.get(99));
    assertEquals(new Integer(100), list.get(100));
    
    try
    {
      list.get(-1);
      fail();
    }          
    catch (IndexOutOfBoundsException ioobe)
    {
    }

    try
    {
      list.get(250);
      fail();
    }          
    catch (IndexOutOfBoundsException ioobe)
    {
    }
  }
}
