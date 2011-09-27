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
package org.apache.myfaces.trinidadbuild.test;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Base class for Map test cases.
 */
abstract public class MapTestCase extends TestCase
{
  public MapTestCase(String testName)
  {
    super(testName);
  }

  public void testInitialState()
  {
    Map<String, Object> map = createMap();
    assertEquals(0, map.size());
    assertTrue(map.isEmpty());
    assertTrue(map.entrySet().isEmpty());
    assertTrue(map.keySet().isEmpty());
    assertTrue(map.values().isEmpty());
    assertTrue(!map.entrySet().iterator().hasNext());
  }

  public void testPut()
  {
    Map<String, Object> map = createMap();
    assertNull(map.put("foo", "bar"));
    assertEquals(1, map.size());
    assertEquals("bar", map.get("foo"));
    assertTrue(map.containsKey("foo"));
    assertTrue(map.containsValue("bar"));

    assertTrue(map.keySet().contains("foo"));
    assertTrue(map.values().contains("bar"));

    assertEquals("bar", map.put("foo", "baz"));
    assertEquals(1, map.size());
    assertEquals("baz", map.get("foo"));

    assertTrue(map.containsKey("foo"));
    assertTrue(map.containsValue("baz"));
    assertTrue(!map.containsValue("bar"));
  }

  public void testPutAll()
  {
    Map<String, Object> map = createMap();
    HashMap<String, Object> hashMap = new HashMap<String, Object>();
    _putTwo(hashMap);
    
    map.putAll(hashMap);
    assertEquals(2, map.size());
    assertTrue(map.containsKey("first"));
    assertEquals(new Integer(1), map.get("first"));
    assertTrue(map.containsKey("second"));
    assertEquals(new Integer(2), map.get("second"));
  }


  public void testPutNull()
  {

    Map<String, Object> map = createMap();

    // Test putting a null value
    try
    {
      map.put("foo", null);
    }
    catch (NullPointerException e)
    {
      if (supportsNullValues())
        fail();
    }

    if (supportsNullValues())
    {
      if (isNullRemove())
      {
        assertEquals(0, map.size());
        assertTrue(!map.containsKey("foo"));
        assertTrue(!map.containsValue(null));
      }
      else
      {
        assertEquals(1, map.size());
        assertTrue(map.containsKey("foo"));
        assertTrue(map.containsValue(null));
      }
    }

    // Test putting a null key
    map = createMap();
    try
    {
      map.put(null, "foo");
    }
    catch (NullPointerException e)
    {
      if (supportsNullKeys())
        fail();
    }
    
    if (supportsNullKeys())
    {
      assertEquals(1, map.size());
      assertTrue(map.containsKey(null));
      assertTrue(map.containsValue("foo"));
    }
  }

  public void testEntrySet()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);

    Set<Map.Entry<String, Object>> entries = map.entrySet();
    assertEquals(2, entries.size());
    _assertIteratorSize(entries.iterator(), 2);

    Iterator<Map.Entry<String, Object>> iterator = entries.iterator();
    while (iterator.hasNext())
    {
      Map.Entry<String, Object> entry = iterator.next();
      if (entry.getKey().equals("second"))
      {
        entry.setValue(new Integer(3));
      }
      else if (entry.getKey().equals("first"))
      {
        try
        {
          iterator.remove();
        }
        catch (UnsupportedOperationException e)
        {
          if (supportsIteratorRemove())
            fail();
        }
      }
      else
      {
        fail();
      }
    }

    if (supportsIteratorRemove())
    {
      assertTrue(!map.containsKey("first"));
    }

    assertEquals(new Integer(3), map.get("second"));

    map.clear();
    assertTrue(map.isEmpty());
    assertTrue(entries.isEmpty());

    _putTwo(map);

    _assertIteratorSize(entries.iterator(), 2);
    
    assertTrue(!entries.isEmpty());
    entries.clear();

    _assertIteratorSize(entries.iterator(), 0);
    assertTrue(map.isEmpty());
    assertTrue(entries.isEmpty());
  }

  public void testEquals()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);
    assertEquals(map, map);

    Map<String, Object> secondMap = createMap();
    assertTrue(!secondMap.equals(map));
    assertTrue(!map.equals(secondMap));
    assertTrue(!map.equals(null));
    
    _putTwo(secondMap);
    assertEquals(map, secondMap);
    
    HashMap<String, Object> hashMap = new HashMap<String, Object>();
    _putTwo(hashMap);
    assertEquals(hashMap, map);
    assertEquals(map, hashMap);
  }

  public void testRemove()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);
    assertNull(map.remove("NOTTHERE"));
    assertEquals(new Integer(2), map.remove("second"));
    assertEquals(1, map.size());

    assertTrue(!map.containsKey("second"));
    assertNull(map.remove("second"));
    assertEquals(1, map.size());

    assertEquals(new Integer(1), map.remove("first"));
    assertTrue(map.isEmpty());
    assertNull(map.remove("first"));
  }

  public void testKeySet()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);
    Set<String> keys = map.keySet();
    assertEquals(2, keys.size());
    assertTrue(keys.contains("first"));
    assertTrue(keys.contains("second"));
    
    HashSet<String> hashSet = new HashSet<String>();
    hashSet.add("first");
    hashSet.add("second");
    
    assertEquals(keys, hashSet);
    assertEquals(hashSet, keys);

    hashSet.add("third");
    assertTrue(!keys.equals(hashSet));
    assertTrue(!hashSet.equals(keys));

    
    keys.clear();
    assertTrue(keys.isEmpty());
    assertTrue(map.isEmpty());
  }
  
  public void testValues()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);
    Collection<Object> values = map.values();
    assertEquals(2, values.size());
    assertTrue(values.contains(new Integer(1)));
    assertTrue(values.contains(new Integer(2)));

    // Can't really assert that this values collection is equal to 
    // any other, because we can't rely on the order of the collection
    
    values.clear();
    assertTrue(values.isEmpty());
    assertTrue(map.isEmpty());
  }



  protected boolean isNullRemove()
  {
    return false;
  }

  protected boolean supportsNullValues()
  {
    return true;
  }

  protected boolean supportsNullKeys()
  {
    return true;
  }

  protected boolean supportsIteratorRemove()
  {
    return true;
  }

  private void _assertIteratorSize(Iterator<?> iterator, int count)
  {
    for (int i = 0; i < count; i++)
    {
      assertTrue(iterator.hasNext());
      iterator.next();
    }

    assertTrue(!iterator.hasNext());
  }

  private void _putTwo(Map<String, Object> map)
  {
    map.put("first", new Integer(1));
    map.put("second", new Integer(2));
  }

  abstract protected Map<String, Object> createMap();
}
