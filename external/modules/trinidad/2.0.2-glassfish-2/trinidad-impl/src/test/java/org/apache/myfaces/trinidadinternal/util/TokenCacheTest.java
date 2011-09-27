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
package org.apache.myfaces.trinidadinternal.util;

import junit.framework.TestCase;

import java.util.Map;
import java.util.HashMap;

/**
 * Test of TokenCache.
 * NOTE: Calling TokenCache.isAvailable() effectively bumps
 * an item up to the top of the most-recently-used list.
 * So the calls to assertTrue(cache.isAvailable(...)) are
 * not without side-effects, and you can in fact break
 * the test by removing or changing the order of assert calls!
 */
public class TokenCacheTest extends TestCase
{
  public TokenCacheTest(String testName)
  {
    super(testName);
  }

  public void testBasic()
  {
    HashMap<String, Object> map = new HashMap<String, Object>();
    // Add a pre-existing key to the test to verify that it is untouched
    // Slight assumption here that this key would never get hit
    map.put("this-would-never-be-used", 17);

    TokenCache cache = new TokenCache(2);

    // Add first value to cache
    String token1 = cache.addNewEntry(1, map);
    assertEquals(Integer.valueOf(1), map.get(token1));
    assertEquals(2, map.size());
    assertTrue(cache.isAvailable(token1));

    // Add second value to cache
    String token2 = cache.addNewEntry(2, map);
    assertEquals(Integer.valueOf(1), map.get(token1));
    assertEquals(Integer.valueOf(2), map.get(token2));
    assertEquals(3, map.size());
    assertTrue(cache.isAvailable(token1));
    assertTrue(cache.isAvailable(token2));

    // Add third value to cache - first value is now gone
    String token3 = cache.addNewEntry(3, map);
    assertNull(map.get(token1));
    assertEquals(Integer.valueOf(2), map.get(token2));
    assertEquals(Integer.valueOf(3), map.get(token3));
    assertEquals(3, map.size());
    assertFalse(cache.isAvailable(token1));
    assertTrue(cache.isAvailable(token2));
    assertTrue(cache.isAvailable(token3));

    // Remove the third value from the cache
    Object removed = cache.removeOldEntry(token3, map);
    assertEquals(2, map.size());
    assertFalse(cache.isAvailable(token3));
    assertEquals(Integer.valueOf(3), removed);
    assertEquals(Integer.valueOf(17), map.get("this-would-never-be-used"));
    
    // Clear the cache - only the non-cache value remains
    cache.clear(map);
    assertEquals(1, map.size());
    assertEquals(Integer.valueOf(17), map.get("this-would-never-be-used"));
  }

  public void testIsAvailableAffectsOrder()
  {
    HashMap<String, Object> map = new HashMap<String, Object>();
    TokenCache cache = new TokenCache(2);
    String token1 = cache.addNewEntry(1, map);
    String token2 = cache.addNewEntry(2, map);
    // Check the availability of token1, which means that it
    // is now more recently used than token2
    assertTrue(cache.isAvailable(token1));

    // Add token3:  we now have 1 and 3, not 2 and 3
    String token3 = cache.addNewEntry(3, map);
    assertTrue(cache.isAvailable(token1));
    assertFalse(cache.isAvailable(token2));
    assertTrue(cache.isAvailable(token3));
  }

  public void testPinned()
  {
    HashMap<String, Object> map = new HashMap<String, Object>();
    // Add a pre-existing key to the test to verify that it is untouched
    // Slight assumption here that this key would never get hit
    map.put("this-would-never-be-used", 17);

    TokenCache cache = new TokenCache(2);

    // Add first value to cache
    String token1 = cache.addNewEntry(1, map);
    // Add second value to cache, but pin it to token1
    String token2 = cache.addNewEntry(2, map, token1);

    assertEquals(Integer.valueOf(1), map.get(token1));
    assertEquals(Integer.valueOf(2), map.get(token2));
    assertEquals(3, map.size());
    assertTrue(cache.isAvailable(token1));
    assertTrue(cache.isAvailable(token2));

    // Add third value to cache - first value should still be present
    // because it is pinned by token 2
    String token3 = cache.addNewEntry(3, map);
    assertEquals(Integer.valueOf(1), map.get(token1));
    assertEquals(Integer.valueOf(2), map.get(token2));
    assertEquals(Integer.valueOf(3), map.get(token3));
    assertEquals(4, map.size());
    assertTrue(cache.isAvailable(token1));
    assertTrue(cache.isAvailable(token2));
    assertTrue(cache.isAvailable(token3));

    // Remove the second value from the cache, which
    // should remove the first value as well
    Object removed = cache.removeOldEntry(token2, map);
    assertEquals(2, map.size());
    assertFalse(cache.isAvailable(token1));
    assertFalse(cache.isAvailable(token2));
    assertTrue(cache.isAvailable(token3));
    assertEquals(Integer.valueOf(2), removed);
    assertEquals(Integer.valueOf(3), map.get(token3));
    assertEquals(Integer.valueOf(17), map.get("this-would-never-be-used"));
    
    // Clear the cache - only the non-cache value remains
    cache.clear(map);
    assertEquals(1, map.size());
    assertEquals(Integer.valueOf(17), map.get("this-would-never-be-used"));
  }

  public void testRecursivePinned()
  {
    HashMap<String, Object> map = new HashMap<String, Object>();
    TokenCache cache = new TokenCache(2);

    // Add first value to cache
    String token1 = cache.addNewEntry(1, map);
    // Add second value to cache, but pin it to token1
    String token2 = cache.addNewEntry(2, map, token1);
    // Add third value to cache, pinned to token2
    String token3 = cache.addNewEntry(3, map, token2);
    // Add fourth value to cache, pinned to token3
    String token4 = cache.addNewEntry(4, map, token3);
    assertEquals(4, map.size());

    // Add fifth value to cache:  all should still be present,
    // because 4 and 5 are the last two, and 3, 2, and 1 stay pinned
    String token5 = cache.addNewEntry(5, map);
    assertEquals(5, map.size());
    assertTrue(cache.isAvailable(token1));
    assertTrue(map.containsKey(token1));
    assertTrue(cache.isAvailable(token2));
    assertTrue(map.containsKey(token2));
    assertTrue(cache.isAvailable(token3));
    assertTrue(map.containsKey(token3));
    assertTrue(cache.isAvailable(token4));
    assertTrue(map.containsKey(token4));
    assertTrue(cache.isAvailable(token5));
    
    // Add sixth value to cache:  only two should now be present
    String token6 = cache.addNewEntry(6, map);
    assertFalse(cache.isAvailable(token1));
    assertFalse(map.containsKey(token1));
    assertFalse(cache.isAvailable(token2));
    assertFalse(map.containsKey(token2));
    assertFalse(cache.isAvailable(token3));
    assertFalse(map.containsKey(token3));
    assertFalse(cache.isAvailable(token4));
    assertFalse(map.containsKey(token4));
    assertTrue(cache.isAvailable(token5));
    assertTrue(cache.isAvailable(token6));
    assertEquals(2, map.size());
  }

  public void testMultiplePinned()
  {
    HashMap<String, Object> map = new HashMap<String, Object>();
    TokenCache cache = new TokenCache(2);

    // Add first value to cache
    String token1 = cache.addNewEntry(1, map);
    // Add second value to cache, but pin it to token1
    String token2 = cache.addNewEntry(2, map, token1);
    // Add third value to cache, pinned to token1.
    // All will be present, because 2 and 3 are the last
    // two, but 1 is pinned.
    String token3 = cache.addNewEntry(3, map, token1);
    assertEquals(3, map.size());
    assertTrue(cache.isAvailable(token1));
    assertTrue(map.containsKey(token1));
    assertTrue(cache.isAvailable(token2));
    assertTrue(map.containsKey(token2));
    assertTrue(cache.isAvailable(token3));
    assertTrue(map.containsKey(token3));

    // Add fourth value to cache, pinned to token1.
    // Now should have 1, 3, and 4.
    String token4 = cache.addNewEntry(4, map, token1);

    assertEquals(3, map.size());
    assertTrue(cache.isAvailable(token1));
    assertTrue(map.containsKey(token1));
    assertFalse(cache.isAvailable(token2));
    assertFalse(map.containsKey(token2));
    assertTrue(cache.isAvailable(token3));
    assertTrue(map.containsKey(token3));
    assertTrue(cache.isAvailable(token4));
    assertTrue(map.containsKey(token4));

    // Add fifth value to cache, unpinned:  now should have 1, 4 and 5
    String token5 = cache.addNewEntry(5, map);
    assertEquals(3, map.size());

    assertTrue(cache.isAvailable(token1));
    assertTrue(map.containsKey(token1));
    assertFalse(cache.isAvailable(token2));
    assertFalse(map.containsKey(token2));
    assertFalse(cache.isAvailable(token3));
    assertFalse(map.containsKey(token3));
    assertTrue(cache.isAvailable(token4));
    assertTrue(map.containsKey(token4));
    assertTrue(cache.isAvailable(token5));

    // Add sixth value, unpinned.  Should now only have 5 and 6
    String token6 = cache.addNewEntry(6, map);
    assertFalse(cache.isAvailable(token1));
    assertFalse(map.containsKey(token1));
    assertFalse(cache.isAvailable(token2));
    assertFalse(map.containsKey(token2));
    assertFalse(cache.isAvailable(token3));
    assertFalse(map.containsKey(token3));
    assertFalse(cache.isAvailable(token4));
    assertFalse(map.containsKey(token4));
    assertTrue(cache.isAvailable(token5));
    assertTrue(cache.isAvailable(token6));
  }

  public void testPinnedInTinyCache()
  {
    HashMap<String, Object> map = new HashMap<String, Object>();
    TokenCache cache = new TokenCache(1);
    // Verify that adding a token that would have flushed an entry
    // won't do so as long as it's pinning that entry
    String token1 = cache.addNewEntry(1, map);
    String token2 = cache.addNewEntry(2, map, token1);
    String token3 = cache.addNewEntry(3, map, token2);
    assertTrue(cache.isAvailable(token1));
    assertTrue(cache.isAvailable(token2));    
    assertTrue(cache.isAvailable(token3));    
    assertEquals(3, map.size());
  }
}
