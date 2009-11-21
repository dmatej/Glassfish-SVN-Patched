/*
 * Copyright 1999-2004 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.collections;



/**
 * Tests the {@link org.apache.commons.collections.ProxyMap} class.
 *
 * @author <a href="mailto:dlr@finemaltcoding.com">Daniel Rall</a>
 * @since 2.0
 */
public abstract class TestProxyMap extends TestMap
{
    public TestProxyMap(String testName)
    {
        super(testName);
    }

    public static void main(String args[])
    {
        String[] testCaseName = { TestProxyMap.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    public boolean useNullKey() {
      return false;
    }

    protected ProxyMap map = null;

    public void setUp()
    {
        map = (ProxyMap) makeEmptyMap();
    }

    public void testNewMap()
    {
        assertTrue("New map is empty", map.isEmpty());
        assertEquals("New map has size zero", map.size(), 0);
    }

    public void testSearch()
    {
        map.put("first", "First Item");
        map.put("second", "Second Item");
        assertEquals("Top item is 'Second Item'", map.get("first"),
                     "First Item");
        assertEquals("Next Item is 'First Item'", map.get("second"),
                     "Second Item");
        map.clear();
    }
}
