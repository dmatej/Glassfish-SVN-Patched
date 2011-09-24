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

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class TypeTest extends TestCase
{
  public static final Test suite()
  {
    return new TestSuite(TypeTest.class);
  }
  
  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public TypeTest(
    String testName)
  {
    super(testName);
  }

  @Override
  public void setUp()
  {
    _type = new FacesBean.Type();
    _fooKey = _type.registerKey("foo",
                                PropertyKey.CAP_TRANSIENT);
    _barKey = _type.registerKey("bar",
                                PropertyKey.CAP_LIST);

    _fooAliasKey = _type.registerAlias(_fooKey, "fooAlias");
    
    _subtype = new FacesBean.Type(_type);
    _bazKey = _subtype.registerKey("baz");
  }

  @Override
  public void tearDown()
  {
    _type = null;
  }

  public void testKeyEquality()
  {
    assertTrue(_fooKey.equals(_fooKey));
    assertTrue(_barKey.equals(_barKey));
    assertTrue(!_fooKey.equals(_barKey));
    assertTrue(!_barKey.equals(_fooKey));
    assertTrue(!_fooKey.equals(null));
    assertTrue(!_fooKey.equals("foo"));
    assertTrue(!"foo".equals(_fooKey));
  }

  public void testKeyCapabilities()
  {
    assertTrue(_fooKey.isTransient());
    assertTrue(_fooKey.getSupportsBinding());
    assertTrue(!_fooKey.isList());

    assertTrue(!_barKey.isTransient());
    assertTrue(!_barKey.getSupportsBinding());
    assertTrue(_barKey.isList());
  }

  public void testFindKeyByName()
  {
    PropertyKey foo = _type.findKey("foo");
    assertSame(foo, _fooKey);
    assertEquals("foo", foo.getName());
    
    PropertyKey bar = _type.findKey("bar");
    assertSame(bar, _barKey);
    assertEquals("bar", bar.getName());

    PropertyKey baz = _type.findKey("baz");
    assertNull(baz);

    baz = _subtype.findKey("baz");
    assertSame(baz, _bazKey);
  }

  public void testFindKeyByIndex()
  {
    PropertyKey foo = _type.findKey(0);
    assertSame(foo, _fooKey);
    assertEquals(0, foo.getIndex());
    
    PropertyKey bar = _type.findKey(1);
    assertSame(bar, _barKey);
    assertEquals(1, bar.getIndex());

    PropertyKey baz = _type.findKey(2);
    assertNull(baz);

    baz = _type.findKey(-1);
    assertNull(baz);
  }

  public void testTypeModifications()
  {
    PropertyKey newKey = _subtype.registerKey("new");
    assertNotNull(newKey);

    assertEquals(3, newKey.getIndex());
    assertEquals("new", newKey.getName());

    // Verify that re-registering a key fails
    try
    {
      _subtype.registerKey("new");
      fail();
    }
    catch (IllegalStateException ise)
    {
    }

    
    // Verify that we didn't overwrite anything
    assertSame(newKey, _subtype.findKey("new"));
    assertSame(newKey, _subtype.findKey(3));
    
    // Lock the type
    _subtype.lock();

    // And register a "reallyNew" key
    try
    {
      _subtype.registerKey("reallyNew");
      fail();
    }
    catch (IllegalStateException ise)
    {
    }

    // The key should not be be present
    assertNull(_subtype.findKey("reallyNew"));
    assertNull(_subtype.findKey(4));
  }

  public void testAlias()
  {
    assertSame(_fooKey, _fooAliasKey);
    assertSame(_fooKey, _type.findKey("fooAlias"));
  }

  private FacesBean.Type _type;
  private FacesBean.Type _subtype;
  private PropertyKey  _fooKey;
  private PropertyKey  _barKey;
  private PropertyKey  _bazKey;
  private PropertyKey  _fooAliasKey;
}
