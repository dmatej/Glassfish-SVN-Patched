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

import org.apache.myfaces.trinidad.util.Service;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


public class ServiceTest extends TestCase
{
  public static final Test suite()
  {
    return new TestSuite(ServiceTest.class);
  }
  
  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public ServiceTest(
    String testName)
  {
    super(testName);
  }

  public void testDirect()
  {
    Base b = new Base();
    Base b1 = new Base1();
    Base b2 = new Base2();

    assertEquals(b, Service.getService(b, Base.class));
    assertNull(Service.getService(b, Interface1.class));
    assertNull(Service.getService(b, Interface2.class));

    assertEquals(b1, Service.getService(b1, Base.class));
    assertEquals(b1, Service.getService(b1, Interface1.class));
    assertNull(Service.getService(b1, Interface2.class));

    assertEquals(b2, Service.getService(b2, Base.class));
    assertEquals(b2, Service.getService(b2, Interface1.class));
    assertEquals(b2, Service.getService(b2, Interface2.class));
  }

  @SuppressWarnings("cast")
  public void testProvider()
  {
    Base b = new UsesProvider();
    Base b2 = new UsesProvider2();

    assertEquals(b, Service.getService(b, Base.class));
    assert(Service.getService(b, Interface1.class) != null);
    assert(Service.getService(b, Interface1.class) instanceof Interface1);
    assertNull(Service.getService(b, Interface2.class));

    assertEquals(b2, Service.getService(b2, Base.class));
    assert(Service.getService(b2, Interface1.class) != null);
    assert(Service.getService(b2, Interface1.class) instanceof Interface1);
    assertEquals(b2, Service.getService(b2, Interface2.class));
  }

  

  static public class Base
  {
  }

  static public interface Interface1
  {
  }

  static public interface Interface2
  {
  }

  static public class Base1 extends Base implements Interface1
  {
  }

  static public class Base2 extends Base1 implements Interface2
  {
  }

  static public class UsesProvider extends Base implements Service.Provider
  {
    @SuppressWarnings("unchecked")
    public <T> T getService(Class<T> serviceClass)
    {
      if (serviceClass == Interface1.class)
        return (T)new Interface1(){};
        
      return null;
    }    
  }

  static public class UsesProvider2 extends UsesProvider implements Interface2
  {
  }
}
