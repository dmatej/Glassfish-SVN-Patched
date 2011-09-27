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
package org.apache.myfaces.trinidadinternal.metadata;

import java.io.IOException;

import java.net.URL;

import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import javax.faces.context.FacesContext;

import junit.framework.Test;
import junit.framework.TestSuite;

import junit.textui.TestRunner;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;
import org.apache.myfaces.trinidadinternal.metadata.RegionMetadata.AttributeMetaData;
import org.apache.myfaces.trinidadinternal.metadata.RegionMetadata.ComponentMetaData;

/**
 * Test for parsing region-metadata.xml
 */
public class RegionMetadataTest extends FacesTestCase
{
  public RegionMetadataTest(
    String testName)
  {
    super(testName);
  }
  
  @Override
  protected void setUp() throws Exception
  {
    super.setUp();
  }
  
  @Override
  protected void tearDown() throws Exception
  {
    super.tearDown();
  }
  
  public static Test suite()
  {
    return new TestSuite(RegionMetadataTest.class);
  }

  // test loading the region-metadata from the classpath:
  public void testParsingRegionMetadataFromClassLoader()
  {
    Thread t = Thread.currentThread();
    ClassLoader loader = t.getContextClassLoader();
    try
    {
      t.setContextClassLoader(new RegionClassLoader(loader));
  
  
      _testRegionMetadata(facesContext);
  
    }
    finally
    {
      t.setContextClassLoader(loader);
    }
  }

  private void _testRegionMetadata(FacesContext context)
  {
    RegionMetadata rmd = RegionMetadata.getRegionMetadata(context);

    ComponentMetaData comp = 
      (ComponentMetaData) rmd.getRegionConfig("org.apache.myfaces.trinidad.view.test.TestRegion1");
    assertEquals("/regions/testRegion1.jspx", comp.getJspUIDef());
    List<AttributeMetaData> attrs = comp.getAttributes();
    assertEquals(2, attrs.size());

    AttributeMetaData attr = attrs.get(0);
    assertEquals("stock", attr.getAttrName());
    assertEquals(Integer.class, attr.getAttrClass());
    assertTrue(attr.isRequired());
    assertNull(attr.getDefaultValue());

    attr = attrs.get(1);
    assertEquals("desc", attr.getAttrName());
    assertEquals(String.class, attr.getAttrClass());
    assertFalse(attr.isRequired());
    assertEquals("Test", attr.getDefaultValue());

    comp = (ComponentMetaData) rmd.getRegionConfig("org.apache.myfaces.trinidad.view.test.TestRegion2");
    assertEquals("/regions/testRegion2.jspx", comp.getJspUIDef());
    attrs = comp.getAttributes();
    assertEquals(0, attrs.size());

    comp = (ComponentMetaData) rmd.getRegionConfig("org.apache.myfaces.trinidad.view.test.TestRegion3");
    assertEquals("/regions/testRegion3.jspx", comp.getJspUIDef());
    attrs = comp.getAttributes();
    assertEquals(0, attrs.size());
  }

  public static void main(String[] args)
  {
    TestRunner.run(RegionMetadataTest.class);
  }

  static private class RegionClassLoader extends ClassLoader
  {
    public RegionClassLoader(
      ClassLoader parent)
    {
      super(parent);
    }
    
    @Override
    public Enumeration<URL> findResources(String name) throws IOException
    {
      if (RegionMetadata.__CONFIG_FILE_OTHER == name)
      {
        URL url = this.getClass().getResource(_TEST_FILE);
        assertNotNull("resource does not exist:"+_TEST_FILE, url);
        URL url2 = this.getClass().getResource(_TEST_FILE2);
        assertNotNull("resource does not exist:"+_TEST_FILE2, url2);
        URL[] resources = new URL[] {url, url2};
        return Collections.enumeration(Arrays.asList(resources));
      }
      return super.findResources(name);
    }
  }

  private static final String _TEST_FILE = "region-metadata-test.xml";
  private static final String _TEST_FILE2 = "region-metadata-test2.xml";
}
