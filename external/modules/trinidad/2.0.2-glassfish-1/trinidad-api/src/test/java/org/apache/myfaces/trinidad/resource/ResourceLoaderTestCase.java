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
package org.apache.myfaces.trinidad.resource;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

import org.apache.myfaces.trinidad.resource.ResourceLoader;

import junit.framework.TestCase;

abstract public class ResourceLoaderTestCase extends TestCase
{
  public ResourceLoaderTestCase(
    String testName)
  {
    super(testName);
  }

  protected void doTestUnknownContentLength(
    URL url) throws IOException
  {
    URLConnection conn = url.openConnection();
    long actualContentLength = conn.getContentLength();

    assertEquals("Invalid explicit content length",
                 -1L, actualContentLength);
  }
  
  protected void doTestContentLength(
    URL url) throws IOException
  {
    URLConnection conn = url.openConnection();
    long expectedContentLength = conn.getContentLength();

    if (expectedContentLength != -1)
    {
      byte[] buffer = new byte[2048];
      InputStream in = conn.getInputStream();
  
      try
      {
        long actualContentLength = 0;
    
        int length;
        while ((length = (in.read(buffer))) >= 0)
        {
          actualContentLength += length;
        }
  
        assertEquals("Inaccurate explicit content length",
                     expectedContentLength, actualContentLength);
      }
      finally
      {
        in.close();
      }
    }
  }

  public class LocalResourceLoader extends ResourceLoader
  {
    @Override
    protected URL findResource(
      String name
      ) throws IOException
    {
      return getClass().getResource(name);
    }
  }
}
