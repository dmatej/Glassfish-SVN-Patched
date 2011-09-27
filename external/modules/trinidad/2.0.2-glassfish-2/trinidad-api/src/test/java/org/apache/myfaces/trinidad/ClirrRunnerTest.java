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
package org.apache.myfaces.trinidad;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class ClirrRunnerTest extends TestCase
{
  /**
   * Returns a test suite of all PropertyKey tests.
   */
  public static final Test suite()
  {
    return new TestSuite(ClirrRunnerTest.class);
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


  public void testBinaryCompatibility()
  {
    
    try
    {
      String golden = System.getProperty("trinidad.clirr.golden");
      String clirrRunResult = System.getProperty("trinidad.clirr.compare");

      String goldenFileContent = FileUtils.readFileToString(new File(golden));
      String clirrRunFileContent = FileUtils.readFileToString(new File(clirrRunResult));
      
      String binaryIssue = StringUtils.difference(goldenFileContent, clirrRunFileContent);
      
      if (binaryIssue.length() > 0)
      {
        fail("Check your binary compatibility! - See output in file: "  + clirrRunResult);
      }
    }
    catch (IOException e)
    {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }
}
