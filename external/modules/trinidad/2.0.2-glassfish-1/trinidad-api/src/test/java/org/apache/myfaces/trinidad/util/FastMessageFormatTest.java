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

import org.apache.myfaces.trinidad.util.FastMessageFormat;

public class FastMessageFormatTest
  extends TestCase
{
  public static final Test suite()
  {
    return new TestSuite(FastMessageFormatTest.class);
  }

  public static void main(String[] args)
    throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public FastMessageFormatTest(String testName)
  {
    super(testName);
  }

  public void testGet()
  {
    // {0} and {1} should be replaced.
    // Param for {2} is null, so remove {2}.
    // The rest is interpreted literally.
    // Expected result: "beef {{3} isn't {} {a} {12a}kosher {"
    FastMessageFormat fmf =
      new FastMessageFormat("{0} {{3} isn't {} {a} {12a}{2}{1} {");
    String[] params = { "beef", "kosher", null };
    String result = fmf.format(params);
    assertEquals(result, "beef {{3} isn't {} {a} {12a}kosher {");
  }
}

