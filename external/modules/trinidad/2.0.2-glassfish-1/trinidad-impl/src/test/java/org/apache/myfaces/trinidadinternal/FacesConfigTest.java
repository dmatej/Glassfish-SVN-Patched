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
package org.apache.myfaces.trinidadinternal;

public class FacesConfigTest extends XMLValidityTestCase
{
  public FacesConfigTest(
    String testName)
  {
    super(testName);
  }

  /* DISABLE THIS TEST: we cannot redistribute the web-facesconfig_1_1.dtd
     without a proper license.  Once in MyFaces Apache, we should be
     able to refer to the MyFaces copy of the DTD for this test */
  /* But now in 1.2, we have to disable it again - because we need
     to use Schema Validation. */
  public void testFacesConfigValidity() throws Throwable
  {
    /*
    URL dtdSource = getClass().getResource("web-facesconfig_1_1.dtd");
    String publicID = 
      "-//Sun Microsystems, Inc.//DTD JavaServer Faces Config 1.1//EN";
    URL configSource = getClass().getResource("/META-INF/faces-config.xml");
    executeValidityTest(dtdSource,
                        publicID,
                        configSource); 
    */
  }
}
