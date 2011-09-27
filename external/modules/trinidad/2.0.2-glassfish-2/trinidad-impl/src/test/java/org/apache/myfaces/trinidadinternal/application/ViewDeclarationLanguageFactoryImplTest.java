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
package org.apache.myfaces.trinidadinternal.application;

import junit.framework.Test;
import junit.framework.TestSuite;

import javax.faces.component.UIViewRoot;
import javax.faces.render.RenderKitFactory;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;
import org.apache.myfaces.trinidadinternal.renderkit.RenderKitBootstrap;

public class ViewDeclarationLanguageFactoryImplTest extends FacesTestCase
{
  public ViewDeclarationLanguageFactoryImplTest(
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
    return new TestSuite(ViewDeclarationLanguageFactoryImplTest.class);
  }

  public void testInternalView() throws Throwable
  {
    ViewDeclarationLanguageFactoryImpl vdlf = 
        new ViewDeclarationLanguageFactoryImpl(new NullViewDeclarationLanguageFactory());
    RenderKitBootstrap.setFactories(null);
    try
    {
      UIViewRoot viewRoot = new UIViewRoot();
      String viewId = "/testURL";
      viewRoot.setViewId(viewId);
      viewRoot.setRenderKitId(RenderKitFactory.HTML_BASIC_RENDER_KIT);
      facesContext.setViewRoot(viewRoot);
      vdlf.getViewDeclarationLanguage(viewId).renderView(facesContext, viewRoot);
      assertEquals("render", __internalViewCalled);

      vdlf.getViewDeclarationLanguage(viewId).restoreView(facesContext, viewId);
      assertEquals("restore", __internalViewCalled);

      vdlf.getViewDeclarationLanguage(viewId).createView(facesContext, viewId);
      assertEquals("create", __internalViewCalled);
    }
    finally 
    {
      __internalViewCalled = null;
      RenderKitBootstrap.clearFactories();
      facesContext.release();
    }
  }
  
  static String __internalViewCalled = null;
}
