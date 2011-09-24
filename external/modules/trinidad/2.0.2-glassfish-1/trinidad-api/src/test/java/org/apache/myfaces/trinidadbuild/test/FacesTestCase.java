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
package org.apache.myfaces.trinidadbuild.test;

import java.io.IOException;

import java.util.List;

import javax.faces.FactoryFinder;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;

import org.apache.shale.test.jmock.AbstractJmockJsfTestCase;
import org.apache.shale.test.mock.MockFacesContext;
import org.apache.shale.test.mock.MockRenderKitFactory;
import org.jmock.Mock;

/**
 * Base class for JavaServer Faces unit tests.
 * Acts as a wrapper class to <code>AbstractJsfTestCase</code>
 * 
 */
public class FacesTestCase extends AbstractJmockJsfTestCase
{
  /**
   * Creates a new FacesTestCase.
   *
   * @param testName  the unit test name
   */
  public FacesTestCase(
    String testName)
  {
    super(testName);
  }

  @Override
  protected void setUp() throws Exception
  {
    super.setUp();
    FacesContext oldFacesContext = facesContext;
    UIViewRoot oldViewRoot = oldFacesContext.getViewRoot();
    oldFacesContext.release();
    facesContext = new MockFacesContext12(externalContext,
                                          lifecycle,
                                          application);
    facesContext.setViewRoot(oldViewRoot);
    facesContext.setApplication(application);

    facesContext.getViewRoot().setRenderKitId("org.apache.myfaces.trinidad.core"); 
    RenderKitFactory renderKitFactory = (RenderKitFactory)
    FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);
    Mock mockRenderKitty = mock(RenderKit.class);
    RenderKit renderKit = (RenderKit) mockRenderKitty.proxy();
    _mockRenderKit = new MockRenderKitWrapper(mockRenderKitty, renderKit);
    renderKitFactory.addRenderKit("org.apache.myfaces.trinidad.core", renderKit);
  }

  @Override
  protected void tearDown() throws Exception
  {
    super.tearDown();
  }

  /**
   * Configures the FactoryFinder RenderKitFactory implementation class.
   *
   * @param renderkitFactoryClass  the render kit factory class
   */
  protected RenderKitFactory setupRenderKitFactory(
    Class<? extends RenderKitFactory> renderkitFactoryClass)
  {

    FactoryFinder.setFactory(FactoryFinder.RENDER_KIT_FACTORY,
                             renderkitFactoryClass.getName());
    return (RenderKitFactory)
      FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);
  }

  /**
   * Configures the FactoryFinder MockRenderKitFactory implementation class.
   * @TODO rename the class.
   * @TODO move to Shale.
   */
  protected MockRenderKitFactory setupMockRenderKitFactory()
  {
    return (MockRenderKitFactory)
      setupRenderKitFactory(MockRenderKitFactory.class);
  }


  /**
   * Renders the component tree.
   *
   * @param context    the faces context
   * @param component  the component tree to render
   *
   * @throws IOException  when the render fails
   */
  @SuppressWarnings("unchecked")
  protected void doRenderResponse(
    FacesContext context,
    UIComponent  component) throws IOException
  {
    component.encodeBegin(context);
    if (component.getRendersChildren())
    {
      component.encodeChildren(context);
    }
    else
    {
      for(UIComponent child : (List<UIComponent>)component.getChildren())
      {
        doRenderResponse(context, child);
      }
    }
    component.encodeEnd(context);
  }

  public void setCurrentContext(
      FacesContext context)
  {
    TestFacesContext.setCurrentInstance(context);
  }
  
  private MockRenderKitWrapper _mockRenderKit = null;
  
  public MockRenderKitWrapper getMockRenderKitWrapper()
  {
    return _mockRenderKit;
  }

  /**
   * @todo add this code to MockFacesContext
   */
  public static abstract class TestFacesContext extends MockFacesContext
  {
    public static void setCurrentInstance(
      FacesContext context)
    {
      FacesContext.setCurrentInstance(context);
    }
  }
}