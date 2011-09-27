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
package org.apache.myfaces.trinidadinternal.renderkit;

import junit.framework.TestCase;

import org.xml.sax.SAXException;

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.component.UIViewRoot;
import javax.faces.render.RenderKit;

import org.apache.myfaces.trinidad.component.core.CoreDocument;
import org.apache.myfaces.trinidad.component.core.CoreForm;
import org.apache.myfaces.trinidad.render.ExtendedRenderKitService;
import org.apache.myfaces.trinidad.util.Service;

import org.apache.myfaces.trinidadinternal.io.XhtmlResponseWriter;



public class RenderKitPerfTestCase extends TestCase
{
  public RenderKitPerfTestCase() throws IOException, SAXException
  {
  }

  public RenderKitPerfTestCase(String testName) throws IOException, SAXException
  {
    super(testName);
  }



  protected void renderRoot(UIViewRoot root) throws IOException
  {
    _facesContext.setViewRoot(root);
  
    ExtendedRenderKitService service =
      _getExtendedRenderKitService(_facesContext);
    if (service != null)
      service.encodeBegin(_facesContext);

    try
    {
      RenderUtils.encodeRecursive(_facesContext, root);
      if (service != null)
        service.encodeEnd(_facesContext);
    }
    finally
    {
      if (service != null)
        service.encodeFinally(_facesContext);
    }

    _facesContext.setViewRoot(null);
  }
   
  protected UIViewRoot createTestTree(UIComponent compToTest, String testName)
    throws IOException
  {
    return createTestTree(compToTest, testName, 25000);
  }

  protected String getRenderKitId()
  {
    return "org.apache.myfaces.trinidad.core";
  }

  @SuppressWarnings("unchecked")
  protected UIViewRoot createTestTree(
               UIComponent compToTest, String testName, int count)
    throws IOException
  {
    if (_fileWriter != null)
    {
      _fileWriter.write("\nRunning test " + testName + "\n");
      _fileWriter.write("----------------------------------\n");
    }
    UIViewRoot root = RenderKitBootstrap.createUIViewRoot(_facesContext);
    root.setRenderKitId(getRenderKitId());

    CoreDocument doc = new CoreDocument();
    doc.setId("docId");
    root.getChildren().add(doc);
    CoreForm form = new CoreForm();
    form.setId("formId");
    doc.getChildren().add(form);

    PerfComp perfer = new PerfComp(count, testName);
    form.getChildren().add(perfer);
    perfer.getChildren().add(compToTest);

    return root;
  }

  @Override
  protected void setUp() throws IOException
  {
    RenderKitBootstrap.clearFactories();
    RenderKitBootstrap.setFactories(_bootstrap.getFacesConfigInfo());

    //    _fileWriter = new java.io.BufferedWriter(new java.io.FileWriter("test.out", true));
    //    _fileWriter = new java.io.BufferedWriter(new java.io.FileWriter("c:\\tmp\\test.out", true));

    //    
    _facesContext = new MFacesContext(MApplication.sharedInstance(), false);
    ResponseWriter writer = new XhtmlResponseWriter(
      _fileWriter == null ? new NullWriter() : _fileWriter,
      XhtmlResponseWriter.XHTML_CONTENT_TYPE,
      "UTF-8");

    _facesContext.setResponseWriter(writer);

    _requestContext = new MRequestContext();
    _requestContext.setSkinFamily("minimal");
    _requestContext.setAgent(RenderKitBootstrap.getGeckoAgent());
  }
  
  @Override
  protected void tearDown() throws IOException
  {
    MFacesContext.clearContext();
    _requestContext.release();
    if (_fileWriter != null)
      _fileWriter.close();

    RenderKitBootstrap.clearFactories();
  }
  

  private ExtendedRenderKitService _getExtendedRenderKitService(
    FacesContext context)
  {
    RenderKit rk = context.getRenderKit();
    if (rk == null)
      throw new NullPointerException("No renderkit");

    return Service.getService(rk, ExtendedRenderKitService.class);
  }



  static private class PerfComp extends UIComponentBase
  {
    public PerfComp(int count, String name)
    {
      _count = count;
      _name  = name;
      setRendererType(null);
    }

    @Override
    public String getFamily()
    {
      return "org.apache.myfaces.trinidadtest.PerfComp";
    }

    @Override
    public boolean getRendersChildren()
    {
      return true;
    }

    @Override
    public void encodeChildren(FacesContext context) throws IOException
    {
      long start = System.currentTimeMillis();
      UIComponent child = (UIComponent) getChildren().get(0);
      for (int i = 0; i < _count; i++)
      {
        RenderUtils.encodeRecursive(context, child);
      }
      long count = System.currentTimeMillis() - start;
      System.out.println("Test: " + _name + ", " + count + " ms");
    }

    private int _count;
    private String _name;
  }

  private java.io.Writer     _fileWriter;
  private MFacesContext _facesContext;
  private MRequestContext _requestContext;

  static private RenderKitBootstrap _bootstrap;


  static
  {
    try
    {
      _bootstrap = new RenderKitBootstrap();
      _bootstrap.init();
    }
    catch (Throwable t)
    {
      t.printStackTrace();
    }
  }
}
