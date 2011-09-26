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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.util.Locale;

import javax.faces.component.UIViewRoot;
import javax.faces.context.ResponseWriter;

import junit.framework.TestCase;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.io.XhtmlResponseWriter;
import org.apache.myfaces.trinidadinternal.renderkit.FacesConfigInfo;
import org.apache.myfaces.trinidadinternal.renderkit.MApplication;
import org.apache.myfaces.trinidadinternal.renderkit.MFacesContext;
import org.apache.myfaces.trinidadinternal.renderkit.MRequestContext;
import org.apache.myfaces.trinidadinternal.renderkit.NullWriter;
import org.apache.myfaces.trinidadinternal.renderkit.RenderKitBootstrap;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;

/**
 * Unit tests for XhtmlLafUtils.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/test/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/XhtmlLafUtilsTest.java#1 $) $Date: 16-aug-2005.15:15:42 $
 */
public class XhtmlLafUtilsTest extends TestCase
{
  public XhtmlLafUtilsTest(
    String testName)
  {
    super(testName);
  }

  protected void setUp() throws Exception
  {
    RequestContext rc = RequestContext.getCurrentInstance();
    if (rc != null)
    {
      rc.release();
    }
    
    _bootstrap = new RenderKitBootstrap();
    _bootstrap.init();
    
    RenderKitBootstrap.clearFactories();
    RenderKitBootstrap.setFactories(_bootstrap.getFacesConfigInfo());

    _facesContext = new MFacesContext(MApplication.sharedInstance(), true);
    _requestContext = new MRequestContext();
    _requestContext.setSkinFamily("minimal");
    _requestContext.setAgent(RenderKitBootstrap.getGeckoAgent());
    _requestContext.setRightToLeft(false);
    _requestContext.setAccessibilityMode(null);

    UIViewRoot root = RenderKitBootstrap.createUIViewRoot(_facesContext);
    root.setRenderKitId("org.apache.myfaces.trinidad.core");
    root.setLocale(Locale.getDefault());
    _facesContext.setViewRoot(root);
    
    try
    {
      new CoreRenderingContext();
    }
    catch (IllegalStateException ex)
    {
      return;
    }
  }

  protected void tearDown() throws Exception
  {
    MFacesContext.clearContext();
    _requestContext.release();
    RenderKitBootstrap.clearFactories();
  }

  /**
   * Tests JavaScript strings are escaped inside single quotation marks.
   */
  public void testEscapeInQuoteJS()
  {
    String raw = "a'b";
    StringBuilder escaped = new StringBuilder();
    XhtmlLafUtils.escapeJS(escaped, raw, true);
    assertEquals("a\\\'b", escaped.toString());
  }

  /**
   * Tests JavaScript strings are double escaped inside single quotation marks.
   */
  public void testDoubleEscapeInQuoteJS()
  {
    String raw = "a'b";
    StringBuilder escaped = new StringBuilder();
    XhtmlLafUtils.escapeJS(escaped, raw, true, 2);
    assertEquals("a\\\\\\\'b", escaped.toString());
  }
  
  private MFacesContext _facesContext;
  private MRequestContext _requestContext;

  private RenderKitBootstrap _bootstrap;
}
