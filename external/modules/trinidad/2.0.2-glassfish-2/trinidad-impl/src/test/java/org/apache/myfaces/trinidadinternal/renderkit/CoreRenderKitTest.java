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

import java.io.IOException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;

import junit.framework.Test;

import org.apache.myfaces.trinidad.component.core.CoreDocument;
import org.apache.myfaces.trinidad.component.core.CoreForm;
import org.apache.myfaces.trinidad.component.html.HtmlHtml;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.render.RenderUtils;

import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;

import org.xml.sax.SAXException;


public class CoreRenderKitTest extends RenderKitTestCase
{
  public static Test suite() throws Throwable
  {
    CoreRenderKitTest suite =
      new CoreRenderKitTest(CoreRenderKitTest.class.getName());
    return suite;
  }

  public CoreRenderKitTest(String testName) throws IOException, SAXException
  {
    super(testName);
  }

  @Override
  protected Iterable<SuiteDefinition> getSuiteDefinitions()
  {
    return _definitions;
  }

  @Override
  protected String getRenderKitId()
  {
    return "org.apache.myfaces.trinidad.core";
  }

  @SuppressWarnings("unchecked")
  @Override
  protected UIComponent populateDefaultComponentTree(
    UIViewRoot  root,
    TestScript  script)
  {
    String componentType =
           script.getDefinition().getComponentInfo().componentType;

    if ("org.apache.myfaces.trinidad.HtmlHtml".equals(componentType))
    {
      return root;
    }

    if (_sHtmlComponents.contains(componentType))
    {
      HtmlHtml html = new HtmlHtml();
      html.setId("htmlId");
      root.getChildren().add(html);
      return html;
    }
    else
    {
      CoreDocument doc = new CoreDocument();
      doc.setId("docId");
      root.getChildren().add(doc);
      CoreForm form = new CoreForm();
      form.setId("formId");
      if (script.getDefinition().isUsesUpload())
        form.setUsesUpload(true);
      doc.getChildren().add(form);
      return form;
    }
  }

  static private List<SuiteDefinition> _definitions =
    new ArrayList<SuiteDefinition>();
  private static HashSet<String> _sHtmlComponents;

  static
  {
    // Force the CoreRenderKit logger level to SEVERE, to bypass the
    // warnings about not finding the Basic HTML RenderKit.
    Logger logger = Logger.getLogger(CoreRenderKit.class.getName());
    logger.setLevel(Level.SEVERE);
    logger.setUseParentHandlers(false);
    // Force the RenderUtils logger level to SEVERE to bypass the
    // warnings in getRelativeId method when the component
    // with the relativeId could not be found which is the case in our
    // render kit rendering tests.
    Logger loggerTwo = Logger.getLogger(RenderUtils.class.getName());
    loggerTwo.setLevel(Level.SEVERE);
    loggerTwo.setUseParentHandlers(false);

    _definitions.add(new SuiteDefinition("minimal",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getGeckoAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalIE",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getIEAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalIERtl",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getIEAgent(),
                                         true));
    _definitions.add(new SuiteDefinition("minimalPPC",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getPocketPCAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalSaf",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getSafariAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalScrRdr",
                                         "minimal",
                                         RequestContext.Accessibility.SCREEN_READER,
                                         RenderKitBootstrap.getGeckoAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalInacc",
                                         "minimal",
                                         RequestContext.Accessibility.INACCESSIBLE,
                                         RenderKitBootstrap.getGeckoAgent(),
                                         false));

    _sHtmlComponents = new HashSet<String>(5);
    _sHtmlComponents.add("org.apache.myfaces.trinidad.HtmlBody");
    _sHtmlComponents.add("org.apache.myfaces.trinidad.HtmlFrame");
    _sHtmlComponents.add("org.apache.myfaces.trinidad.HtmlFrameBorderLayout");
    _sHtmlComponents.add("org.apache.myfaces.trinidad.HtmlHead");
    _sHtmlComponents.add("org.apache.myfaces.trinidad.CoreStyleSheet");
  }
}
