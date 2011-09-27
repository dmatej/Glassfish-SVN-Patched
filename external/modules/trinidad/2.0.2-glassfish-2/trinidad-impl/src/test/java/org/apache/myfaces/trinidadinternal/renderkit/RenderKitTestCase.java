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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import javax.faces.event.PhaseId;

import org.apache.commons.lang.StringUtils;

import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.render.ExtendedRenderKitService;
import org.apache.myfaces.trinidad.util.Service;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

import org.apache.myfaces.trinidadinternal.io.XhtmlResponseWriter;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;
import junit.framework.TestResult;
import junit.framework.TestSuite;

import org.xml.sax.SAXException;

abstract public class RenderKitTestCase extends TestSuite
{

  public RenderKitTestCase(String testName) throws IOException, SAXException
  {
    super(testName);

    try
    {
      _initGlobal();
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }

    _initTests();
  }

  protected void setUp() throws Exception
  {
  }

  protected void tearDown() throws Exception
  {
  }

  @Override
  public void run(TestResult result)
  {
    try
    {
      setUp();
      super.run(result);
      tearDown();
    }
    catch (Exception e)
    {
      result.addError(this, e);
    }
  }


  abstract public class BaseTest extends FacesTestCase
  {
    public BaseTest(String name,
                    SuiteDefinition definition)
    {
      this(name,
           definition.getCategory(),
           definition.getSkin(),
           definition.getAgent(),
           definition.getAccessibilityMode(),
           definition.isRightToLeft());
    }


    public BaseTest(String name,
                    String categoryName,
                    String skin,
                    Agent agent,
                    RequestContext.Accessibility accMode,
                    boolean rightToLeft)
    {
      super(name + "-" + categoryName);
      _skin = skin;
      _agent = agent;
      _accMode = accMode;
      _rightToLeft = rightToLeft;
    }

    @Override
    public void run(TestResult result)
    {
      // Cache the TestResult so we can directly add failure without
      // aborting the run
      _result = result;
      CatchSevere catchSevere = new CatchSevere();
      Logger apacheLogger = Logger.getLogger("org.apache");
      apacheLogger.addHandler(catchSevere);

      try
      {
        RenderKitBootstrap.setFactories(_facesConfigInfo);
        super.run(result);
      }
      finally
      {
        apacheLogger.removeHandler(catchSevere);
        RenderKitBootstrap.clearFactories();
      }
    }

    @Override
    protected void setUp() throws IOException  
    {
      _facesContext = new MFacesContext(MApplication.sharedInstance(), true);
      _requestContext = new MRequestContext();
      _requestContext.setSkinFamily(_skin);
      _requestContext.setAgent(_agent);
      _requestContext.setRightToLeft(_rightToLeft);
      _requestContext.setAccessibilityMode(_accMode);
      _facesContext.setCurrentPhaseId(PhaseId.RENDER_RESPONSE);

      UIViewRoot root = RenderKitBootstrap.createUIViewRoot(_facesContext);
      root.setRenderKitId(getRenderKitId());
      _facesContext.setViewRoot(root);

      ExtendedRenderKitService service =
        _getExtendedRenderKitService(_facesContext);

      if (service != null)
        service.encodeBegin(_facesContext);
        
    }

    @Override
    protected void tearDown() throws IOException  
    {
      ExtendedRenderKitService service =
        _getExtendedRenderKitService(_facesContext);
      if (service != null)
      {
        service.encodeEnd(_facesContext);
        service.encodeFinally(_facesContext);
      }

      MFacesContext.clearContext();
      _requestContext.release();

      _facesContext = null;
      _requestContext = null;
      _result = null;
    }

    @Override
    abstract protected void runTest() throws Throwable;

    protected FacesContext getFacesContext()
    {
      return _facesContext;
    }

    protected TestResult getResult()
    {
      return _result;
    }

    protected Agent getAgent()
    {
      return _agent;
    }

    protected void renderRoot(UIViewRoot root) throws IOException
    {
      RenderUtils.encodeRecursive(_facesContext, root);
    }
    
    protected void initializeContext(Writer out) throws IOException
    {
      _facesContext.getExternalContext().getRequestMap().clear();
      _facesContext.setResponseWriter(_createResponseWriter(out));
    }

    private ResponseWriter _createResponseWriter(Writer out) throws IOException
    {
      return new TestResponseWriter(out,
                                    XhtmlResponseWriter.XHTML_CONTENT_TYPE,
                                    "UTF-8",
                                    this,
                                    _result);
    }

    // Severe errors should count as a test failure
    private class CatchSevere extends Handler
    {
      @Override
      public void publish(LogRecord record)
      {
        if (record.getLevel() == Level.SEVERE)
        {
          String message = (new SimpleFormatter()).format(record);
          _result.addError(BaseTest.this,
                           new AssertionFailedError(message));
        }
      }

      @Override
      public void flush() { }

      @Override
      public void close() { }
    }

    private TestResult    _result;
    private MFacesContext _facesContext;
    private MRequestContext _requestContext;
    private String           _skin;
    private Agent            _agent;
    private RequestContext.Accessibility  _accMode;
    private boolean          _rightToLeft;
  }



  public class RendererTest extends BaseTest
  {
    public RendererTest(String name,
                        SuiteDefinition definition,
                        boolean lenient) throws IOException, SAXException
    {
      super(name, definition);
      _scriptName = name + ".xml";
      File scriptFile = new File(_scriptDir, _scriptName);

      _script =
        TestScriptParser.getTestScript(scriptFile, _facesConfigInfo);
      _lenient     = lenient;


      // We run golden-file checks on each subtest - though all differences
      // get counted only as a single diff.  We also do a comparison
      // of each subtest against the base, and verify that startComponent()
      // is correctly called
      if (lenient)
        _testCaseCount = 1;
      else
        _testCaseCount = (_script.getTests().size() * 3) + 1;
    }

    @Override
    public int countTestCases()
    {
      // See TRINIDAD-48: reporting anything other than the number
      // of actual JUnit testcases makes JUnit confused
      return 1;//_testCaseCount;
    }

    @Override
    public void run(TestResult result)
    {
      if (!_script.isSupportedAgentType(getAgent().getType()))
      {
        /*
        System.out.println("SKIPPING UNSUPPORTED SCRIPT: " + _scriptName);
        System.out.println("AGENT IS " + getAgent());
        System.out.println("AGENT TYPE IS " + getAgent().getType());
        */
        return;
      }

      super.run(result);
    }

    @Override
    protected void tearDown() throws IOException  
    {
      super.tearDown();
      _script = null;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void runTest() throws Throwable
    {
      UIViewRoot root = getFacesContext().getViewRoot();

      initializeContext(new NullWriter());

      UIComponent docRoot = populateDefaultComponentTree(root,
                                                         _script);

      StringWriter first = new StringWriter();
      docRoot.getChildren().add(new GatherContent(first,
                                               _createComponent(),
                                               getResult(),
                                               this,
                                               _lenient));

      StringWriter base = new StringWriter();
      docRoot.getChildren().add(new GatherContent(base,
                                               _createComponent(),
                                               getResult(),
                                               this,
                                               _lenient));

      Iterator<TestScript.Test> tests = _script.getTests().iterator();
      while (tests.hasNext())
      {
        TestScript.Test test = tests.next();

        UIComponent testComponent = _createComponent();

        test.apply(getFacesContext(), testComponent);
        docRoot.getChildren().add(new GatherContent(test.getOutput(),
                                                 testComponent,
                                                 getResult(),
                                                 this,
                                                 _lenient));
      }

      

      renderRoot(root);

      File goldenFile = new File(_goldenDir, getName() + "-golden.xml");
      String golden = null;
      if (goldenFile.exists())
      {
        StringBuffer buffer = new StringBuffer((int) goldenFile.length());
        BufferedReader in = new BufferedReader(new FileReader(goldenFile));
        while (true)
        {
          String line = in.readLine();
          if (line == null)
            break;
          buffer.append(line);
          buffer.append('\n');
        }

        golden = buffer.toString();
        in.close();
      }

      boolean forceGolden = "true".equals(
         System.getProperty("org.apache.myfaces.trinidad.ForceGolden"));

      Writer out = new StringWriter(golden == null ? 1000 : golden.length());
      out.write("<results>");
      String baseResults = base.toString();
      out.write(baseResults);


      tests = _script.getTests().iterator();
      while (tests.hasNext())
      {
        TestScript.Test test = tests.next();
        out.write("\n<!--");
        out.write(test.toString());
        out.write("-->\n");
        String testResults = test.getOutput().toString();
        out.write(testResults);

        if (_lenient)
          continue;
        if (!test.shouldMatchBase() &&
            baseResults.equals(testResults))
        {
          AssertionFailedError failure = new AssertionFailedError(
            "Result of " + test.toString() + " were identical to " +
            "base, but should not have been!");
          getResult().addError(this, failure);
        }
        else if (test.shouldMatchBase() &&
                 !baseResults.equals(testResults))
        {
          AssertionFailedError failure = new AssertionFailedError(
            "Result of " + test.toString() + " were not identical to " +
            "base, but should have been!");
          getResult().addError(this, failure);
        }
      }

      out.write("\n</results>\n");
      out.close();

      String results = out.toString();
      if ((golden == null) || !golden.equals(results))
      {
        File failureFile;
        // Set the "org.apache.myfaces.trinidad.ForceGolden" property to true to
        // force failures to be directly copied into the target directory
        if (forceGolden)
          failureFile = new File(_goldenDir, getName() + "-golden.xml");
        else
          failureFile = new File(_failureDir, getName() + "-golden.xml");
        failureFile.getParentFile().mkdirs();
        FileWriter failureOut = new FileWriter(failureFile);
        failureOut.write(results);
        failureOut.close();

        if (golden == null)
        {
          // Don't report "no golden file" as an error when
          // forceGolden is on; but do report diffs as errors
          if (!forceGolden)
          {
            throw new AssertionFailedError("No golden file for test " +
                                           _scriptName);
          }
        }
        else
        {
          int index = StringUtils.indexOfDifference(golden, results);
          String difference = StringUtils.difference(golden, results);
          int diffLength = difference.length();
          if (diffLength > 50)
            difference = StringUtils.abbreviate(difference, 50);
          throw new AssertionFailedError(
               "Golden file for test "+ _scriptName + " did not match; " +
               "first difference at " + index + ", difference of length " +
               diffLength + ", \"" + difference + "\"");
        }
      }
    }

    private UIComponent _createComponent()
    {
        return _script.getDefinition().createComponent(getFacesContext());
    }

    private int           _testCaseCount;
    private String           _scriptName;
    private TestScript       _script;
    private boolean          _lenient;
  }


  static private void _initGlobal() throws IOException, SAXException
  {
    RenderKitBootstrap bootstrap = new RenderKitBootstrap();
    bootstrap.init();

    _facesConfigInfo = bootstrap.getFacesConfigInfo();

    String scripts = System.getProperty("trinidad.renderkit.scripts");
    String golden = System.getProperty("trinidad.renderkit.golden");
    String failures = System.getProperty("trinidad.renderkit.failures");

    _scriptDir = new File(scripts);
    _goldenDir = new File(golden);
    _failureDir = new File(failures);
  }

  private void _initTests() throws IOException, SAXException
  {
    String script = System.getProperty("trinidad.renderkit.script");
    Set<String> includedScripts = null;
    if (script != null)
    {
      String[] scripts = script.split(",");
      includedScripts = new HashSet<String>();
      for (int i = 0; i < scripts.length; i++)
      {
        System.out.println("Including " + scripts[i]);
        includedScripts.add(scripts[i]);
      }
    }

    // See if we want to run the full test suite (by default, no)
    String fulltests = System.getProperty("trinidad.renderkit.fulltests");
    // We can run the full test suite in two modes:  strict, and lenient.
    // We should go to "strict" all the time, but "lenient" simply
    // diffs against the golden files
    boolean lenient = "lenient".equals(fulltests);

    String[] scriptArray = _scriptDir.list();
    for (int i = 0; i < scriptArray.length; i++)
    {
      String name = scriptArray[i];
      if ((includedScripts != null) && !includedScripts.contains(name))
        continue;

      if (name.endsWith(".xml"))
      {
        boolean first = true;
        name = name.substring(0, name.length() - 4);
        for (SuiteDefinition definition : getSuiteDefinitions())
        {
          if (first)
          {
            addTest(new RendererTest(name,
                                     definition,
                                     false));
            first = false;
          }
          else
          {
            addTest(new RendererTest(name, definition, lenient));
          }
        }
      }
    }
  }

  protected abstract UIComponent populateDefaultComponentTree(
    UIViewRoot root,
    TestScript script);

  protected abstract Iterable<SuiteDefinition> getSuiteDefinitions();
  protected abstract String getRenderKitId();

  static public class SuiteDefinition
  {
    public SuiteDefinition(
      String category,
      String skin,
      RequestContext.Accessibility accessibilityMode,
      Agent  agent,
      boolean rightToLeft)
    {
      _category = category;
      _skin     = skin;
      _accessibilityMode  = accessibilityMode;
      _agent    = agent;
      _rightToLeft = rightToLeft;
    }

    public String getCategory()
    {
      return _category;
    }

    public String getSkin()
    {
      return _skin;
    }

    public RequestContext.Accessibility getAccessibilityMode()
    {
      return _accessibilityMode;
    }

    public Agent getAgent()
    {
      return _agent;
    }


    public boolean isRightToLeft()
    {
      return _rightToLeft;
    }

    private String _category;
    private String _skin;
    private RequestContext.Accessibility _accessibilityMode;
    private Agent _agent;
    private boolean _rightToLeft;
  }

  static private ExtendedRenderKitService _getExtendedRenderKitService(
    FacesContext context)
  {
    return Service.getService(context.getRenderKit(),
                              ExtendedRenderKitService.class);
  }


  static private FacesConfigInfo _facesConfigInfo;
  static private File _scriptDir;
  static private File _goldenDir;
  static private File _failureDir;
}
