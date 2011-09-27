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
package org.apache.myfaces.trinidad.component;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;

import java.io.IOException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.Collections;
import java.util.Map;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.el.ValueBinding;
import javax.faces.event.FacesEvent;
import javax.faces.event.ValueChangeListener;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;
import javax.faces.render.Renderer;
import javax.faces.validator.Validator;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.context.MockRequestContext;
import org.apache.myfaces.trinidad.event.AttributeChangeEvent;
import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

import org.jmock.Mock;
import org.jmock.core.Constraint;


/**
 * Base class for JavaServer Faces UIComponent unit tests.
 *
 */
public class UIComponentTestCase extends FacesTestCase
{
  /**
   * Creates a new UIComponentTestCase.
   *
   * @param testName  the unit test name
   */
  public UIComponentTestCase(
    String testName)
  {
    super(testName);
  }

  @Override
  protected void setUp() throws Exception
  {
    _mockRequestContext = new MockRequestContext();
    super.setUp();
  }

  @Override
  protected void tearDown() throws Exception
  {
    super.tearDown();
    _mockRequestContext.release();
  }

  public static Test suite()
  {
    return new TestSuite(UIComponentTestCase.class);
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   *
   * @param component   the component with attribute map
   * @param attrName    the attribute name to test
   * @param attrValue   the value for use by the attribute map mutator
   * @param propValue   the value for use by the bean mutator
   */
  @SuppressWarnings("unchecked")
  protected void doTestAttributeTransparency(
    UIComponent component,
    String      attrName,
    Object      attrValue,
    Object      propValue)
  {
    assertFalse("Test values for attribute \"" + attrName + "\" must differ",
                (attrValue == propValue ||
                 (attrValue != null &&
                  attrValue.equals(propValue))));

    Map<String, Object> attrMap = component.getAttributes();
    try
    {
      boolean foundProperty = false;
      // bean info is cached
      BeanInfo info = Introspector.getBeanInfo(component.getClass());
      PropertyDescriptor[] pds = info.getPropertyDescriptors();
      for (int i=0; i < pds.length; i++)
      {
        String propName = pds[i].getName();
        if (attrName.equals(propName))
        {
          if (pds[i].getPropertyType().isPrimitive())
          {
            assertNotNull("Primitive \"" + attrName +
                          "\" attribute value must be non-null",
                          attrValue);
            assertNotNull("Primitive \"" + propName +
                          "\" property value must be non-null",
                          propValue);
          }

          foundProperty = true;

          Method reader = pds[i].getReadMethod();
          Method writer = pds[i].getWriteMethod();
          writer.invoke(component, new Object[] { propValue });
          assertEquals("Property set not visible in attribute map",
                       attrMap.get(attrName), propValue);
          attrMap.put(attrName, attrValue);
          assertEquals("Attribute put not visible in property value",
                       reader.invoke(component, new Object[0]), attrValue);
          break;
        }
      }

      if (!foundProperty)
        fail("Unable to find attribute property \"" + attrName + "\"");
    }
    catch (IntrospectionException e)
    {
      e.printStackTrace();
      fail("Unable to access attribute property \"" + attrName + "\"");
    }
    catch (InvocationTargetException e)
    {
      e.printStackTrace();
      fail("Unable to access attribute property \"" + attrName + "\"");
    }
    catch (IllegalAccessException e)
    {
      e.printStackTrace();
      fail("Unable to access attribute property \"" + attrName + "\"");
    }
  }

  /**
   * Tests the transparency of the component facet by comparing
   * bean accessor and mutator methods with facet map accessor
   * and mutator methods.
   *
   * @param component   the component with attribute map
   * @param facetName   the facet name to test
   * @param facetValue  the value for use by the facet map mutator
   * @param propValue   the value for use by the bean mutator
   */
  @SuppressWarnings("unchecked")
  protected void doTestFacetTransparency(
    UIComponent component,
    String      facetName)
  {
    Mock mockFacetValue = mock(UIComponent.class);
    UIComponent facetValue = (UIComponent) mockFacetValue.proxy();
    mockFacetValue.stubs().method("getParent").will(returnValue(null));
    mockFacetValue.stubs().method("setParent");

    Mock mockPropValue = mock(UIComponent.class);
    UIComponent propValue = (UIComponent) mockPropValue.proxy();
    mockPropValue.stubs().method("getParent").will(returnValue(null));
    mockPropValue.stubs().method("setParent");

    Map<String, UIComponent> facetMap = component.getFacets();
    try
    {
      // bean info is cached
      BeanInfo info = Introspector.getBeanInfo(component.getClass());
      PropertyDescriptor[] pds = info.getPropertyDescriptors();
      boolean foundProperty = false;
      for (int i=0; i < pds.length; i++)
      {
        String propName = pds[i].getName();
        if (facetName.equals(propName))
        {
          assertTrue("Facet bean accessor must return UIComponent or subclass",
            UIComponent.class.isAssignableFrom(pds[i].getPropertyType()));

          foundProperty = true;

          Method reader = pds[i].getReadMethod();
          Method writer = pds[i].getWriteMethod();
          writer.invoke(component, new Object[] { propValue });
          assertEquals("Property set not visible in facet map",
                       facetMap.get(facetName), propValue);
          facetMap.put(facetName, facetValue);
          assertEquals("Facet put not visible in property value",
                       reader.invoke(component, new Object[0]), facetValue);
          break;
        }
      }

      if (!foundProperty)
        fail("Unable to find facet property \"" + facetName + "\"");
    }
    catch (IntrospectionException e)
    {
      e.printStackTrace();
      fail("Unable to access facet property \"" + facetName + "\"");
    }
    catch (InvocationTargetException e)
    {
      e.printStackTrace();
      fail("Unable to access facet property \"" + facetName + "\"");
    }
    catch (IllegalAccessException e)
    {
      e.printStackTrace();
      fail("Unable to access facet property \"" + facetName + "\"");
    }
    finally
    {
      mockFacetValue.verify();
      mockPropValue.verify();
    }
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  protected void doTestApplyRequestValues(
    UIComponent component)
  {
    UIViewRoot root = new UIViewRoot();
    doTestApplyRequestValues(root, component);
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  protected void doTestApplyRequestValues(
    UIViewRoot  root,
    UIComponent component)
  {

    Mock mockRenderKitFactory = mock(RenderKitFactory.class);

    Mock mockRenderkit = getMockRenderKitWrapper().getMock();
    RenderKit renderkit = getMockRenderKitWrapper().getRenderKit();

    Mock mockRenderer = mock(Renderer.class);
    Renderer renderer = (Renderer) mockRenderer.proxy();

    mockRenderKitFactory.stubs().method("getRenderKit").will(returnValue(renderkit));
    mockRenderkit.stubs().method("getRenderer").will(returnValue(renderer));

    if (isRendererUsed() && component.isRendered())
    {
      mockRenderer.expects(once()).method("decode");
    }
    else
    {
      mockRenderer.expects(never()).method("decode");
    }

    try
    {
      setCurrentContext(facesContext);
      doTestApplyRequestValues(facesContext, root, component);
    }
    finally
    {
      setCurrentContext(null);
    }

    mockRenderKitFactory.verify();
    mockRenderkit.verify();
    mockRenderer.verify();
  }


  @SuppressWarnings("unchecked")
  protected void doTestApplyRequestValues(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {

    Mock mock = createMockUIComponent();
    UIComponent child = (UIComponent) mock.proxy();

    // JavaServer Faces 1.0 Specification, section 2.2.2
    // During the apply-request-values phase,
    // only the processDecodes lifecycle method may be called.
    if (willChildrenBeProcessed(component))
      mock.expects(once()).method("processDecodes");

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    if (component.getParent() == null)
      root.getChildren().add(component);

    component.getChildren().add(child);

    AttributeChangeTester attributeChangeTester = null;
    if (component instanceof UIXComponent)
    {
      attributeChangeTester = new AttributeChangeTester();
      ((UIXComponent) component).setAttributeChangeListener(attributeChangeTester);
      ((UIXComponent) component).addAttributeChangeListener(attributeChangeTester);
      AttributeChangeEvent ace =
        new AttributeChangeEvent(component, "testProperty",
                                 Boolean.FALSE, Boolean.TRUE);
      ace.queue();
    }

    root.processDecodes(context);

    if (attributeChangeTester != null)
      attributeChangeTester.verify();

    mock.verify();
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  protected void doTestProcessValidations(
    UIComponent component)
  {
    doTestProcessValidations(component, "submittedValue", "convertedValue");
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  protected void doTestProcessValidations(
    UIComponent component,
    Object      submittedValue,
    Object      convertedValue)
  {
    UIViewRoot root = new UIViewRoot();
    doTestProcessValidations(root, component, submittedValue, convertedValue);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  protected void doTestProcessValidations(
    UIViewRoot  root,
    UIComponent component,
    Object      submittedValue,
    Object      convertedValue)
  {

    Mock mockRenderKit = getMockRenderKitWrapper().getMock();

    Mock mockRenderer = mock(Renderer.class);
    Renderer renderer = (Renderer) mockRenderer.proxy();
    mockRenderKit.stubs().method("getRenderer").will(returnValue(renderer));

    Mock mockConverter = mock(Converter.class);
    Converter converter = (Converter) mockConverter.proxy();

    Mock mockValidator = mock(Validator.class);
    Validator validator = (Validator) mockValidator.proxy();

    Mock mockListener = mock(ValueChangeListener.class);
    ValueChangeListener listener = (ValueChangeListener) mockListener.proxy();

    setCurrentContext(facesContext);

    // if the component is an EditableValueHolder, then the submitted value
    // must be converted and validated before this phase completes.
    if (component instanceof EditableValueHolder)
    {

      EditableValueHolder editable = (EditableValueHolder)component;
      mockConverter.expects(never()).method("getAsObject");
      mockConverter.expects(never()).method("getAsString");
      mockRenderer.expects(once()).method("getConvertedValue").will(returnValue(convertedValue));
      editable.setConverter(converter);
      editable.setSubmittedValue(submittedValue);
      editable.addValidator(validator);
      editable.addValueChangeListener(listener);

      mockListener.expects(once()).method("processValueChange");
      mockValidator.expects(once()).method("validate").with(new Constraint[]  { eq(facesContext), eq(component), eq(convertedValue) });

    }
    // if the component is a ValueHolder, then the value is not updated or
    // validated and no value change event occurs.
    else if (component instanceof ValueHolder)
    {
      ValueHolder holder = (ValueHolder)component;
      holder.setConverter(converter);
      mockConverter.expects(never()).method("getAsObject");//setExpectedGetAsObjectCalls(0);
      mockConverter.expects(never()).method("getAsString");
    }

    doTestProcessValidations(facesContext, root, component);

    mockRenderKit.verify();
    mockRenderer.verify();
    mockConverter.verify();
    mockValidator.verify();
    mockListener.verify();

    setCurrentContext(null);
  }


  @SuppressWarnings("unchecked")
  protected void doTestProcessValidations(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {

    Mock mock = createMockUIComponent();
    UIComponent child = (UIComponent) mock.proxy();

    // JavaServer Faces 1.0 Specification, section 2.2.3
    // During the process-validations phase,
    // only the processValidators lifecycle method may be called.
    if (willChildrenBeProcessed(component))
      mock.expects(once()).method("processValidators");

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    if (component.getParent() == null)
      root.getChildren().add(component);
    component.getChildren().add(child);

    root.processValidators(context);

    mock.verify();
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  protected void doTestUpdateModelValues(
    UIComponent component)
  {
    UIViewRoot root = new UIViewRoot();
    doTestUpdateModelValues(root, component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  protected void doTestUpdateModelValues(
    UIViewRoot  root,
    UIComponent component)
  {

    Mock mockRenderkit = getMockRenderKitWrapper().getMock();

    Mock mockRenderer = mock(Renderer.class);
    Renderer renderer = (Renderer) mockRenderer.proxy();
    mockRenderkit.stubs().method("getRenderer").will(returnValue(renderer));

    Mock mockBinding = mock(ValueBinding.class);
    ValueBinding binding = (ValueBinding) mockBinding.proxy();

    setCurrentContext(facesContext);

    // if the component is an EditableValueHolder, then the value binding
    // must be updated with the new value before this phase completes.
    if (component instanceof EditableValueHolder)
    {
      EditableValueHolder editable = (EditableValueHolder)component;
      component.setValueBinding("value", binding);
      editable.setValue("newValue");
      mockBinding.expects(atLeastOnce()).method("setValue").with(eq(facesContext), eq("newValue"));

      assertEquals(true, editable.isLocalValueSet());
    }

    doTestUpdateModelValues(facesContext, root, component);

    setCurrentContext(null);

    mockRenderer.verify();
    mockBinding.verify();
  }


  @SuppressWarnings("unchecked")
  protected void doTestUpdateModelValues(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {
    Mock mock = createMockUIComponent();
    UIComponent child = (UIComponent) mock.proxy();

    // JavaServer Faces 1.0 Specification, section 2.2.4
    // During the update-model-values phase,
    // only the processUpdates lifecycle method may be called.
    if (willChildrenBeProcessed(component))
      mock.expects(once()).method("processUpdates");

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    if (component.getParent() == null)
      root.getChildren().add(component);
    component.getChildren().add(child);
    root.processUpdates(context);

    mock.verify();
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  protected void doTestInvokeApplication(
    UIComponent   component,
    FacesEvent    event)
  {
    try
    {
      setCurrentContext(facesContext);

      doTestInvokeApplication(facesContext, facesContext.getViewRoot(), component, event);

    }
    finally
    {
      setCurrentContext(null);
    }
  }



  @SuppressWarnings("unchecked")
  protected void doTestInvokeApplication(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component,
    FacesEvent   event)
  {

    Mock mock = createMockUIComponent();
    UIComponent child = (UIComponent) mock.proxy();
    // JavaServer Faces 1.0 Specification, section 2.2.5
    // During the invoke-application phase,
    // no per-component lifecycle methods may be called.

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    root.getChildren().add(component);
    if (event != null)
      event.queue();

    component.getChildren().add(child);
    root.processApplication(context);

    mock.verify();
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  @SuppressWarnings("unchecked")
  protected void doTestRenderResponse(
    UIComponent component) throws IOException
  {

//    MockRenderKitFactory factory = setupMockRenderKitFactory();

    Mock mockRenderkit = getMockRenderKitWrapper().getMock();

    Mock mockRenderer = mock(Renderer.class);
    Renderer renderer = (Renderer) mockRenderer.proxy();
    mockRenderkit.stubs().method("getRenderer").will(returnValue(renderer));

    Mock mockChild = createMockUIComponent(); //mock(UIComponent.class);
    UIComponent child = (UIComponent) mockChild.proxy();

    mockChild.expects(atLeastOnce()).method("getParent").will(returnValue(null));
    mockChild.expects(atLeastOnce()).method("isTransient").will(returnValue(false));
    mockChild.expects(atLeastOnce()).method("getRendersChildren").will(returnValue(true));

    UIViewRoot root = new UIViewRoot();

    mockRenderer.expects(atLeastOnce()).method("getRendersChildren").will(returnValue(false));
    mockRenderer.expects(never()).method("decode");
    mockRenderer.expects(never()).method("getConvertedValue");
    mockRenderer.expects(never()).method("encodeChildren");

    if (isRendererUsed())
    {
      mockRenderer.expects(once()).method("encodeBegin");
      mockRenderer.expects(once()).method("encodeEnd");
    }
    else
    {
      mockRenderer.expects(never()).method("encodeBegin");
      mockRenderer.expects(never()).method("encodeEnd");
    }

    // JavaServer Faces 1.0 Specification, section 2.2.6
    // During the render-response phase,
    // only the encodeBegin, encodeEnd, encodeChildren
    // and processSaveState lifecycle methods may be called.
    mockChild.expects(never()).method("processRestoreState");
    mockChild.expects(never()).method("processDecodes");
    mockChild.expects(never()).method("processValidators");
    mockChild.expects(never()).method("processUpdates");
    mockChild.expects(once()).method("processSaveState");

    //fix this!
    mockChild.expects(once()).method("encodeBegin");
    mockChild.expects(once()).method("encodeChildren");
    mockChild.expects(once()).method("encodeEnd");

    root.getChildren().add(component);
    component.getChildren().add(child);

    FacesContext current = FacesContext.getCurrentInstance();
    try
    {
      TestFacesContext.setCurrentInstance(facesContext);
      root.processSaveState(facesContext);
      doRenderResponse(facesContext, root);
    }
    finally
    {
      TestFacesContext.setCurrentInstance(current);
    }

    mockRenderer.verify();
    mockChild.verify();
  }

  protected void doTestValidateFailure(
    UIViewRoot root)
  {
    // -= Simon =-
    // All those variables do not seem to be used and do not seem
    // to test anything either
    /*Mock mockRenderkit = getMockRenderKitWrapper().getMock();
    RenderKit renderkit = getMockRenderKitWrapper().getRenderKit();
    */
    Mock mockRenderer = mock(Renderer.class);
    /*Renderer renderer = (Renderer) mockRenderer.proxy();

    Mock mockValidator = mock(Validator.class);
    Validator validator = (Validator) mockValidator.proxy();

    ViewHandler viewhandler = this.facesContext.getApplication().getViewHandler();*/

    setCurrentContext(facesContext);

    root.processValidators(facesContext);

    mockRenderer.verify();

    setCurrentContext(null);
  }

  /**
   * Creates a MockUIComponent that does not expect to have
   * any of its lifecycle methods invoked;  if you expect to
   * have any invoked, override the "expected calls" for
   * that lifecycle method.
   */
  protected Mock createMockUIComponent()
  {
    Mock mock = mock(UIComponent.class);

    mock.stubs().method("getParent").will(returnValue(null));
    mock.stubs().method("setParent");
    mock.stubs().method("getFacetsAndChildren").will(returnIterator(Collections.emptyList()));

    mock.expects(never()).method("processRestoreState");
    mock.expects(never()).method("processDecodes");
    mock.expects(never()).method("processValidators");
    mock.expects(never()).method("processUpdates");
    mock.expects(never()).method("processSaveState");
    mock.expects(never()).method("encodeBegin");
    mock.expects(never()).method("encodeChildren");
    mock.expects(never()).method("encodeEnd");

    return mock;
  }

  protected boolean willChildrenBeProcessed(UIComponent component)
  {
    return (component.isRendered());
  }

  protected boolean willChildrenBeRendered(UIComponent component)
  {
    return true;
  }

  protected boolean isRendererUsed()
  {
    return _isRendererUsed;
  }

  protected void setRendererUsed(boolean isRendererUsed)
  {
    _isRendererUsed = isRendererUsed;
  }

  private boolean _isRendererUsed = true;
  private MockRequestContext _mockRequestContext;
}
