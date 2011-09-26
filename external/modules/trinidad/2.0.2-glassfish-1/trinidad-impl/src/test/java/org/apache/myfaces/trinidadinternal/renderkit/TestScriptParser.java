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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.myfaces.trinidadinternal.share.expl.Coercions;
import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.TreeBuilder;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

class TestScriptParser extends BaseNodeParser
{
  static public TestScript getTestScript(File file, FacesConfigInfo info)
    throws IOException, SAXException
  {
    TestScript script = new TestScript();
    TreeBuilder builder = new TreeBuilder();
    InputStream inputStream = new FileInputStream(file);

    try
    {
      InputSource source = new InputSource(inputStream);
      source.setSystemId(file.getAbsolutePath());
      builder.parse(source, new TestScriptParser(script, info));
    }
    finally
    {
      inputStream.close();
    }

    return script;
  }

  static public final String NAMESPACE = "http://myfaces.apache.org/trinidad";

  public TestScriptParser(TestScript script, FacesConfigInfo info)
  {
    _script = script;
    _info   = info;
  }

  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    String agentNotSupported = attrs.getValue("agentNotSupported");
    if (agentNotSupported != null)
    {
      StringTokenizer tokens = new StringTokenizer(agentNotSupported);
      while (tokens.hasMoreTokens())
      {
        _script.removeAgentType(tokens.nextToken());
      }
    }
  }

  public NodeParser createTestParser(
    FacesConfigInfo.ComponentInfo componentInfo,
    String                        localName,
    String componentId )
  {
    if ("attribute-test".equals(localName))
      return new AttributeTestParser(componentInfo, componentId);
    if ("enum-test".equals(localName))
      return new EnumTestParser(componentInfo);
    if ("boolean-test".equals(localName))
      return new BooleanTestParser(componentInfo);
    if ("js-test".equals(localName))
      return new JavascriptTestParser(componentInfo);
    if ("message".equals(localName))
      return new MessageParser(componentInfo);
    return null;
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    if ("base-component".equals(localName))
      return new ComponentDefinitionParser(_info);

    return createTestParser(_componentInfo, localName, null);
  }

  @Override
  @SuppressWarnings("unchecked")
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child) throws SAXParseException
  {
    if (child instanceof TestScript.Test)
    {
      _script.getTests().add((TestScript.Test)child);
    }
    else if (child instanceof List)
    {
      Iterator<TestScript.Test> iter = 
        ((List<TestScript.Test>) child).iterator();
      while (iter.hasNext())
      {
        _script.getTests().add(iter.next());
      }
    }
    else if (child instanceof ComponentDefinition)
    {
      ComponentDefinition definition = (ComponentDefinition) child;
      _componentInfo = definition.getComponentInfo();
      _script.setDefinition((ComponentDefinition) child);
    }
  }


  private class ComponentDefinitionParser extends BaseNodeParser
  {
    public ComponentDefinitionParser(FacesConfigInfo info)
    {
      this(null, info);
    }

    public ComponentDefinitionParser(
       ComponentDefinition parent, FacesConfigInfo info)
    {
      _parent = parent;
      _info   = info;
    }

    @Override
    public void startElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      _type = attrs.getValue(NAMESPACE, "type");
      if (_type == null)
        logError(context, "tr:type attribute not on component", null);
      String facet = attrs.getValue(NAMESPACE, "facet");
      _definition = new ComponentDefinition(_type, _info);
      for (int i = 0; i < attrs.getLength(); i++)
      {
        if ("".equals(attrs.getURI(i)))
        {
          String name = attrs.getLocalName(i);
          String valueStr = attrs.getValue(i);
          _definition.getAttributes().put(name, valueStr);
        }
      }

      if (_parent != null)
      {
        if (facet != null)
          _parent.getFacets().put(facet, _definition);
        else
          _parent.getChildren().add(_definition);
      }

      _definition.setUsesUpload(
         "true".equals(attrs.getValue(NAMESPACE, "usesUpload")));
    }

    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      if ("component".equals(localName))
      {
        return new ComponentDefinitionParser(_definition, _info);
      }

      String id = (String) _definition.getAttributes().get("id");
      
      if (id == null)
       {
         logError(context,
                 "The id attribute has not been set for the component \"" + _info.getComponentInfo(_type).componentType +"\"",
                 null);
      return null;
    }
      else
       {
          return createTestParser(_info.getComponentInfo(_type), localName, id);
       }
    }

    @Override
    public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child) throws SAXParseException
  {
    if (child instanceof TestScript.Test)
    {
      _script.getTests().add((TestScript.Test)child);
    }
  }


    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      return _definition;
    }

    private ComponentDefinition _definition;
    private ComponentDefinition _parent;
    private FacesConfigInfo     _info;
    private String              _type;
  }


  private class AttributeTestParser extends BaseNodeParser
  {
    public AttributeTestParser(FacesConfigInfo.ComponentInfo componentInfo, String componentId)
    {
      _componentInfo = componentInfo;
      _componentId = componentId;
    }

    @Override
    public void startElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      boolean matchesBase = "true".equals(attrs.getValue("matchesBase"));

      String name = getRequiredAttribute(context, attrs, "name");
      String valueStr = attrs.getValue("value");

      FacesConfigInfo.PropertyInfo property = _componentInfo.getPropertyInfo(name);
      final Object value;
      if (!ComponentDefinition.isValueExpression(valueStr) &&
          (property != null) && (property.type != null))
        value = Coercions.coerce(null, valueStr, property.type);
      else
        value = valueStr;

      _test = new TestScript.AttributeTest(name,
                                           value,
                                           matchesBase,
                                           null, 
                                           _componentId);
    }

    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      if ("attribute-test".equals(localName))
        return new AttributeTestParser(_componentInfo, _componentId);
      return null;
    }

    @Override
    public void addCompletedChild(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Object       child) throws SAXParseException
    {
      if (child instanceof TestScript.Test)
      {
        _test.addDelegate((TestScript.Test) child);
      }
    }

    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      return _test;
    }

    private TestScript.AttributeTest        _test;
    private FacesConfigInfo.ComponentInfo   _componentInfo;
    private String  _componentId;
  }


  private class EnumTestParser extends BaseNodeParser
  {
    public EnumTestParser(FacesConfigInfo.ComponentInfo info)
    {
      _componentInfo = info;
    }

    @Override
    public void startElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      _name = attrs.getValue("name");
      _propertyInfo = _componentInfo.getPropertyInfo(_name);
      if (_propertyInfo == null)
        logError(context,
                 "Couldn't find property info for \"" + _name +"\"",
                 null);
      else if (_propertyInfo.enumValues == null)
        logError(context,
                 "Property \"" + _name +"\" is not an enumeration",
                 null);

      _defaultValue = attrs.getValue("default");
      if (_defaultValue == null)
      {
        _defaultValue = _propertyInfo.defaultValue;
        /* No default presumably means that null is legit, and that
           none of the values should "match base"
        if (defaultValue == null)
          logError(context, "Property \"" + _name + "\" does not have a " +
            "default value",
            null);
        */
      }
    }

    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      return createTestParser(_componentInfo, localName, null);
    }

    @SuppressWarnings("unchecked")
    @Override
    public void addCompletedChild(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Object       child) throws SAXParseException
    {
      if (child instanceof TestScript.Test)
      {
        _childTests.add((TestScript.Test)child);
      }
      else if (child instanceof List)
      {
        _childTests.addAll((List<TestScript.Test>) child);
      }
    }

    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      ArrayList<TestScript.AttributeTest> allTests = 
        new ArrayList<TestScript.AttributeTest>();

      Iterator<Object> values = _propertyInfo.enumValues.iterator();
      while (values.hasNext())
      {
        Object value = values.next();
        boolean isDefault = value.equals(_defaultValue);
        allTests.add(new TestScript.AttributeTest(_name,
                                                  value,
                                                  isDefault));
        Iterator<TestScript.Test> childTests = _childTests.iterator();
        while (childTests.hasNext())
        {
          TestScript.Test childTest = childTests.next();
          allTests.add(new TestScript.AttributeTest(_name,
                                                    value,
                                                    isDefault,
                                                    childTest));
        }
      }

      return allTests;
    }

    private FacesConfigInfo.ComponentInfo   _componentInfo;
    private FacesConfigInfo.PropertyInfo    _propertyInfo;
    private String _name;
    private Object _defaultValue;
    private List<TestScript.Test> _childTests = new ArrayList<TestScript.Test>();
  }

  private class JavascriptTestParser extends BaseNodeParser
  {
    public JavascriptTestParser(FacesConfigInfo.ComponentInfo info)
    {
      _componentInfo = info;
    }

    @Override
    public void startElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      Iterator<String> properties = _componentInfo.properties.keySet().iterator();
      while (properties.hasNext())
      {
        String name = properties.next();
        if (name.startsWith("on"))
        {
          _tests.add(new TestScript.AttributeTest(name, null, false));
        }
      }
    }

    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      return _tests;
    }

    private FacesConfigInfo.ComponentInfo   _componentInfo;
    private List<TestScript.AttributeTest> _tests = 
      new ArrayList<TestScript.AttributeTest>();
  }

  private class BooleanTestParser extends BaseNodeParser
  {
    public BooleanTestParser(FacesConfigInfo.ComponentInfo info)
    {
      _componentInfo = info;
    }

    @Override
    public void startElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      _name = attrs.getValue("name");
      FacesConfigInfo.PropertyInfo property = _componentInfo.getPropertyInfo(_name);
      if (property == null)
        logError(context,
                 "Couldn't find property info for \"" + _name +"\"",
                 null);
      else if (property.type != Boolean.TYPE)
        logError(context,
                 "Property \"" + _name +"\" is not a boolean type",
                 null);

      String defaultStr = attrs.getValue("default");
      if (defaultStr != null)
      {
        _defaultValue = "true".equals(defaultStr);
      }
      else
      {
        _defaultValue = Boolean.TRUE.equals(property.defaultValue);
      }

      // For some reason, people keep thinking that "matchesBase" is on
      //
      String matchesBase = attrs.getValue("matchesBase");
      if (matchesBase != null)
        logError(context,
                 "\"matchesBase\" is not an attribute of &lt;boolean-test&gt;",
                 null);

    }

    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      return createTestParser(_componentInfo, localName, null);
    }

    @SuppressWarnings("unchecked")
    @Override
    public void addCompletedChild(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Object       child) throws SAXParseException
    {
      if (child instanceof TestScript.Test)
      {
        _childTests.add((TestScript.Test)child);
      }
      else if (child instanceof List)
      {
        _childTests.addAll((List<TestScript.Test>) child);
      }
    }

    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      _addTests(false);
      _addTests(true);
      return _tests;
    }

    private void _addTests(boolean value)
    {
      boolean matchesBase = (value == _defaultValue);
      Boolean valueObj    = value ? Boolean.TRUE : Boolean.FALSE;
      _tests.add(new TestScript.AttributeTest(_name, valueObj, matchesBase));
      Iterator<TestScript.Test> iter = _childTests.iterator();
      while (iter.hasNext())
      {
        TestScript.Test test = iter.next();
        // Don't bother testing and re-testing that an attribute
        // matches the base in all sorts of combos;  keep it simple!
        if (matchesBase)
          _tests.add(test);
        else
          _tests.add(new TestScript.AttributeTest(_name,
                                                  valueObj,
                                                  false,
                                                  test));
      }
    }

    private String _name;
    private boolean _defaultValue;
    private FacesConfigInfo.ComponentInfo   _componentInfo;
    private List<TestScript.Test> _tests = new ArrayList<TestScript.Test>();
    private List<TestScript.Test> _childTests = new ArrayList<TestScript.Test>();
  }

  private class MessageParser extends BaseNodeParser
  {
    public MessageParser(FacesConfigInfo.ComponentInfo info)
    {
      _componentInfo = info;
    }

    @Override
    public void startElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      String summary = attrs.getValue("summary");
      String detail = attrs.getValue("detail");
      String clientId = attrs.getValue("clientId");

        // If a summary or detail is set, add a message
        String severity = attrs.getValue("severity");
        if (severity == null)
          severity = "INFO";
        else
          severity = severity.toUpperCase();

        if (summary == null)
          summary = severity + " message summary";
        if (detail == null)
          detail = severity + " message detail";

      _test = new TestScript.MessageTest(severity, summary, detail,
                                         clientId);
      }

    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      return _test;
    }

    // -= Simon Lessard =- 
    // TODO: Never read locally as of 2006-08-09. Delete whenever 
    //       this attribute prove to be useless.
    //private FacesConfigInfo.ComponentInfo _componentInfo;
    private TestScript.Test _test;
  }

  private FacesConfigInfo _info;
  private TestScript      _script;
  private FacesConfigInfo.ComponentInfo   _componentInfo;
}
