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

import java.io.StringWriter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.model.SortCriterion;

import org.apache.myfaces.trinidad.context.Agent;

public class TestScript
{
  public TestScript()
  {
    _agentTypes.add(Agent.TYPE_DESKTOP);
    _agentTypes.add(Agent.TYPE_PDA);
  }

  public boolean isSupportedAgentType(Object agentType)
  {
    return _agentTypes.contains(agentType);
  }

  public void removeAgentType(Object agentType)
  {
    _agentTypes.remove(agentType);
  }

  public void setDefinition(ComponentDefinition cd)
  {
    _definition = cd;
    if (cd.getAttributes().get("id") == null)
      cd.getAttributes().put("id", "mainId");
  }

  public ComponentDefinition getDefinition()
  {
    return _definition;
  }

  public List<Test> getTests()
  {
    return _tests;
  }

  static abstract public class Test
  {
    abstract public void apply(FacesContext context, UIComponent component);
    abstract public boolean shouldMatchBase();
    public StringWriter getOutput()
    {
      return _output;
    }

    private StringWriter _output = new StringWriter();
  }

  
  static public class MessageTest extends Test
  {
    public MessageTest(String severity, String summary, String detail,
                       String clientId)
    {
      FacesMessage.Severity sev = FacesMessage.SEVERITY_INFO;

      if (severity != null)
      {
        if ("ERROR".equals(severity))
          sev = FacesMessage.SEVERITY_ERROR;
        else if ("WARN".equals(severity) || "WARNING".equals(severity))
          sev = FacesMessage.SEVERITY_WARN;
        else if ("FATAL".equals(severity))
          sev = FacesMessage.SEVERITY_FATAL;
      }
      
      _messages.put(clientId, new FacesMessage(sev, summary, detail));
    }

    @Override
    public void apply(FacesContext context, UIComponent component)
    {
      // first, cleanup any messages left over from a previous test
      // ((MFacesContext) context).clearMessages();

      // Add any messages needed to run this suite of tests
      Iterator<String> messageIds = _messages.keySet().iterator();
      while (messageIds.hasNext())
      {
        String id = messageIds.next();
        FacesMessage fm = _messages.get(id);
        context.addMessage(id, fm);
      }
    }

    @Override
    public boolean shouldMatchBase()
    {
      return true;
    }

    @Override
    public String toString()
    {
      StringBuffer sb = new StringBuffer(100);
      sb.append(" MessageTest[ ");

      boolean gotFirst = false;
      Iterator<String> messageIds = _messages.keySet().iterator();
      while (messageIds.hasNext())
      {
        String id = messageIds.next();
        FacesMessage fm = _messages.get(id);

        // Severity toString() implementation is different in the
        // RI and MyFaces;  in the RI it's "name ordinal",
        // and in MyFaces it's just "name".  And in the RI,
        // the names are uppercase, whereas they're mixed case
        // in MyFaces.
        String sevStr = fm.getSeverity().toString().toUpperCase();
        int space = sevStr.indexOf(" ");
        if (!gotFirst)
          gotFirst = true;
        else
          sb.append(", ");
        sb.append("id:" + id + ",");
        if (space < 0)
          sb.append(sevStr);
        else
          sb.append(sevStr.substring(0,space));
      }

      sb.append(" ]");
      return sb.toString();
    }

    private Map<String, FacesMessage> _messages = 
      new LinkedHashMap<String, FacesMessage>();
  }

  static public class AttributeTest extends Test
  {
    public AttributeTest(String name, Object value, boolean matchesBase)
    {
      this(name, value, matchesBase, null, null);
    }
    
    public AttributeTest(String name, 
                        Object value, 
                        boolean matchesBase, 
                        Test    delegateTest)
    {
      this(name, value, matchesBase, delegateTest, null);
    }

    public AttributeTest(String name,
                         Object value,
                         boolean matchesBase,
                         Test    delegateTest,
                         String  componentId)
    {
      _name = name;
      _value = value;
      _testComponentId = componentId;
      
      if (delegateTest != null)
      {
        matchesBase = (matchesBase && delegateTest.shouldMatchBase());
        addDelegate(delegateTest);
      }

      _matchesBase = matchesBase;
    }

    public void addDelegate(Test delegateTest)
    {
      _delegateTests.add(delegateTest);
    }

    @SuppressWarnings("unchecked")
    @Override
    public void apply(FacesContext context, UIComponent component)
    {
      Iterator<Test> tests = _delegateTests.iterator();
      
      if (_testComponentId != null)
        component = component.findComponent(_testComponentId); 
      
      while (tests.hasNext())
      {
        Test test = tests.next();
        test.apply(context, component);
      }
      
      Object value = _value;
      
         
      // Hack to test table sort. This is a custom attribute not renderer or component attribute
      if ("tableSortCriteria".equals(_name))
      {
        UIXCollection tableModel = (UIXCollection)  component;
        List<SortCriterion> sortCriteriaList = new ArrayList<SortCriterion>();
        String sortCriterion = (String) _value;
        Boolean isAscending  = 
           new Boolean(sortCriterion.substring(sortCriterion.indexOf(" ")).trim());
        sortCriteriaList.add(new SortCriterion(
              sortCriterion.substring(0,sortCriterion.indexOf(" ")), 
              isAscending.booleanValue()));
        tableModel.setSortCriteria(sortCriteriaList);
      }
      else
      {      
      if ((value instanceof String) &&
          ComponentDefinition.isValueExpression(value.toString()))
      {
        ValueBinding binding = context.getApplication().
          createValueBinding(value.toString());
        component.setValueBinding(_name, binding);
      }
      else
      {
        if (value == null)
          value = "test-" + _name;

        component.getAttributes().put(_name, value);
      }
      }
    }

    @Override
    public boolean shouldMatchBase()
    {
      return _matchesBase;
    }
    
    public String getName()
    {
      return _name;
    }
    
    public Object getValue()
    {
      return _value;
    }
    
    @Override
    public String toString()
    {
      String valStr;
      if (_value instanceof Object[])
      {
        boolean gotFirst = false;
        StringBuffer sb = new StringBuffer(1000);
        sb.append("[");
        for (int i = 0; i < ((Object[])_value).length; i++)
        {
          Object curr = ((Object[])_value)[i];

          if (gotFirst)
          {
            sb.append(",");
          }
          else
          {
            gotFirst = true;
          }
          sb.append(curr);
        }
        sb.append("]");
        valStr = sb.toString();
      }
      else if (_value != null)
      {
        valStr = _value.toString();
      }
      else
        valStr = "null";


      if (_delegateTests.isEmpty())
      {
        return "AttributeTest[" + _name + "," + valStr + "]";
      }
      else
      {
        StringBuffer buffer = new StringBuffer();
        buffer.append("AttributeTest[" + _name + "," + valStr);
        Iterator<Test> tests = _delegateTests.iterator();
        while (tests.hasNext())
        {
          buffer.append(',');
          buffer.append(tests.next());
        }

        buffer.append("]");
        return buffer.toString();
      }
    }

    private String     _name;
    private Object     _value;
    private String     _testComponentId;
    private boolean    _matchesBase;
    private List<Test> _delegateTests = new ArrayList<Test>();
  }

  private Set<Object>         _agentTypes = new HashSet<Object>();
  private List<Test>          _tests      = new ArrayList<Test>();
  private ComponentDefinition _definition;
  
}
