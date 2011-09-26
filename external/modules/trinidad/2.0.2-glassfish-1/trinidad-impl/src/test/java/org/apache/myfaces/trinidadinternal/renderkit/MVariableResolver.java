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

import java.awt.Color;

import java.beans.IntrospectionException;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.el.VariableResolver;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.ChartModel;
import org.apache.myfaces.trinidad.model.ChildPropertyTreeModel;
import org.apache.myfaces.trinidad.model.DefaultBoundedRangeModel;
import org.apache.myfaces.trinidad.model.ProcessMenuModel;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetImpl;
import org.apache.myfaces.trinidad.model.RowKeySetTreeImpl;
import org.apache.myfaces.trinidad.model.TreeModel;

import org.apache.myfaces.trinidadinternal.renderkit.testData.Person;

public class MVariableResolver extends VariableResolver
{
  @Override
  public Object resolveVariable(FacesContext context, String name)
  {
    Object o =  context.getExternalContext().getRequestMap().get(name);
    if (o != null)
      return o;

    if ("pageList".equals(name))
    {
      try
      {
        if (_pageList == null)
        {
          List<PageImpl> al = _createPageList();
          _pageList = new MenuModelImpl(al, "viewId", "/1.jspx");
        }

        return _pageList;
      }
      catch(IntrospectionException e)
      {
        _LOG.severe(e);
        return null;
      }
    }
    else if ("menu".equals(name))
    {
      try
      {
        if (_menu == null)
        {
          List<PageImpl> al = _createPageList();
          TreeModel treeModel = new ChildPropertyTreeModel(al, "children");
          _menu = new MenuModelImpl(treeModel, "viewId", "/4.jspx");
        }

        return _menu;
      }
      catch(IntrospectionException e)
      {
        _LOG.severe(e);
        return null;
      }
    }
    else if ("navigationpath".equals(name))
    {
      try
      {
        if (_navigationpath == null)
        {
          List<PageImpl> al = _createPageList();
          TreeModel treeModel = new ChildPropertyTreeModel(al, "children");
          _navigationpath = new MenuModelImpl(treeModel, "viewId", "/7.jspx");
        }

        return _navigationpath;
      }
      catch(IntrospectionException e)
      {
        _LOG.severe(e);
        return null;
      }
    }
    else if ("simpleList".equals(name))
    {
      if (_simpleList == null)
      {
        _simpleList = new ArrayList<Map<String, Object>>();
        _simpleList.add(_createHashMap("First", 1));
        _simpleList.add(_createHashMap("Second", 2));
        _simpleList.add(_createHashMap("Three", 3));
        _simpleList.add(_createHashMap("Four", 4));
        _simpleList.add(_createHashMap("Five", 5));
        _simpleList.add(_createHashMap("Six", 6));
      }

      return _simpleList;
    }
    else if ("numberList".equals(name))
    {
      // A simple out-of-order list of numeric strings,
      // for use in the selectManyListbox test
      List<String> list = new ArrayList<String>(2);
      list.add("8");
      list.add("2");
      return list;
    }
    else if("iteratorList".equals(name))
    {
      if (_iteratorList == null)
      {
        // -= Simon Lessard =-
        // FIXME: _iteratorList will contains 5 null value since 
        //        put return the value before the insertion at  
        //        specified key. 
        // Ref: http://java.sun.com/j2se/1.5.0/docs/api/java/util/Map.html#put(K, V)
        _iteratorList = new ArrayList<String>();
        _iteratorList.add(new HashMap<String, String>().put("data","One"));
        _iteratorList.add(new HashMap<String, String>().put("data","Two"));
        _iteratorList.add(new HashMap<String, String>().put("data","Three"));
        _iteratorList.add(new HashMap<String, String>().put("data","Four"));
        _iteratorList.add(new HashMap<String, String>().put("data","Five"));
      }

      return _iteratorList;
    }
    else if("colorList".equals(name))
    {
      ArrayList<Color> colorList = new ArrayList<Color>();
      colorList.add(new Color(255, 0, 0));
      colorList.add(new Color(0, 0, 255));
      colorList.add(new Color(255, 255, 0));
      colorList.add(new Color(0, 255, 0));
      return colorList;
    }
    else if("treeModel".equals(name))
    {
      if (_treeModel == null)
      {
        Person john = new Person("John Smith");
        Person kim = new Person("Kim Smith");
        Person tom = new Person("Tom Smith");
        Person zoe = new Person("Zoe Smith");
        Person ira = new Person("Ira Wickrememsinghe");
        Person mallika = new Person("Mallika Wickremesinghe");

        john.getKids().add(kim);
        john.getKids().add(tom);
        tom.getKids().add(zoe);
        ira.getKids().add(mallika);

        List<Person> people = new ArrayList<Person>();
        people.add(john);
        people.add(ira);

        _treeModel = new ChildPropertyTreeModel(people, "kids");
      }

      return _treeModel;
    }
    else if("pathSet".equals(name))
    {
      if (_treeState == null)
      {
        _treeState = new RowKeySetTreeImpl(true);

      }
      return _treeState;
    }
    else if("disclosureState".equals(name))
    {
      if(_disclosureState == null)
        _disclosureState = new RowKeySetImpl(true);
      return _disclosureState;
    }
    else if("arrayString".equals(name))
    {
      String stringArray[] = {"id1","id2","id3"};
      return stringArray;
    }
    else if("rangeModel".equals(name))
    {
      return new DefaultBoundedRangeModel(2,4);
    }
    else if ("bigList".equals(name))
    {
      return new BigList();
    }
    else if("rangeChoiceBarModel".equals(name))
    {
      return new RangeChoiceBarModelImpl();
    }
    else if ("oldDate".equals(name))
    {
      // FIXME: Should be able to use Calendar for non deprecated calls.
      return new Date(70, 5, 10);
    }
    else if ("midDate".equals(name))
    {
      // FIXME: Should be able to use Calendar for non deprecated calls.
      return new Date(105, 6, 27);
    }
    else if ("newDate".equals(name))
    {
      // FIXME: Should be able to use Calendar for non deprecated calls.
      return new Date(130, 0, 5);
    }
    else if ("currTime".equals(name))
    {
      return new Date(109,0,1);
    }
    else if("simpleDocument".equals(name))
    {
      return "First paragraph followed by \\r\r" + 
             "Second paragraph followed by \\n\n" + 
             "Third paragraph followed by \\n\\r\n\r" + 
             "Fourth paragraph";
    }
    else if("chartModel".equals(name))
    {
      return new MyChartModel();
    }
    return null;
  }

  public class MenuModelImpl extends ProcessMenuModel
  {
    public MenuModelImpl(
      Object instance,
      String viewIdProperty,
      String currentViewId
    )throws IntrospectionException
    {
      super(instance, viewIdProperty, null);
      _currentViewId = currentViewId;
    }

    @Override
    protected String getCurrentViewId()
    {
      return _currentViewId;
    }

    private String _currentViewId;
  }

  static private Map<String, Object> _createHashMap(String s, int i)
  {
    HashMap<String, Object> m = new HashMap<String, Object>();
    m.put("string", s);
    m.put("int", new Integer(i));
    return m;
  }

  public static class PageImpl
  {
    public PageImpl(String viewId, String label, boolean disabled)
    {
      _viewId = viewId;
      _label = label;
      _disabled = disabled;
    }

    public void setViewId(String viewId)
    {
      _viewId = viewId;
    }

    public void setLabel(String label)
    {
      _label = label;
    }

    public void setDisabled(boolean disabled)
    {
      _disabled = disabled;
    }

    public void setChildren(List<Object> children)
    {
      _children = children;
    }

    public String getViewId()
    {
      return _viewId;
    }

    public String getLabel()
    {
      return _label;
    }

    public boolean isDisabled()
    {
      return _disabled;
    }

    public List<Object> getChildren()
    {
      return _children;
    }

    private String _viewId;
    private String _label;
    private boolean _disabled;
    private List<Object> _children;
  }

  public static class BigList extends AbstractList<Integer>
  {
    @Override
    public int size()
    {
      return 10000;
    }

    @Override
    public Integer get(int i)
    {
      return new Integer(i);
    }
  }

  private static List<PageImpl> _createPageList()
  {
    ArrayList<PageImpl> al = new ArrayList<PageImpl>();
    PageImpl page1 = new PageImpl("/1.jspx", "First", false);

    al.add(page1);
    al.add(new PageImpl("/2.jspx", "Second", false));
    al.add(new PageImpl("/3.jspx", "Third", false));

    PageImpl page5 = new PageImpl("/5.jspx", "fifth", false);
    ArrayList<Object> p1 = new ArrayList<Object>();
    p1.add(new PageImpl("/4.jspx", "fourth", false));
    p1.add(page5);

    ArrayList<Object> p2 = new ArrayList<Object>();
    p2.add(new PageImpl("/6.jspx", "sixth", false));
    p2.add(new PageImpl("/7.jspx", "seventh", false));

    page1.setChildren(p1);
    page5.setChildren(p2);

    return al;
  }

  private static class RangeChoiceBarModelImpl
  {
    private List<String> _names;
    private int _start;
    private int _end;

    public RangeChoiceBarModelImpl()
    {
      _names = new ArrayList<String>();
      _names.add("vox");
      _names.add("populi");
      _names.add("en");
      _names.add("vogue");
    }
    public void setNames(List<String> names)
    {
      this._names = names;
    }

    public List<String> getNames()
    {
      return _names;
    }

    public void setStart(int start)
    {
      this._start = start;
    }

    public int getStart()
    {
      return _start;
    }

    public void setEnd(int end)
    {
      this._end = end;
    }

    public int getEnd()
    {
      return _end;
    }
  }

  private class MyChartModel extends ChartModel
  {
    @Override
    public List<String> getSeriesLabels()
    {
      return _seriesLabels;
    }

    @Override
    public List<String> getGroupLabels()
    {
      return _groupLabels;
    }
        
    @Override
    public List<List<Double>> getXValues()
    {
      return _chartXValues;
    }
  
    @Override
    public List<List<Double>> getYValues()
    {
      return _chartYValues;
    }
  
    @Override
    public Double getMaxYValue()
    {
      return 500000.0;
    }
  
  
    @Override
    public Double getMinYValue()
    {        
      return 0.0; 
    }
  
  
    @Override
    public Double getMaxXValue()
    {
      return 10.0; 
    }
  
  
    @Override
    public Double getMinXValue()
    {
      return 6.0; 
    }
  
  
    @Override
    public String getTitle()
    {
      return "Title";
    }
  
    @Override
    public String getSubTitle()
    {
      return "SubTitle"; 
    }
  
  
    @Override
    public String getFootNote()
    {
      return "FootNote"; 
    }
    
    private final List<String> _groupLabels = 
      Arrays.asList(new String[]{"June", "July", "August", "September","October"});

    private final List<String> _seriesLabels = 
      Arrays.asList(new String[]{"Previous", "Target", "Actual"});
    
    private final ArrayList<List<Double>> _chartYValues;
    private final ArrayList<List<Double>> _chartXValues; 
    {
      _chartYValues = new ArrayList<List<Double>>();
      _chartYValues.add(Arrays.asList(new Double[]{135235., 155535., 141725.}));
      _chartYValues.add(Arrays.asList(new Double[]{106765., 131725., 127868.}));
      _chartYValues.add(Arrays.asList(new Double[]{108456., 119326., 139326.}));
      _chartYValues.add(Arrays.asList(new Double[]{136765., 147265., 184349.})); 
      _chartYValues.add(Arrays.asList(new Double[]{107868., 113968., 174349.}));

      _chartXValues = new ArrayList<List<Double>>();
      _chartXValues.add(Arrays.asList(new Double[]{6.1, 6.3, 6.5}));
      _chartXValues.add(Arrays.asList(new Double[]{6.8, 7.1, 7.3}));
      _chartXValues.add(Arrays.asList(new Double[]{7.6, 7.8, 8.0}));
      _chartXValues.add(Arrays.asList(new Double[]{8.25, 8.55, 8.78}));
      _chartXValues.add(Arrays.asList(new Double[]{9.23, 9.48, 9.88}));
    } 
  }
  private MenuModelImpl _pageList;
  private MenuModelImpl _menu;
  private MenuModelImpl _navigationpath;

  private List<String> _iteratorList;
  private List<Map<String, Object>> _simpleList;

  private ChildPropertyTreeModel _treeModel;

  private RowKeySet _treeState = null;
  private RowKeySet _disclosureState = null;

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(MVariableResolver.class);
}
