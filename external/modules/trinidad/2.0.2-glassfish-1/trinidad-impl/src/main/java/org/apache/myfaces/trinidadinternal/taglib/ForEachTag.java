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
package org.apache.myfaces.trinidadinternal.taglib;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.el.ELContext;
import javax.el.PropertyNotWritableException;
import javax.el.ValueExpression;
import javax.el.VariableMapper;
import javax.faces.context.FacesContext;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspTagException;
import javax.servlet.jsp.jstl.core.IndexedValueExpression;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.webapp.TrinidadTagSupport;

//JSTL Core Library - <c:forEach> Tag
//===================================
//Syntax 1: Iterate over a collection of objects
//
//<c:forEach [var="varName "] items="collection"
//  [varStatus="varStatusName"]
//  [begin="begin"] [end="end"] [step=" step"]>
//  body content
//</c:forEach>
//
//Syntax 2: Iterate a fixed number of times
//
//<c:forEach [var="varName"]
//  [varStatus="varStatusName"]
//  begin=" begin" end="end" [step="step"]>
//  body content
//</c:forEach>

/**
 *
 */
public class ForEachTag extends TrinidadTagSupport
{
  public void setItems(ValueExpression items)
  {
    if (items.isLiteralText())
      throw new IllegalArgumentException(_LOG.getMessage(
        "MUST_BE_SIMPLE_JSF_EL_EXPRESSION"));
    _items = items;
  }

  public void setBegin(ValueExpression begin)
  {
    _beginVE = begin;
  }

  public void setEnd(ValueExpression end)
  {
    _endVE = end;
  }

  public void setStep(ValueExpression step)
  {
    _stepVE = step;
  }

  public void setVar(String var)
  {
    _var = var;
  }

  public void setVarStatus(String varStatus)
  {
    _varStatus = varStatus;
  }

  @Override
  public int doStartTag() throws JspException
  {
    _validateAttributes();

    FacesContext context = FacesContext.getCurrentInstance();
    _currentBegin = (_begin == null) ? 0 : _begin.intValue();
    _isFirst = true;
    int length;

    if (null != _items)
    {
      // AdamWiner: for reasons I cannot yet explain, using the JSP's
      // ELContext is giving me big problems trying to grab Lists
      // from inside of managed beans.  Switching this one call
      // to the JSF ELContext seems to resolve that.  We certainly
      // have to use the JSPs ELResolver for calling through
      // to the VariableMapper
      Object items = _items.getValue(context.getELContext());//pageContext.getELContext());

      //pu: If items is specified and resolves to null, it is treated as an
      //  empty collection, i.e., no iteration is performed.
      if (items == null)
      {
        if (_LOG.isFine())
          _LOG.fine("Items expression " + _items + " resolved to null.");
        return SKIP_BODY;
      }

      _itemsValue = items;
      // =-=AEW <c:forEach> supports arbitrary collections;  but
      // JSF only supports List in its EL.
      if (items instanceof List)
        length = ((List) items).size();
      else if (items.getClass().isArray())
        length = Array.getLength(items);
      else
        throw new JspException(_LOG.getMessage(
          "MUST_POINT_TO_LIST_OR_ARRAY"));
      if (length == 0)
      {
        if (_LOG.isFine())
          _LOG.fine("Items found at " + _items + " is empty.");
        return SKIP_BODY;
      }
      //pu: If valid 'items' was specified, and so was 'begin', get out if size
      //  of collection were to be less than the begin. A mimic of c:forEach.
      if (length < _currentBegin)
      {
        if (_LOG.isFine())
          _LOG.fine("Size of 'items' is less than 'begin'");
        return SKIP_BODY;
      }

      _currentEnd = (_end == null) ? length - 1 : _end.intValue();
      //pu: If 'end' were specified, but is beyond the size of collection, limit
      //  the iteration to where the collection ends. A mimic of c:forEach and
      //  fix for bug 4029853.
      if (length < _currentEnd)
        _currentEnd = length - 1;
    }
    else
    {
      _currentEnd = (_end == null) ? 0 : _end.intValue();
    }
    _currentIndex = _currentBegin;
    _currentCount = 1;
    _currentStep = (_step == null) ? 1 : _step.intValue();
    //pu: Now check the valid relation between 'begin','end' and validity of 'step'
    _validateRangeAndStep();
    
    // If we can bail, do it now
    if (_currentEnd < _currentIndex)
      return SKIP_BODY;

    _isLast = _currentIndex == _currentEnd;

    // Save off the previous deferred variables
    VariableMapper vm = 
      pageContext.getELContext().getVariableMapper();
    if (_var != null)
      _previousDeferredVar = vm.resolveVariable(_var);

    if (null != _varStatus)
    {
      _previousDeferredVarStatus = vm.resolveVariable(_varStatus);
      _propertyReplacementMap = new HashMap<String, Object>(9, 1);
      _propertyReplacementMap.put("begin", Integer.valueOf(_currentBegin));
      _propertyReplacementMap.put("end", Integer.valueOf(_currentEnd));
      _propertyReplacementMap.put("step", Integer.valueOf(_currentStep));
      _propertyReplacementMap.put("count", Integer.valueOf(_currentCount));
      _propertyReplacementMap.put("index", Integer.valueOf(_currentIndex));
      // FIXME: Can we support "current" efficiently?
      //      _propertyReplacementMap.put("current", _varReplacement);
      _propertyReplacementMap.put(
        "first",
        (_isFirst)? Boolean.TRUE:Boolean.FALSE);
      _propertyReplacementMap.put(
        "last",
        (_isLast)? Boolean.TRUE:Boolean.FALSE);
    }

    if (_LOG.isFiner())
    {
      _LOG.finer("Iterating from " + _currentIndex + " to " + _currentEnd +
                 " by " + _currentStep);
    }

    // Update the variables
    _updateVars();

    return EVAL_BODY_INCLUDE;
  }

  @Override
  public int doAfterBody()
  {
    _currentIndex += _currentStep;
    _currentCount += 1;

    //pu: if there is no varStatus set, no point in keeping loop status
    //  variables updated.
    if (null != _varStatus)
    {
      if (_isFirst)
      {
        _propertyReplacementMap.put("first", Boolean.FALSE);
        _isFirst = false;
      }

      _isLast = (_currentIndex == _currentEnd);
      if (_isLast)
      {
        _propertyReplacementMap.put("last", _isLast);
      }
      _propertyReplacementMap.put("count", Integer.valueOf(_currentCount));
      _propertyReplacementMap.put("index", Integer.valueOf(_currentIndex));
      // FIXME: Can we support "current" efficiently?
      //      _propertyReplacementMap.put("current", _varReplacement);
    }

    // If we're at the end, bail
    if (_currentEnd < _currentIndex)
    {
      // Restore EL state
      VariableMapper vm = 
        pageContext.getELContext().getVariableMapper();
      if (_var != null)
        vm.setVariable(_var, _previousDeferredVar);
      if (_varStatus != null)
        vm.setVariable(_varStatus, _previousDeferredVarStatus);

      return SKIP_BODY;
    }
    
    // Otherwise, update the variables and go again
    _updateVars();

    return EVAL_BODY_AGAIN;
  }

  /**
   * Release state.
   */
  @Override
  public void release()
  {
    super.release();
    _begin = null;
    _end = null;
    _step = null;
    _items = null;
    _itemsValue = null;
    _var = null;
    _varStatus = null;
    _propertyReplacementMap = null;
    _previousDeferredVar = null;
    _previousDeferredVarStatus = null;
  }

  // Push new values into the VariableMapper and the pageContext
  private void _updateVars()
  {
    VariableMapper vm = 
      pageContext.getELContext().getVariableMapper();
    if (_var != null)
    {
      // Catch programmer error where _var has been set but
      // _items has not
      if (_items != null)
      {
        ValueExpression iterated = new IndexedValueExpression(_items,
                                                              _currentIndex);
        vm.setVariable(_var, iterated);
      }
      
      // Ditto (though, technically, one check for
      // _items is sufficient, because if _items evaluated
      // to null, we'd skip the whole loop)
      Object items = _itemsValue;
      if (items != null)
      {
        Object item;
        if (items instanceof List)
          item = ((List) items).get(_currentIndex);
        else
          item = Array.get(items, _currentIndex);

        pageContext.setAttribute(_var, item);
      }
    }
    
    if (_varStatus != null)
    {
      pageContext.setAttribute(_varStatus, _propertyReplacementMap);
      ValueExpression constant = new Constants(
                                      new HashMap(_propertyReplacementMap));
      vm.setVariable(_varStatus, constant);
    }
  }

  private Integer _evaluateInteger(
    FacesContext context,
    ValueExpression ve)
  {
    if (ve == null)
      return null;

    Object val = ve.getValue(context.getELContext());
    if (val instanceof Integer)
      return (Integer) val;
    else if (val instanceof Number)
      return Integer.valueOf(((Number) val).intValue());

    return null;
  }

  private void _validateAttributes() throws JspTagException
  {
    // Evaluate these three ValueExpressions into integers
    // For why we use FacesContext instead of PageContext, see
    // above (the evaluation of _items)
    FacesContext context = FacesContext.getCurrentInstance();
    _end = _evaluateInteger(context, _endVE);
    _begin = _evaluateInteger(context, _beginVE);
    _step = _evaluateInteger(context, _stepVE);

    if (null == _items)
    {
      if (null == _begin || null == _end)
      {
        throw new JspTagException(
          "'begin' and 'end' should be specified if 'items' is not specified");
      }
    }
    //pu: This is our own check - c:forEach behavior un-defined & unpredictable.
    if ((_var != null) &&
        _var.equals(_varStatus))
    {
      throw new JspTagException(
        "'var' and 'varStatus' should not have the same value");
    }
  }

  private void _validateRangeAndStep() throws JspTagException
  {
    if (_currentBegin < 0)
      throw new JspTagException("'begin' < 0");
    if (_currentStep < 1)
      throw new JspTagException("'step' < 1");
  }

  // Basic ValueExpression that always returns a constant object
  static private class Constants extends ValueExpression
                                 implements Serializable
  {
    public Constants(Object o)
    {
      _o = o;
    }

    public Object getValue(ELContext context)
    {
      return _o;
    }

    public void setValue(ELContext context, Object value)
    {
      throw new PropertyNotWritableException();
    }

    public boolean isReadOnly(ELContext context)
    {
      return true;
    }

    public Class getType(ELContext context)
    {
      return _o.getClass();
    }

    public Class getExpectedType()
    {
      return _o.getClass();
    }

    public String getExpressionString()
    {
      return null;
    }

    public boolean equals(Object obj)
    {
      return obj == this;
    }

    public int hashCode()
    {
      return _o.hashCode();
    }

    public boolean isLiteralText()
    {
      return true;
    }

    private Object _o;    
    private static final long serialVersionUID = 1L;
  }

  private int _currentBegin;
  private int _currentIndex;
  private int _currentEnd;
  private int _currentStep;
  private int _currentCount;
  private boolean _isFirst;
  private boolean _isLast;


  private ValueExpression _items;
  private Object          _itemsValue;

  private ValueExpression _beginVE;
  private ValueExpression _endVE;
  private ValueExpression _stepVE;

  private Integer _begin;
  private Integer _end;
  private Integer _step;

  private String _var;
  private String _varStatus;
  
  // Saved values on the VariableMapper
  private ValueExpression _previousDeferredVar;
  private ValueExpression _previousDeferredVarStatus;

  // Map for properties referred off from 'varStatus' and their replacements
  private Map<String, Object> _propertyReplacementMap;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ForEachTag.class);
  private static final long serialVersionUID = 1L;
}
