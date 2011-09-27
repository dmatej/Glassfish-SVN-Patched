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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.Scriptlet;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.XhtmlScriptletFactory;

/**
 * Public utility methods useful for working with AutoSubmits
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/AutoSubmitUtils.java#0 $) $Date: 10-nov-2005.19:01:20 $
 */
public class AutoSubmitUtils
{
  public static void writeDependencies(
    FacesContext        context,
    RenderingContext arc
    ) throws IOException
  {
    // Render our scriptlet
    XhtmlUtils.addLib(context, arc, _AUTO_SUBMIT_SCRIPTLET);
  }

  /**
   * Find all UIParameter children of a component, and
   * return it as a JS string of "name1:value1,name2:value2".
   */
  @SuppressWarnings("unchecked")
  public static String getParameters(UIComponent comp)
  {
    int childCount = comp.getChildCount();
    if (childCount == 0)
      return null;
      
    StringBuilder builder = null;
    for(UIComponent child : (List<UIComponent>)comp.getChildren())
    {
      if (child instanceof UIParameter)
      {
        UIParameter param = (UIParameter) child;
        String name = param.getName();
        Object value = param.getValue();
        if ((value == null) || (name == null))
          continue;

        if (builder == null)
          builder = new StringBuilder();
          
        // Add a comma if needed
        if (builder.length() > 0)
          builder.append(',');
        
        // Add the name and value, and in both cases
        // wrap in single quotes and escape it - we don't
        // know for sure if the name will be a legit JS identifier
        builder.append('\'');
        builder.append(XhtmlUtils.escapeJS(name));
        builder.append("':'");
        builder.append(XhtmlUtils.escapeJS(value.toString()));
        builder.append('\'');
      }
    }
    
    if (builder == null)
      return null;
      
    return builder.toString();
  }
    
  public static String getFullPageSubmitScript(
     RenderingContext arc,
     String              source,
     boolean             immediate,
     String              event,
     String              extraParams,
     boolean             returnTrue)
  {
    FormData fData = arc.getFormData();
    if (fData == null)
      return null;

    String formName = fData.getName();
    if (formName == null)
      return null;


    String startString = _FULL_PAGE_START;
    String endString = (returnTrue ? _TRUE_END : _FALSE_END);

    int length = (startString.length()
                  + formName.length()
                  + (event == null ? 0 : event.length() + 9)
                  + (source.length() + 11)
                  + endString.length());

    if (extraParams != null)
    {
      length += (1 + extraParams.length());
    }

    StringBuilder builder = new StringBuilder(length);
    builder.append(startString);
    builder.append(formName);
    builder.append(immediate ? "',0," : "',1,");

    builder.append("{source:");
    _appendJSParameter(builder, source);

    if (event != null)
    {
      builder.append(",event:");
      _appendJSParameter(builder, event);
    }

    if (extraParams != null)
    {
      builder.append(",");
      builder.append(extraParams);
    }

    builder.append('}');
    builder.append(endString);

    return builder.toString();
  }

  /**
   * Returns a String value which can be used as the onclick handler for
   * an element which fires partial change events.
   *
   * @param destination The destination URL, which contains any
   *   event information, including the partialTargets parameter.
   */
  public static String getPartialGetScript(String destination)
  {
    // Pre-compute StringBuilder size
    int length = _FIRE_PARTIAL_CHANGE_START.length() +
                 _FIRE_PARTIAL_CHANGE_END.length()   +
                 destination.length();

    StringBuilder builder = new StringBuilder(length);
    builder.append(_FIRE_PARTIAL_CHANGE_START);
    builder.append(destination);
    builder.append(_FIRE_PARTIAL_CHANGE_END);

    return builder.toString();
  }

  public static String getSubmitScript(
     RenderingContext    arc,
     String              source,
     boolean             immediate
     )
  {
    return getSubmitScript(arc, source, null, immediate);
  }

  public static String getSubmitScript(
     RenderingContext    arc,
     String              source,
     String              event,
     boolean             immediate
     )
  {
    return getSubmitScript(arc, source, immediate, false, event, null, true);
  }

  public static String getSubmitScript(
     RenderingContext    arc,
     String              source,
     boolean             immediate,
     boolean             isRadio)
  {
    return getSubmitScript(arc, source, immediate, isRadio, null, null, true);
  }

  /**
   * TODO: remove "isRadio", which shouldn't be necessary
   */
  public static String getSubmitScript(
     RenderingContext    arc,
     String              source,
     boolean             immediate,
     boolean             isRadio,
     String              event,
     String              extraParams,
     boolean             returnTrue)
  {
    StringBuilder builder = new StringBuilder();

    // Get the formName
    FormData formData = arc.getFormData();
    if (formData == null)
      return null;

    String formName = formData.getName();
    if (formName == null)
      return null;

    builder.append("TrPage._autoSubmit('");
    builder.append(formName);
    builder.append("',");
    _appendJSParameter(builder, source);
    boolean isDesktop = (arc.getAgent().getType().equals(Agent.TYPE_DESKTOP));
    if (isDesktop)
    {
      builder.append(",event,");
    } 
    else  
    {
      builder.append(",null,");
    }
    builder.append(immediate ? "0" : "1");
    if (extraParams != null)
    {
      builder.append(",{");
      builder.append(extraParams);
      builder.append("}");
    }
    builder.append(returnTrue ? _TRUE_END : _FALSE_END);
    return builder.toString();
  }

  // Appends a parameter to a JavaScript function call builder
  private static void _appendJSParameter(
    StringBuilder builder,
    String value
    )
  {
    if (value == null)
    {
      builder.append("0");
    }
    else
    {
      // double escape in-quotes string
      // e.g. "\'" + escapeJS("a'b") + "\'" -> "\'a\\\'b\'"
      builder.append("\'");
      XhtmlUtils.escapeJS(builder, value, true, 2);
      builder.append("\'");
    }
  }

  // Scriptlet that renders the submitPartialUpdate script
  private static class AutoSubmitScriptlet extends Scriptlet
  {
    static public Scriptlet sharedInstance()
    {
      return _sInstance;
    }

    @Override
    public Object getScriptletKey()
    {
      return _AUTO_SUBMIT_SCRIPTLET;
    }

    @Override
    protected void outputScriptletContent(
      FacesContext context,
      RenderingContext arc)
      throws IOException
    {
      // Make sure we have a form
      if (arc.getFormData() == null)
        return;

      String formName = arc.getFormData().getName();
      if (formName == null)
        return;

      // We output a function "_adfspu" (ADF Submit Partial Update)
      // which takes the following arguments:
      // - f:  The form name
      // - v:  The validation flag
      // - e:  The event name
      // - s:  The source parameter
      // - o:  Object containing client-defined parameters

      // To support redirecting on IE, we need to check if the parent page is
      // actually responding to a partial update call, so we keep track of it
      // here. We don't worry about re-setting this to false because the only
      // time it comes into play is when the back button is causing a re-render
      // of the iframe and, hence, a re-execution of the redirect script. In
      // that case, we'll have a new javascript context, and _pprUpdateMode will
      // be invalid, or will be initialized to false.

      ResponseWriter writer = context.getResponseWriter();
      writer.writeText("var _pprUpDatemode=false;", null);
      writer.writeText("function _adfspu(f,v,e,s,o){", null);
      writer.writeText("_pprUpdateMode=true;", null);
      writer.writeText("if(!o)o=new Object();if(e)o.", null);
      writer.writeText(XhtmlConstants.EVENT_PARAM, null);
      writer.writeText("=e;if(s)o.", null);
      writer.writeText(XhtmlConstants.SOURCE_PARAM, null);
      writer.writeText("=s;_submitPartialChange(f,v,o);}", null);
    }

    private static AutoSubmitScriptlet _sInstance =
      new AutoSubmitScriptlet();
  }

  static
  {
    XhtmlScriptletFactory.registerAllScriptlets();

    // Register our scriptlet
    AutoSubmitScriptlet.sharedInstance().registerSelf();
  }

  // Name for our Scriptlet
  private static final String _AUTO_SUBMIT_SCRIPTLET =
    "org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.AUTO_SUBMIT_SCRIPTLET";

  private static final String _FULL_PAGE_START = "submitForm(\'";
  private static final String _START = "_adfspu(\'";
  private static final String _START_RADIO = "_radioSet_adfspu(\'";
  private static final String _TRUE_END = ");return true;";
  private static final String _FALSE_END = ");return false;";

  // Partial page rendering scripts
  private static final String _FIRE_PARTIAL_CHANGE_START =
    "_firePartialChange(\'";
  private static final String _FIRE_PARTIAL_CHANGE_END = "\');return false;";
}
