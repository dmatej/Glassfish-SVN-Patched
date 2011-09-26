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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXProcess;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.Scriptlet;
import org.apache.myfaces.trinidad.util.IntegerUtils;


/**
 * Utility class for the Process components and SelectRange components since
 * they share the submit javascript code.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/ProcessUtils.java#0 $) $Date: 10-nov-2005.19:01:42 $
 */
public class ProcessUtils
{
  /**
   * Returns a String with the javascript function call _navSubmit
   */
  public static String getSubmitScriptCall(
    String  form,
    String  id,
    long    value,
    boolean doValidate
    )
  {
    String valueString = IntegerUtils.getString(value);

    String gotoEvent = XhtmlConstants.GOTO_EVENT;
    int bufferSize = _LINK_ON_CHANGE_FORM_START.length() +
                     form.length() +
                     34 +
                     gotoEvent.length() +
                     id.length() +
                     valueString.length();

    StringBuffer buffer = new StringBuffer(bufferSize);
    buffer.append(_LINK_ON_CHANGE_FORM_START);
    buffer.append(form);
    buffer.append("', '");
    buffer.append(gotoEvent);
    buffer.append("', '");
    buffer.append(id);
    if (doValidate)
      buffer.append("',1,'");
    else
      buffer.append("',0,'");
    buffer.append(valueString);
    buffer.append("');return false");

    return buffer.toString();
  }

  /**
 * @param component
 * @param stamp
 * @param startIndex
 * @return
 */
  public static int getBackIndex(
    UIXProcess component,
    UIComponent stamp,
    int startIndex)
  {
    int i = startIndex - 1;
    while (i >= 0)
    {
      component.setRowIndex(i);
      boolean disabled = Boolean.TRUE.equals(
        stamp.getAttributes().get("disabled"));
      boolean readOnly = Boolean.TRUE.equals(
        stamp.getAttributes().get("readOnly"));
      boolean rendered = stamp.isRendered();

      // if this node is rendered and not disabled and not readOnly
      // then it can be used as the back button node.
      if (!disabled && !readOnly && rendered)
      {
        component.setRowIndex(startIndex);
        return i;
      }

      i--;
    }

    component.setRowIndex(startIndex);
    return NO_INDEX;

  }

  /**
   */
  public static String getChoiceOnChangeFormSubmitted(
    String           form,
    String           id,
    boolean          validate
    )
  {
    String gotoEvent = XhtmlConstants.GOTO_EVENT;
    int initialSize = _CHOICE_ON_CHANGE_FORM_START.length() +
                          form.length()                         +
                          13                                    +
                          id.length()                         +
                          _CHOICE_ON_CHANGE_FORM_END.length();

    StringBuffer buffer = new StringBuffer(initialSize);
    buffer.append(_CHOICE_ON_CHANGE_FORM_START);
    buffer.append(form);
    buffer.append("','");
    buffer.append(gotoEvent);
    buffer.append("','");
    buffer.append(id);
    if (validate)
      buffer.append("',1");
    else
      buffer.append("',0");

    buffer.append(_CHOICE_ON_CHANGE_FORM_END);
    return buffer.toString();
  }


  /**
   * @param component
   * @param stamp
   * @param startIndex
   * @return
   */
  public static int getNextIndex(
    UIXProcess component, 
    UIComponent stamp,
    int startIndex)
  {
    int i = startIndex + 1;
    int rowCount = component.getRowCount();
    while (i < rowCount)
    {
      component.setRowIndex(i);
      boolean disabled = Boolean.TRUE.equals(
        stamp.getAttributes().get("disabled"));
      boolean readOnly = Boolean.TRUE.equals(
        stamp.getAttributes().get("readOnly"));
      boolean rendered = stamp.isRendered();

      // if this node is rendered and not disabled and not readOnly
      // then it can be used as the back button node.
      if (!disabled && !readOnly && rendered)
      {
        component.setRowIndex(startIndex);
        return i;
      }

      i++;
    }

    component.setRowIndex(startIndex);
    return NO_INDEX;
  }

  public static void renderNavSubmitScript(
    FacesContext        context,
    RenderingContext arc
    ) throws IOException
  {
    XhtmlUtils.addLib(context, arc, _NAV_SUBMIT_SCRIPTLET);
  }

  public static void renderNavChoiceSubmitScript(
    FacesContext        context,
    RenderingContext arc
    ) throws IOException
  {
    XhtmlUtils.addLib(context, arc,_NAV_CHOICE_SUBMIT_SCRIPTLET);
  }



  // Scriptlet that renders the _navSubmit script
  private static class NavSubmitScriptlet extends Scriptlet
  {
    static public Scriptlet sharedInstance()
    {
      return _sInstance;
    }

    @Override
    public Object getScriptletKey()
    {
      return _NAV_SUBMIT_SCRIPTLET;
    }

    @Override
    protected void outputScriptletContent(
      FacesContext context,
      RenderingContext arc) throws IOException
    {
      context.getResponseWriter().writeText(_NAV_SUBMIT_SCRIPT, null);
    }

    private static NavSubmitScriptlet _sInstance =
      new NavSubmitScriptlet();
  }

  // Scriptlet that renders the _navChoiceSubmit script
  private static class NavChoiceSubmitScriptlet extends Scriptlet
  {
    static public Scriptlet sharedInstance()
    {
      return _sInstance;
    }

    @Override
    public Object getScriptletKey()
    {
      return _NAV_CHOICE_SUBMIT_SCRIPTLET;
    }

    @Override
    protected void outputScriptletContent(
      FacesContext context,
      RenderingContext arc) throws IOException
    {
      ResponseWriter writer = context.getResponseWriter();

      // write generic navbar submission code
      writer.writeText(_CHOICE_SUBMIT_SCRIPT, null);
    }

    private static NavChoiceSubmitScriptlet _sInstance =
      new NavChoiceSubmitScriptlet();
  }

  // Name for our scriptlets
  private static final String _NAV_SUBMIT_SCRIPTLET = "NavSubmit";
  private static final String _NAV_CHOICE_SUBMIT_SCRIPTLET = "NavChoiceSubmit";

  static
  {
    // Register our scriptlets
    NavSubmitScriptlet.sharedInstance().registerSelf();
    NavChoiceSubmitScriptlet.sharedInstance().registerSelf();
  }

   static private final String _NAV_SUBMIT_SCRIPT =
     "function _navSubmit(formName, event, id, vld, val)"
    +"{"
    +  "return _submitPartialChange("
    +             "formName,"
    +             "vld,"
    +             "{"
    +               "event:event,"
    +               "source:id,"
    +               "value:val});}";

  static private final String _LINK_ON_CHANGE_FORM_START =
  "_navSubmit('";

  static private final String _CHOICE_ON_CHANGE_FORM_START =
  "_navChoiceSubmit(this, '";

  static private final String _CHOICE_ON_CHANGE_FORM_END =
  ")";
  // submits the form for the choice and rolls back if validation fails
  private static final String _CHOICE_SUBMIT_SCRIPT =
    "function _navChoiceSubmit(choice, formName, event, id, vld)"
   +"{"
   +  "if (!_navSubmit(formName, event, id, vld, choice.options[choice.selectedIndex].value))"
   +  "{"
   +    "choice.selectedIndex = choice._lastValue;"
   +  "}"
   +"}";


  static public final int NO_INDEX = -1;
}
