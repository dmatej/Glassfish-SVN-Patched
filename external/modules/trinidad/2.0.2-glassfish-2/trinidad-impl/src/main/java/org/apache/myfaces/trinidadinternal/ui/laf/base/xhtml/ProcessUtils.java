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

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.context.RenderingContext;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.Scriptlet;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidad.util.IntegerUtils;

/**
 * Utility class for the Process components and SelectRange components since
 * they share the submit javascript code.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ProcessUtils.java#0 $) $Date: 10-nov-2005.18:54:09 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ProcessUtils
{

  /**
   * Creates the submit button bean
   */
  public static UINode createSubmitButton(
    UIXRenderingContext context,
    Object           buttonText,
    Object           buttonAccessKey,
    String           buttonID,
    String           formName,
    boolean          validate,
    String           eventKey,
    String           sourceKey,
    String           source,
    String           valueKey,
    long             value,
    String           sizeKey,
    int              size
    )
  {
    MarlinBean submitButton = new MarlinBean(UIConstants.SUBMIT_BUTTON_NAME);
    submitButton.setID(buttonID);
    submitButton.setAttributeValue(UIConstants.FORM_NAME_ATTR, formName);
    submitButton.setAttributeValue(UIConstants.UNVALIDATED_ATTR, Boolean.valueOf(!validate));
    submitButton.setAttributeValue(UIConstants.TEXT_ATTR, buttonText);
    submitButton.setAttributeValue(UIConstants.ACCESS_KEY_ATTR,
                                   buttonAccessKey);
    return submitButton;
  }

  /**
   * Returns a String with the javascript function call _navSubmit
   * @param context
   * @param form
   * @param eventKey
   * @param sourceKey
   * @param name
   * @param valueKey
   * @param value
   * @param sizeKey
   * @param size
   * @param doValidate 
   * @param partialTargetsKey
   * @param partialTargets
   * @return
   */
  public static String getSubmitScriptCall(
    UIXRenderingContext context,
    String  form,
    String  eventKey,
    String  sourceKey,
    String  name,
    String  valueKey,
    long    value,
    String  sizeKey,
    int     size,
    boolean doValidate,
    String  partialTargetsKey,
    String  partialTargets
    )
  {
    String valueString = IntegerUtils.getString(value);
    String sizeString  = IntegerUtils.getString(size);

    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    FormEncoder formEncoder = context.getFormEncoder();
    String encodedGotoEvent =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            eventKey, UIConstants.GOTO_EVENT);
    String encodedSource =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            sourceKey, name);
    String encodedValue =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            valueKey, valueString);
    String encodedSize =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            sizeKey, sizeString);
    String encodedPartialTargets = null;
    if (partialTargetsKey != null && partialTargets != null)
    {
      encodedPartialTargets =
        XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                              partialTargetsKey,
                                              partialTargets);
    }

    if (form == null) form = "";
    if (encodedSource == null) encodedSource = "";
    

    int bufferSize = _LINK_ON_CHANGE_FORM_START.length() +
                     form.length() +
                     34 +
                     encodedGotoEvent.length() +
                     encodedSource.length() +
                     encodedValue.length() +
                     encodedSize.length();

    if (partialTargets != null)
      bufferSize += (partialTargets.length() + 2);

    StringBuffer buffer = new StringBuffer(bufferSize);
    buffer.append(_LINK_ON_CHANGE_FORM_START);
    buffer.append(form);
    buffer.append("', '");
    buffer.append(encodedGotoEvent);
    buffer.append("', '");
    buffer.append(encodedSource);
    if (doValidate)
      buffer.append("',1,'");
    else
      buffer.append("',0,'");
    buffer.append(encodedValue);
    buffer.append("', '");
    buffer.append(encodedSize);
    buffer.append("',");

    // Tack on the partial targets array
    if (encodedPartialTargets != null)
    {
      buffer.append("'");
      buffer.append(encodedPartialTargets);
      buffer.append("'");
    }
    else
    {
      buffer.append("null");
    }

    buffer.append(");return false");

    return buffer.toString();
  }

  public static String getChoiceOnChangeFormSubmitted(
    UIXRenderingContext context,
    UINode           node,
    String           form,
    String           eventKey,
    String           sourceKey,
    String           name,
    String           partialTargetsKey,
    String           partialTargets
    )
  {
    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    FormEncoder formEncoder = context.getFormEncoder();
    String encodedGotoEvent =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder,
                                            form,
                                            eventKey,
                                            UIConstants.GOTO_EVENT);
    String encodedSource =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form, sourceKey, name);

    String encodedPartialTargets =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            partialTargetsKey,
                                            partialTargets);

    int initialSize = _CHOICE_ON_CHANGE_FORM_START.length() +
                          form.length()                         +
                          13                                    +
                          encodedSource.length()                         +
                          _CHOICE_ON_CHANGE_FORM_END.length();

    // Make room for partialTargets if we've got any
    if (encodedPartialTargets != null)
      initialSize += (encodedPartialTargets.length() + 2);

    StringBuffer buffer = new StringBuffer(initialSize);
    buffer.append(_CHOICE_ON_CHANGE_FORM_START);
    buffer.append(form);
    buffer.append("','");
    buffer.append(encodedGotoEvent);
    buffer.append("','");
    buffer.append(encodedSource);
    /** for adf faces, set this to 0
    if (_doValidate(context, node))
      buffer.append("',1");
    else
      buffer.append("',0");
    **/
    buffer.append("',0");
    if (encodedPartialTargets != null)
    {
      buffer.append(",null,");
      buffer.append("'");
      buffer.append(encodedPartialTargets);
      buffer.append("'");
    }

    buffer.append(_CHOICE_ON_CHANGE_FORM_END);
    return buffer.toString();
  }

  public static int getBackIndex(
    UIXCollection component,
    UIComponent    stamp,
    int            startIndex
  )
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


  public static int getNextIndex(
    UIXCollection component,
    UIComponent    stamp,
    int            startIndex
  )
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
    UIXRenderingContext context
    ) throws IOException
  {
    XhtmlLafUtils.addLib(context, _NAV_SUBMIT_SCRIPTLET);
  }

  public static void renderNavChoiceSubmitScript(
    UIXRenderingContext context
    ) throws IOException
  {
    XhtmlLafUtils.addLib(context, _NAV_CHOICE_SUBMIT_SCRIPTLET);
  }

  /**
   * @todo An identical method is used in to SelectRangeChoiceBarRenderer
   * combine value,size into one String.
   */
  public static String concatenateParams(
    long    value,
    int     size
    )
  {
    String valueString = IntegerUtils.getString(value);
    String sizeString  = IntegerUtils.getString(size);

    int bufferSize = valueString.length() +
                     2                    +
                     sizeString.length();

    StringBuffer buffer = new StringBuffer(bufferSize);

    buffer.append(valueString);

    buffer.append(",");

    buffer.append(sizeString);

    return buffer.toString();
  }

  private static String[] _createKeyValueArray(
    String eventKey,
    String sourceKey,
    String source,
    String valueKey,
    long   value,
    String sizeKey,
    int    size,
    String partialTargetKey,
    String partialTargets
    )
  {
    int length = 8;

    // Make room for partial targets if we've got some
    if (partialTargets != null)
      length += 2;

    String[] keyValues = new String[length];
    keyValues[0] = eventKey;
    keyValues[1] = UIConstants.GOTO_EVENT;
    keyValues[2] = sourceKey;
    keyValues[3] = source;
    keyValues[4] = valueKey;
    keyValues[5] = IntegerUtils.getString(value);
    keyValues[6] = sizeKey;
    keyValues[7] = IntegerUtils.getString(size);

    if (partialTargets != null)
    {
      keyValues[8] = partialTargetKey;
      keyValues[9] = partialTargets;
    }

    return keyValues;
  }


  private static String _getNavSubmitScript(
    String eventKey,
    String sourceKey,
    String valueKey,
    String sizeKey,
    String partialTargetsKey
    )
  {
    int len = _NAV_SUBMIT_SCRIPT_LENGTH +
      eventKey.length() +
      sourceKey.length() +
      valueKey.length() +
      sizeKey.length() +
      partialTargetsKey.length();

    StringBuffer buf = new StringBuffer(len);

    buf.append(_NAV_SUBMIT_SCRIPT[0]);
    buf.append(eventKey);
    buf.append(_NAV_SUBMIT_SCRIPT[1]);
    buf.append(sourceKey);
    buf.append(_NAV_SUBMIT_SCRIPT[2]);
    buf.append(valueKey);
    buf.append(_NAV_SUBMIT_SCRIPT[3]);
    buf.append(sizeKey);
    buf.append(_NAV_SUBMIT_SCRIPT[4]);
    buf.append(partialTargetsKey);
    buf.append(_NAV_SUBMIT_SCRIPT[5]);

    return buf.toString();
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
      // We output a function "_navSubmit" (UIX submit a goto event)
      // which takes the following arguments:
      // formName, event, node, vld, val, sze, partialTargets

      ResponseWriter writer = context.getResponseWriter();

      // write generic navbar submission code
      writer.writeText(_getNavSubmitScript(UIConstants.EVENT_PARAM,
                                           UIConstants.SOURCE_PARAM,
                                           UIConstants.VALUE_PARAM,
                                           UIConstants.SIZE_PARAM,
                                           UIConstants.PARTIAL_TARGETS_PARAM),
                       null);
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
      writer.write(_CHOICE_SUBMIT_SCRIPT);
    }

    private static NavChoiceSubmitScriptlet _sInstance =
      new NavChoiceSubmitScriptlet();
  }

  // Name for our scriptlets
  private static final String _NAV_SUBMIT_SCRIPTLET = "ui.NavSubmit";
  private static final String _NAV_CHOICE_SUBMIT_SCRIPTLET = "ui.NavChoiceSubmit";

  static
  {
    // Register our scriptlets
    NavSubmitScriptlet.sharedInstance().registerSelf();
    NavChoiceSubmitScriptlet.sharedInstance().registerSelf();
  }

   static private final String[] _NAV_SUBMIT_SCRIPT =
  {
     "function _navSubmit(formName, event, node, vld, val, sze, partialTargets)"
    +"{"
    +  "var i = val.indexOf(',');"

    +  "if (i >= 0)"
    +  "{"
    +    "sze = val.substring(i+1);"
    +    "val = val.substring(0, i);"
    +  "}"

    +  "var submitFunc = (partialTargets == (void 0)) ? submitForm : "
    +                               "_submitPartialChange;"
    +  "return submitFunc("
    +             "formName,"
    +             "vld,"
    +             "{",                        // followed by the event name
                    ":event,",                // followed by the source
                    ":node,",               // followed by the value
                    ":val,",                  // followed by the prm
                    ":sze,",                  // followed by partialTargets
                    ":partialTargets});}"};

  private static final int _NAV_SUBMIT_SCRIPT_LENGTH =
    XhtmlLafUtils.getLength(_NAV_SUBMIT_SCRIPT);

  static private final String _LINK_ON_CHANGE_FORM_START =
  "_navSubmit('";

  static private final String _CHOICE_ON_CHANGE_FORM_START =
  "_navChoiceSubmit(this, '";

  static private final String _CHOICE_ON_CHANGE_FORM_END =
  ")";
  // submits the form for the choice and rolls back if validation fails
  private static final String _CHOICE_SUBMIT_SCRIPT =
    "function _navChoiceSubmit(choice, formName, event, node, vld, sze, partialTargets)"
   +"{"
   +  "if (!_navSubmit(formName, event, node, vld, choice.options[choice.selectedIndex].value, sze, partialTargets))"
   +  "{"
   +    "choice.selectedIndex = choice._lastValue;"
   +  "}"
   +"}";


  static public final int NO_INDEX = -1;
}
