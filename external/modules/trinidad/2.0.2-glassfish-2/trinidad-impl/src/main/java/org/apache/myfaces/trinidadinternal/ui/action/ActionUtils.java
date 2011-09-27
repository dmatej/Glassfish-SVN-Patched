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
package org.apache.myfaces.trinidadinternal.ui.action;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.collection.Parameter;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;

/**
 * Private utility methods shared by org.apache.myfaces.trinidadinternal.ui.action implementations.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/action/ActionUtils.java#0 $) $Date: 10-nov-2005.18:57:41 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
class ActionUtils
{
  /**
   * Deep clone of a Parameter array
   */
  public static Parameter[] cloneParameterArray(Parameter[] parameters)
  {
    if (parameters == null)
      return null;

    int length = parameters.length;
    if (length == 0)
      return new Parameter[0];

    Parameter[] copied = new Parameter[length];
    for (int i = 0; i < length; i++)
    {
      copied[i] = (Parameter) parameters[i].clone();
    }

    return copied;
  }

  /**
   * Deep clone of two Parameter arrays
   */
  public static Parameter[] joinParameterArrays
    (Parameter[] parametersA,
     Parameter[] parametersB)
  {
    if ((parametersA == null) || (parametersA.length == 0))
      return cloneParameterArray(parametersB);

    if ((parametersB == null) || (parametersB.length == 0))
      return cloneParameterArray(parametersA);

    int lengthA = parametersA.length;
    int lengthB = parametersB.length;
    int index = 0;
    int i;

    Parameter[] copied = new Parameter[lengthA + lengthB];
    for (i = 0; i < lengthA; i++)
    {
      copied[index++] = (Parameter) parametersA[i].clone();
    }

    for (i = 0; i < lengthB; i++)
    {
      copied[index++] = (Parameter) parametersB[i].clone();
    }

    return copied;
  }

  /**
   * Copies an array of partial targets.
   */
  public static String[] copyPartialTargets(String[] targets)
  {
    if (targets == null)
      return null;

    String[] copyTargets = new String[targets.length];
    System.arraycopy(targets, 0, copyTargets, 0, targets.length);
    return copyTargets;
  }

  /**
   * Appends client-defined parameters to the buffer
   */
  public static void appendClientParameters(
    UIXRenderingContext context,
    StringBuilder     buffer,
    Parameter[]      parameters
    )
  {
    appendClientParameters(context, buffer, parameters, null, null);
  }


  /**
   * Appends client-defined parameters to the submit buffer including an event
   */
  public static void appendClientParameters(
    UIXRenderingContext context,
    StringBuilder     buffer,
    Parameter[]      parameters,
    String           event,
    String           source
    )
  {
    appendClientParameters(context, buffer, parameters, event, source, null);
  }

  /**
   * Appends client-defined parameters to the submit buffer including an event
   */
  public static void appendClientParameters(
    UIXRenderingContext context,
    StringBuilder     buffer,
    Parameter[]      parameters,
    String           event,
    String           source,
    String           formName
    )
  {
    boolean gotParams = ((parameters != null) && (parameters.length > 0));
    boolean gotEvent = (event != null);
    boolean gotSource = (source != null);

    if (buffer.charAt(buffer.length() - 1) != ',')
      buffer.append(',');

    if (!gotEvent && !gotSource && !gotParams)
    {
      buffer.append('0');
      return;
    }

    buffer.append('{');

    FormEncoder formEncoder = context.getFormEncoder();
    boolean firstParameter = true;

    if (gotParams)
    {
      for (int i = 0; i < parameters.length; i++)
      {
        Parameter param = parameters[i];
        String paramKey = param.getKey();

        // If the ClientAction sets source or event, don't allow a parameter
        // to override it. If it's sent in, it's an attribute and the client
        // should set the value on the clientAction, not in a parameter.
        if (gotEvent && paramKey.equalsIgnoreCase("event"))
          continue;

        if (gotSource && paramKey.equalsIgnoreCase("source"))
          continue;

        // append a comma to separate the fields
        if (!firstParameter)
          buffer.append(',');
        else
          firstParameter = false;

        // Really, we shouldn't need the quotes here at all, unless
        // "paramKey" is invalid.
        buffer.append("'");
        buffer.append(paramKey);
        buffer.append("':'");

        String value = param.getValue(context);
        // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
        String encodedValue =
          XhtmlLafUtils.getFormEncodedParameter(formEncoder, formName,
                                                paramKey, value);
        if (encodedValue != null)
        {
          XhtmlLafUtils.escapeJS(buffer, encodedValue, true);
        }

        buffer.append("'");
      }
    }
    if (gotEvent)
    {
      if (!firstParameter)
        buffer.append(',');
      else
        firstParameter = false;

      buffer.append("event:'");
      // BUG 3641880 - APPS: CALL FORMENCODER FOR EVENT AND SOURCE PARAMS
      String encodedEvent =
        XhtmlLafUtils.getFormEncodedParameter(formEncoder, formName, "event",
                                              event);
      if (encodedEvent != null)
      {
        XhtmlLafUtils.escapeJS(buffer, encodedEvent, true);
      }      buffer.append("'");
    }
    if (gotSource)
    {
      if (!firstParameter)
        buffer.append(',');
      else
        firstParameter = false;

      buffer.append("source:'");
      // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
      String encodedSource =
        XhtmlLafUtils.getFormEncodedParameter(formEncoder, formName, "source",
                                              source);
      if (encodedSource != null)
      {
        XhtmlLafUtils.escapeJS(buffer, encodedSource, true);
      }
      buffer.append("'");
    }
    buffer.append("}");
  }

  static String appendURLParameters(
    UIXRenderingContext context,
    String       base,
    Parameter[]  params
    )
  {
    int bufLen = 1 + base.length() + getClientParametersSize(context, params);
    StringBuilder buffer = new StringBuilder(bufLen);
    buffer.append(base);
    if (base.indexOf('?') < 0)
      buffer.append('?');
    appendURLParameters(buffer, params);
    return buffer.toString();
  }

  static void appendURLParameters(
    StringBuilder buffer,
    Parameter[]  params
    )
  {
    for (int i = 0; i < params.length; i++)
    {
      appendURLParameter(buffer, params[i].getKey(), params[i].getValue());
    }
  }

  static void appendURLParameter(
    StringBuilder buffer,
    Parameter    param
    )
  {
    appendURLParameter(buffer, param.getKey(), param.getValue());
  }

  // Append a parameter to a URL buffer
  static void appendURLParameter(
    StringBuilder buffer,
    String       name,
    String       value
    )
  {
    if (value == null)
      return;

    char lastChar = buffer.charAt(buffer.length() - 1);
    if ((lastChar != '?') && (lastChar != '&'))
    {
      // Assumes we already are in the query string
      buffer.append('&');
    }

    buffer.append(name);
    buffer.append('=');
    buffer.append(value);
  }


  /**
   * Computes the buffer size for client-defined parameters
   */
  public static final int getClientParametersSize(
    UIXRenderingContext context,
    Parameter[]      parameters
    )
  {
    if ((parameters == null) || (parameters.length == 0))
      return 1;

    // Client parameters look like this:
    //    {key1:value1,key2:value2}
    // Leave room for the open/close braces
    int length = 2;

    for (int i = 0; i < parameters.length; i++)
    {
      Parameter param = parameters[i];
      length += param.getKey().length();

      String value = param.getValue(context);
      if (value != null)
        length += value.length();

      // Make sure there is room so that we can quote the key/value
      length += 4;

      // Leave room for the comma, if this isn't the last parameter
      if (i < (parameters.length - 1))
        length++;
    }

    return length;
  }

  /**
   * Returns the current form name
   */
  public static String getFormName(UIXRenderingContext context)
  {
    Object formName = context.getProperty(UIConstants.MARLIN_NAMESPACE,
                                          UIConstants.FORM_NAME_PROPERTY);

    if (formName == null)
    {
      if (_LOG.isWarning())
        _LOG.warning(_FORM_WARNING);

      assert false:_FORM_WARNING;
      return null;
    }

    return formName.toString();
  }

  // get the source parameter, and if null, then get the id as the source
  public static String getSource(
     UIXRenderingContext context,
     UINode node,
     BoundValue binding,
     String actionSource)
  {
    String source = null;

    // First look to the binding for the source
    if (binding != null)
      source = (String) binding.getValue(context);

    // If nothing bound, check for something hardcoded on the action
    if (source == null)
      source = actionSource;

    // OK, the client has not set a source,
    // look for something on the parent node.
    if (source == null)
    {
      // Check for an ID on the parent node
      Object id = node.getAttributeValue(context, UIConstants.ID_ATTR);
      if (id == null)
        // ...or a name
        id = node.getAttributeValue(context, UIConstants.NAME_ATTR);

      // If one of them was set, return it, otherwise not much we can do...
      source = (id == null) ? null : id.toString();
    }
    return source;
  }

  static Parameter buildParameter(
     UIXRenderingContext context,
     UINode node,
     BoundValue binding,
     String param,
     String paramName)
  {
    Parameter parameter = new Parameter();
    parameter.setKey(paramName);
    parameter.setValue(param);
    parameter.setValueBinding(binding);
    return parameter;
  }

  // Gets a value from a bound value
  public static Object getValue(
    UIXRenderingContext context,
    BoundValue       boundValue,
    Object           defaultValue
    )
  {
    if (boundValue != null)
    {
      // Note that we _do not_ do defaulting behavior (that is,
      // fall back on "defaultValue").  That's because UIBeanDef does
      // defaulting automatically.
      return boundValue.getValue(context);
    }

    return defaultValue;
  }

  private static final String _FORM_WARNING =
    "Action can only be used within a form, or by specifying the formName attribute";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ActionUtils.class);
}
