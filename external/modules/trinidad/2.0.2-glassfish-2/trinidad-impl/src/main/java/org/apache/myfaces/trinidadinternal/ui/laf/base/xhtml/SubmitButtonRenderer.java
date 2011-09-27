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

import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;
import org.apache.myfaces.trinidadinternal.ui.collection.Parameter;
import org.apache.myfaces.trinidadinternal.ui.partial.PartialPageRendererUtils;


/**
 * Renderer for form button nodes.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/SubmitButtonRenderer.java#0 $) $Date: 10-nov-2005.18:54:15 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SubmitButtonRenderer extends ResetButtonRenderer
{
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    // For Non-JavaScript browsers, render the name atttribute which is
    // encoded with parameter name and value pair.
    if (!supportsScripting(context))
    {
      context.getResponseWriter().writeAttribute("name", 
                  node.getAttributeValue(context, NAME_ATTR), null);
    } 
  }

  /**
   * Override to change the type of the button
   */
  @Override
  protected String getButtonType()
  {
    return "submit";
  }


  /**
   * Returns the Object storing the name value pairs
   */
  public static String[] getNameValues(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object nameValues = node.getAttributeValue(context, NAME_VALUES_ATTR);

    if (nameValues != null)
    {
      if (!(nameValues instanceof String[]))
        throw new IllegalArgumentException(
            "NAME_VALUES_ATTR only supports String arrays");
      return (String[]) nameValues;
    }
    else
    {
      ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                     node);
      Parameter[] params = null;
      String[] map = null;
      // If there's an action, and it can render on a link, then...
      if ((action != null) && (!action.renderAsEvent(context, node)))
      {
        // ...get the parameters from the client action, and use them
        params = action.getParameters(context, node);
      }

      if (params != null)
      {
        map = new String[params.length * 2];
        for (int i = 0, j = 0; i < params.length; i++)
        {
          Parameter p = params[i];
          map[j++] = p.getKey();
          map[j++] = p.getValue(context);
        }
        return map;
      }
      else
      {
        Object event = node.getAttributeValue(context, EVENT_ATTR);
        // if the event is set, then we need to trigger the event with the
        // given name, and a source with the id of the button:
        if (event != null)
        {
          int numParams = 2;

          Object id = node.getAttributeValue(context, ID_ATTR);
          if (id != null)
            numParams += 2;

          String[] targets = XhtmlLafUtils.getPartialTargets(context, node);
          if (targets != null)
          {
            numParams += 4;
          }

          map = new String[numParams];
          int pNum = 0;

          map[pNum++] = context.getURLEncoder().encodeParameter(EVENT_PARAM);
          map[pNum++] = event.toString();

          if (id!=null)
          {
            map[pNum++] = context.getURLEncoder().encodeParameter(SOURCE_PARAM);
            map[pNum++] = id.toString();
          }
          if (targets != null)
          {
            // Encode the partial targets into a String
            String enc = PartialPageRendererUtils.encodePartialTargets(targets);
            map[pNum++] = UIConstants.PARTIAL_TARGETS_PARAM;
            map[pNum++] = enc;
            map[pNum++] = PARTIAL_PARAM;
            map[pNum++] = "true";
          }
          return map;
        }
        else // deprecated behaviour
        {
          //
          // get any name/value pair to submit along with the form
          //
          Object name  = node.getAttributeValue(context, NAME_ATTR);

          if (name != null)
          {
            Object value = node.getAttributeValue(context, VALUE_ATTR);
            return new String[]
              {
                name.toString(),
                (value != null) ? value.toString() : null
              };
          }
        }
      }
    }
    return null;
  }



  /**
   * Creates the JavaScript form submission String for the parameters
   * of the SubmitButton
   */
  public static String createJSFunctionCall(
    UIXRenderingContext context,
    UINode           node,
    String           formName
    )
  {
    // encode any parameters that need to be passed to the server when
    // submitting, adding any needed hidden fields
    String initEventObject = XhtmlLafUtils.encodeJSEventObject(
                                                context,
                                                formName,
                                                getNameValues(context, node));

    boolean unValidated = Boolean.TRUE.equals(
                           node.getAttributeValue(context, UNVALIDATED_ATTR));

    int bufferLength = 29 + formName.length();

    // add on any js initialization, plus 1 for comma separator
    if (initEventObject != null)
      bufferLength += initEventObject.length() + 1;

    StringBuffer jsCode = new StringBuffer(bufferLength) ;

    // function/form name to submit
    jsCode.append("submitForm('");
    jsCode.append(formName);
    jsCode.append('\'');

    // determine whether we need to pass the no validation flag
    if (unValidated || (initEventObject != null))
    {
      // append the validation attribute
      jsCode.append(',');
      jsCode.append((unValidated) ? '0' : '1');

      // append any name/value pairs to submit
      if (initEventObject != null)
      {
        jsCode.append(',');
        jsCode.append(initEventObject);
      }
    }

    jsCode.append(");return false");

    return jsCode.toString();
  }

  /**
   * Returns the Javascript function call to return for this button.
   */
  @Override
  protected String getFunctionCall(
    UIXRenderingContext context,
    UINode           node,
    String           formName
    )
  {
    return createJSFunctionCall(context, node, formName);
  }
}
