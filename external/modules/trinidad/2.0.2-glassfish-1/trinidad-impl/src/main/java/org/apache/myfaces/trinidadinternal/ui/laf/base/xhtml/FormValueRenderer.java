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


import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.RoledRenderer;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FormValueRenderer.java#0 $) $Date: 10-nov-2005.18:53:52 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class FormValueRenderer extends XhtmlLafRenderer
                               implements RoledRenderer
{
  public NodeRole getNodeRole(
    UIXRenderingContext context,
    UINode           node)
  {
    return _FORM_VALUE_ROLE;
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);
    context.getResponseWriter().writeAttribute(TYPE_ATTRIBUTE, "hidden", null);

    Object nodeName = getTransformedName(context, node);

    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    context.getFormEncoder().registerFormParameter(nodeName);
    Object value = getAttributeValue(context, node, VALUE_ATTR, null);
    Object encodedValue =
      XhtmlLafUtils.getFormEncodedValue(context, nodeName, value);
    renderAttribute(context, VALUE_ATTRIBUTE, encodedValue);

    if (nodeName != null)
    {
      // render the name attribute
      renderAttribute(context, "name", nodeName);

      String formName = getParentFormName(context);

      // note that we have rendered this value, in case we "need" it later
      _addRenderedValue( formName, nodeName);
    }
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return INPUT_ELEMENT;
  }


  /**
   * Remembers that a specific hidden value name will be needed.
   * @param formName the name of the form that must contain the hidden value.
   * @param name the name of the hidden value.
   */
  public static void addNeededValue(
    UIXRenderingContext context,
    String           formName,
    String           name
    )
  {
    addNeededValue(context, formName, name, null, null, null);
  }

  /**
   * Remembers that a specific hidden value name will be needed. This method
   * allows upto four values. The moment a null name is detected the rest of
   * the names are ignored.
   * @param formName the name of the form that must contain the hidden value.
   * @param name1 the name of a hidden value.
   * @param name2 the name of a hidden value. maybe null.
   * @param name3 the name of a hidden value. maybe null.
   * @param name4 the name of a hidden value. maybe null.
   */
  public static void addNeededValue(
    UIXRenderingContext context,
    String           formName,
    String           name1,
    String           name2,
    String           name3,
    String           name4
    )
  {
    FormData fd = RenderingContext.getCurrentInstance().getFormData();
    fd.addNeededValue(name1);
    if (name2!=null)
    {
      fd.addNeededValue(name2);
      if (name3!=null)
      {
        fd.addNeededValue(name3);
        if (name4!=null)
        {
          fd.addNeededValue(name4);
        }
      }
    }
  }

  //
  // Remembers that a specific hidden value name has already
  // been rendered, in case it is "needed" later
  //
  static private void _addRenderedValue(
    Object           formName,
    Object           name
    )
  {
    if (name != null)
    {
      if (formName == null)
      {
        if (_LOG.isWarning())
          _LOG.warning("CANNOT_LOCATE_PARENT_FORM", name);
        return;
      }

      FormData fd = RenderingContext.getCurrentInstance().getFormData();
      fd.addRenderedValue(name.toString());
    }
  }

  static void __renderHiddenValue(
    UIXRenderingContext context,
    String           formName,
    Object           name,
    Object           value
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    FormEncoder formEncoder = context.getFormEncoder();

    _renderHiddenField(writer, formEncoder, name, value);

    // note that we have rendered this value, in case we "need" it later
    _addRenderedValue(formName, name);
  }

  private static void _renderHiddenField(
    ResponseWriter writer,
    FormEncoder  formEncoder,
    Object       name,
    Object       value
    )
    throws IOException
  {
    Object encodedValue = formEncoder.encodeFormValue(name, value);

    writer.startElement(INPUT_ELEMENT, null);
    writer.writeAttribute(TYPE_ATTRIBUTE, "hidden", null);
    writer.writeAttribute(NAME_ATTRIBUTE, name, null);
    writer.writeAttribute(VALUE_ATTRIBUTE, encodedValue, null);
    writer.endElement(INPUT_ELEMENT);
  }

  static final NodeRole _FORM_VALUE_ROLE =
    new NodeRole("formValue", new NodeRole[] { USER_INVISIBLE_ROLE,
                                               FORM_SUBMIT_DATA_ROLE });
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FormValueRenderer.class);
}
