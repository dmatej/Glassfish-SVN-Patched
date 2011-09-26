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
package org.apache.myfaces.trinidadinternal.renderkit.core;

import java.io.IOException;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.pages.FredJSP;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.DialogStyleScriptlet;

/**
 *
 */
class DialogRequest
{
  @SuppressWarnings("unchecked")
  public DialogRequest(
    UIViewRoot         targetRoot,
    String             clientId,
    String             formId,
    Map<String,Object> dialogProperties,
    boolean            usePopup
    )
  {
    _clientId = clientId;
    _formId   = formId;
    _usePopup = usePopup;

    if (dialogProperties == null)
      dialogProperties = Collections.emptyMap();

    Object width = dialogProperties.get("width");
    Object height = dialogProperties.get("height");

    FacesContext context = FacesContext.getCurrentInstance();

    if (usePopup)
    {
      _url = context.getExternalContext().encodeActionURL(context.getApplication().getViewHandler().getActionURL(context, targetRoot.getViewId()));
    }
    else
    {
      _url = context.getExternalContext().encodeActionURL(FredJSP.getRedirectURL(context,
                                  targetRoot,
                                  CoreRenderer.toString(width),
                                  CoreRenderer.toString(height)));
    }
    _dialogProperties  = dialogProperties;
  }

  static public void addDependencies(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    XhtmlUtils.addLib(context, arc, "_launchDialog()");
  }

  public void renderLaunchJavascript(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    ResponseWriter out = context.getResponseWriter();

    String formName = _formId;

    if (_usePopup)
    {
      // Use the common scriplet to output the javascript
      // that sets the skin entries for the lightweight dialogs.
      DialogStyleScriptlet.outputStyleMapForDialog(context, arc);
      
      // Finally output the call to launch the dialog
      out.writeText("TrPopupDialog._launchDialog(\"", null);
    }
    else
    {
      out.writeText("_launchDialog(\"", null);
    }

    out.writeText(_url, null);
    if (!_usePopup)
    {
      out.writeText("\", \"", null);
      out.writeText(_getDialogWindowName(), null);
    }
    out.writeText("\",{", null);

    boolean writtenOne = false;

    if (!_usePopup)
    {
      // Get some default widths and heights out there in
      // case they're omitted
      if (!_dialogProperties.containsKey("width"))
      {
        out.writeText("width:", null);
        out.writeText(_DEFAULT_WIDTH, null);
        writtenOne = true;
      }

      if (!_dialogProperties.containsKey("height"))
      {
        if (writtenOne)
          out.writeText(",", null);
        else
          writtenOne = true;

        out.writeText("height:", null);
        out.writeText(_DEFAULT_HEIGHT, null);
      }
    }

    Iterator<String> propertiesIter = _dialogProperties.keySet().iterator();
    while (propertiesIter.hasNext())
    {
      String key = propertiesIter.next();
      Object value = _dialogProperties.get(key);
      // =-=AEW When to put in quotes????
      if (value != null)
      {
        if (writtenOne)
          out.writeText(",", null);
        else
          writtenOne = true;

        out.writeText(key, null);
        out.writeText(":'", null);
        out.writeText(value, null);
        out.writeText("'", null);
      }
    }

    out.writeText("}," , null);

    if (_usePopup)
    {
      // Ouptut the callback function, and any properties that will 
      // pass to the callback when invoked
      out.writeText("TrPopupDialog._returnFromDialogAndSubmit, {'formName':'" , null);
      out.writeText(formName, null);
      out.writeText("'", null);
      
      if (_clientId != null)
      {
        out.writeText(",'postback':'", null);
        out.writeText(_clientId, null);
        out.writeText("'", null);
      }
      out.writeText("}", null);
    }
    else
    {
      out.writeText("\"" , null);
      out.writeText(formName, null);
      out.writeText("\",\"", null);
      if (_clientId != null)
        out.writeText(_clientId, null);
      out.writeText("\"", null);
      boolean isPPR = PartialPageUtils.supportsPartialRendering(arc);
      out.writeText(isPPR ? ",1" : ",0", null);
    }
    out.writeText(");", null);
  }

  //
  // Return a new name for every dialog we ever raise.
  // At a minimum, we just need to make sure that the user
  // never has two visible windows with the same name (Javascript
  // gets unhappy!).  We might use some complicated HttpSession
  // scheme, but this seems far, far simpler.
  //
  static private synchronized String _getDialogWindowName()
  {
    return "TrinidadDialog" + _sCount++;
  }

  private final String             _clientId;
  private final String             _formId;
  private final String             _url;
  private final boolean            _usePopup;
  private final Map<String,Object> _dialogProperties;


  // Some default widths and heights to avoid popping up huge
  // windows and snapping them back down (or, worse, not
  // snapping them back down).
  static private final String _DEFAULT_WIDTH = "100";
  static private final String _DEFAULT_HEIGHT = "100";
  static private int _sCount = 0;
}
