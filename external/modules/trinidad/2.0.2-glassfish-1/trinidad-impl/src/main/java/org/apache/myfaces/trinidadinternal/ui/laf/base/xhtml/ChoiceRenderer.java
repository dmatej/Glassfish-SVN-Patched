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

import java.util.List;

import javax.faces.component.UIComponent;

import javax.faces.component.ValueHolder;
import javax.faces.context.ResponseWriter;

import javax.faces.model.SelectItem;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectOneChoice;
import org.apache.myfaces.trinidadinternal.renderkit.uix.SelectItemSupport;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;

import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.HtmlLafRenderer;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ChoiceRenderer.java#0 $) $Date: 10-nov-2005.18:53:46 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ChoiceRenderer extends FormSelectRenderer
{
  @Override
  protected Boolean isMultipleSelection(
    UIXRenderingContext context,
    UINode           node)
  {
    // choice is single selection
    return Boolean.FALSE;
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // If we've got a ClientAction, let it write its dependencies
    // before we start rendering the link
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    if (action != null)
      action.writeDependencies(context, node);

    super.prerender( context, node);

    // Is this choice in a branch of the tree that is repeated
    boolean isRepeatingChoice = _isRepeatingChoice(context, node);

    // if repeating choice and not already rendered,
    // render the javascript function
    // to keep choices with the same name synchronized
    if (isRepeatingChoice)
    {
      if (!isPreviouslyRendered(context, _SYNC_FUNCTION_PROPERTY))
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.startElement( SCRIPT_ELEMENT, null);
        renderScriptDeferAttribute(context);
        // Bug #3426092:
        // render the type="text/javascript" attribute in accessibility mode
        XhtmlLafRenderer.renderScriptTypeAttribute(context);

        writer.writeText(_FUNC, null);
        writer.endElement( SCRIPT_ELEMENT);
      }
    }
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    super.renderContent(context, node);
    selectItemsRenderContent(context, node);
  }



  protected void selectItemsRenderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    UIComponent component = OptionContainerRenderer.__getUIComponent(context,
                                                                     node);
    if (component != null)
    {
      List<SelectItem> selectItems =
        SelectItemSupport.getSelectItems(component,
                                         SelectItemSupport.getConverter(
                                         component));
      // if NOTHING IS SELECTED
      // we must render a blank item first, if the app developer
      // didn't already add one.
      // We use the noSelectionLabel's attribute value if it is available
      // (It might say something like "None", for example.
      // Otherwise, we just leave it blank.

      // now check if nothing is selected by default.

      boolean isNothingSelected = _isNothingSelected(component,
                                                     selectItems);

      if (selectItems != null && !selectItems.isEmpty() && isNothingSelected)
      {
        // now check if the first item is already a blank item.
        // if it isn't, render a blank item, using the
        // noSelectionLabel attribute value as the itemLabel.
        SelectItem firstItem = selectItems.get(0);
        Object firstItemValue = firstItem.getValue();
        if (!("".equals(firstItemValue)))
        {
          Object label = component.getAttributes().get(
                           CoreSelectOneChoice.UNSELECTED_LABEL_KEY);
          String noSelectionLabel = (label != null) ? label.toString() : "";

          SelectItem item = new SelectItem("", noSelectionLabel, "", false);

          renderSelectItem(context, node, component, item,
                           "", false, false, 0);
        }

      }



      renderSelectItemOptions(context,
                              node,
                              component,
                              selectItems);
    }
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.postrender(context, node);

    // If we have a ClientAction which requires an explicit
    // trigger, render the trigger now...
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    if ((action != null) && action.isTriggerRequired(context, node))
    {
      renderTrigger(context, node, action);
    }
  }

  @Override
  protected Object getOnClick(
    UIXRenderingContext context,
    UINode           node)
  {
    if (supportsScripting(context))
    {
      return _getActionHandler(context, node, ON_CLICK_ATTR);
    }
    else
    {
      return super.getOnClick(context, node);
    }
  }

  @Override
  protected Object getOnBlur(
    UIXRenderingContext context,
    UINode           node)
  {
    if (supportsScripting(context))
    {
      return _getActionHandler(context, node, ON_BLUR_ATTR);
    }
    else
    {
      return super.getOnClick(context, node);
    }
  }

  /**
   * Returns the onChange handler.  This is in place to support
   * RadioSetRenderer, which does not have an onChange handler.
   */
  @Override
  protected Object getOnChange(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    if (!supportsScripting(context))
    {
     return super.getOnChange(context, node);
    }

    // Check for a client-provided onChange handler
    Object onChangeObject = node.getAttributeValue(context, ON_CHANGE_ATTR);
    String onChange = null;

    if (onChangeObject != null )
      onChange = onChangeObject.toString();

    String actionScript = null;

    // IE hands off select element events to the beat of a different drummer.
    if ((context.getAgent().getAgentOS() == TrinidadAgent.OS_WINDOWS)
        && (HtmlLafRenderer.isIE(context)))
    {
      // The CHOICE_CHANGE_TRACKER just sets a flag whenever a choice occurs.
      // Only want to do this if there is a ClientAction.
      if (needsChangeTracker(context, node))
        actionScript = CHOICE_CHANGE_TRACKER;
    }
    else
    {
      actionScript = getActionScript(context, node);
    }

    // Is this choice in a branch of the tree that is repeated
    boolean isRepeatingChoice = _isRepeatingChoice(context, node);

    // If nobody's interested, we're done.
    if ((actionScript == null) && (!isRepeatingChoice))
      return onChange;

    // We have at most three scripts that need to be chained:
    // 1. The repeating choice synchronization script
    // 2. The ClientAction script
    // 3. The client's event handler
    // It would be nice if we could just use getChainedJS() to
    // produce nested calls to _chain(), but I don't think
    // getChainedJS() will handle escaping quotes correctly
    // if called multiple times.  Instead, if we have all three
    // scripts, we just manually chain the #1 and #2, and use
    // getChainedJS to chain this with #3.

    String preScript = null;

    if ((actionScript != null) && (isRepeatingChoice))
    {
      // If we have both a ClientAction script and a script
      // for synchronizing across repeating choices, we chain
      // these together manually.
      StringBuffer buffer = new StringBuffer(_CALL_FUNC.length()
                                             + actionScript.length()
                                             + 1);
      buffer.append(_CALL_FUNC);
      buffer.append(';');
      buffer.append(actionScript);

      preScript = buffer.toString();
    }
    else if (isRepeatingChoice)
    {
      preScript = _CALL_FUNC;
    }
    else
    {
      preScript = actionScript;
    }

    // Do we ever want to short circuit? Yes, see bug #3761794
    return XhtmlLafUtils.getChainedJS(onChange, preScript, true);
  }

  /**
   * gets the script to add to the choice.
   * This method may be overwritten, and the return value may be non-null
   * even if we do not have a primaryClientAction, as is the case in lovChoice.
   * So do not use the return value as a check for the existence of a
   * primaryClientAction.
   * @param context
   * @param node
   * @return String, the script
   */
  protected String getActionScript(
    UIXRenderingContext context,
    UINode           node)
  {

    // Check for a ClientAction
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    // If we've got a ClientAction and it does not require an
    // explicit trigger, get the script for the action.
    if ((action != null) && !action.isTriggerRequired(context, node))
    {
      return action.getScript(context, node, Boolean.FALSE);
    }
    else
    {
      return null;
    }
  }

  /**
   * render the client action trigger.
   * (Made this a separate method so that the lovChoice can overwrite it,
   * since the lovChoice needs to render a trigger with a script that is
   * more than the action script)
   */
  protected void renderTrigger(
    UIXRenderingContext context,
    UINode           node,
    ClientAction     action
    ) throws IOException
  {
    action.renderTrigger(context, node);
  }

  /**
   * return true if you need to render the CHOICE_CHANGE_TRACKER and
   * _ACTION_HANDLER_PREFIX javascript.
   * @param context
   * @param node
   * @return boolean true if you need to render the CHOICE_CHANGE_TRACKER
   * javascript.
   */
  protected boolean needsChangeTracker(
    UIXRenderingContext context,
    UINode           node
   )
   {
     return  (ClientActionUtils.getPrimaryClientAction(context,node) != null);
   }

  private Object _getActionHandler(
    UIXRenderingContext context,
    UINode           node,
    Object           attr)
  {
    Object handlerObject = null;
    if (attr == ON_BLUR_ATTR)
      handlerObject = super.getOnBlur(context, node);
    else if (attr == ON_CLICK_ATTR)
      handlerObject = super.getOnClick(context, node);

    // we only need to do special processing on IE when we have an action
    if (!HtmlLafRenderer.isIE(context))
      return handlerObject;

    // The actionScript is a script that will be rendered on the choice.
    // It may be non-null even if there is no primaryClientAction, as is
    // the case in lovChoice.
    String actionScript = getActionScript(context, node);

    // if no action script, we're done
    if (actionScript == null)
      return handlerObject;

    // On IE, if we have a primaryClientAction,
    // we get onChange events whenever a user keys through a SELECT
    // element. Therefore, we can't just fire ClientAction in response to the
    // onChange event, we have to catch at least one change, then fire on the
    // next click or blur. The ACTION_HANDLER_PREFIX makes a call to a method
    // that returns true if state has changed. If there hasn't been a change,
    // the PREFIX will immediately return true, otherwise fall through. If we
    // allow the action script to follow, then the fall through will call it
    // only when there has been a change.

    if (needsChangeTracker(context, node))
      actionScript = _ACTION_HANDLER_PREFIX + actionScript;

    if (handlerObject != null )
    {
      return XhtmlLafUtils.getChainedJS(handlerObject.toString(),
                                        actionScript, true);
    }
    return actionScript;
  }

  // Is this choice in a branch of the tree that is repeated:
  private boolean _isRepeatingChoice(UIXRenderingContext context,
                                     UINode node)
  {
    // check to make sure that repeating property is set, and that this choice
    // has a name (see bug 3194812):
    return ((getNodeName(context, node) != null) &&
            (context.getProperty(MARLIN_NAMESPACE, REPEAT_PROPERTY) != null));
  }

  private boolean _isNothingSelected(
    UIComponent      component,
    List<SelectItem> selectItems)
  {
    // Assume the component is a value holder
    Object value = ((ValueHolder) component).getValue();
    if (value == null)
      return true;
    // now check if none of the items' values match the value.
    int size = selectItems.size();
    for (int i=0; i < size; i++)
    {
      SelectItem item = selectItems.get(i);
      if (value.equals(item.getValue()))
        return false;
    }
    return true;
  }

  private static final Object _SYNC_FUNCTION_PROPERTY = new Object();
  private static final String _CALL_FUNC = "_syncIndex(this)";
  private static final String _FUNC =
      "function _syncIndex(ch){" +
      "var form = ch.form; var name = ch.name;" +
      "var comps = form.elements[name];" +
      "for(i=0; i<comps.length; i++) {" +
      "comps[i].selectedIndex = ch.selectedIndex;}}";
  private static final String _ACTION_HANDLER_PREFIX =
    "if(!_pprChoiceAction(event))return true;";
  private static final String CHOICE_CHANGE_TRACKER =
    "return _pprChoiceChangeEvent(event);";
}
