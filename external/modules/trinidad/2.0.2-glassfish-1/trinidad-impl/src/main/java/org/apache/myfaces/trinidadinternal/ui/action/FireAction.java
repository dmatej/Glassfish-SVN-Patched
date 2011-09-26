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

import java.io.IOException;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.collection.Parameter;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.FormValueRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;

/**
 * ClientAction implementation which fires or submits an event
 * which can be used to re-render dependent components.  When the
 * FireAction is triggered, an "update" event is sent to
 * the application.  The event may specify the following event parameters:
 * <ul>
 * <li>source: The ID of the UINode which triggered the event
 * </ul>
 * <p>
 * The event will be sent either with a form submit or a page redraw.
 * The submit action is provided as a substitute for the
 * JavaScript submitForm() API to allow clients to trigger
 * form submission without having to write JavaScript code.
 * A submitted FormAction always submits the form which
 * contains the UINode that triggers the action.  Clients
 * may optionally specifiy whether the form should be
 * submitted without validation, as well as additional
 * parameters to submit with the form.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/action/FireAction.java#0 $) $Date: 10-nov-2005.18:57:43 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class FireAction extends ClientAction
{
  /**
   * Creates a FireAction which will fire an update event by submiting the
   * enclosing form.
   */
  public FireAction()
  {
    this(null);
  }

  /**
   * Creates a FireAction which will submit the enclosing form with the
   * specified event name.
   *
   * @param event   The name of the event that should be sent when
   *                the FireAction is activated.  If
   *                null, the event name defaults to "update".
   */
  public FireAction(
    String   event
    )
  {
    this(event, true);
  }

  /**
   * Creates a FireAction with the specified event name.
   *
   * @param event   The name of the event that should be sent when
   *                the FireAction is activated.  If
   *                null, the event name defaults to "update".
   * @param submit  Whether or not a form submit will be used to fire the event
   */
  public FireAction(
    String   event,
    boolean  submit
    )
  {
    _submitted = submit;
    _event = event;
  }

  /**
   * Implementation of ClientAction.getScript().
   */
  @Override
  public String getScript
    (UIXRenderingContext context,
     UINode node,
     Boolean returnVal)
  {
    if (BaseLafRenderer.supportsScripting(context))
    {
      String returnScript = (Boolean.TRUE.equals(returnVal)
                             ? TRUE_RETURN_SCRIPT
                             : FALSE_RETURN_SCRIPT);

      return (isFormSubmitted()
              ? getSubmitScript(context, node, returnScript)
              : getChangeScript(context, node, returnScript));
    }
    return null;
  }

  /**
   * Override of ClientAction.writeDependencies().
   */
  @Override
  public void writeDependencies(
    UIXRenderingContext context,
    UINode node
    ) throws IOException
  {
    super.writeDependencies(context, node);

    if (isFormSubmitted())
    {
      // Gotta build formValue fields for any client parameters
      String formName = getFormName(context, node);
      if (formName == null)
        return;

      URLEncoder encoder = context.getURLEncoder();
      String eventKey = encoder.encodeParameter(UIConstants.EVENT_PARAM);
      String sourceKey = encoder.encodeParameter(UIConstants.SOURCE_PARAM);

      FormValueRenderer.addNeededValue(context, formName,
                                       eventKey, sourceKey, null, null);

      Parameter[] parameters = getParameters();
      if (parameters != null)
      {
        // addNeededValue can take up to four parameter names, so we can
        // do these in chunks of four.
        String[] paramKeys = new String[4];
        for (int i = 0; i < parameters.length; i += 4)
        {
          // We're sure that paramKeys[0] will be set below,
          // but clear all others.
          paramKeys[1] = paramKeys[2] = paramKeys[3] = null;

          for (int j = 0; j < 4; j++)
          {
            if (parameters.length > (i + j))
            {
              paramKeys[j] = parameters[i + j].getKey();
            }
            else
              break;
          }

          FormValueRenderer.addNeededValue(context,
                                           formName,
                                           paramKeys[0],
                                           paramKeys[1],
                                           paramKeys[2],
                                           paramKeys[3]);
        }
      }
    }
  }

  protected String getSubmitScript
    (UIXRenderingContext context,
     UINode node,
     String returnScript)
  {
    String startScript;
    if (_isBlocked)
      startScript = _BLOCK_SCRIPT;
    else
      startScript = _SUBMIT_START;
    String endScript = ");";

    // Get the source attr...
    String source = ActionUtils.getSource(context, node,
                                          _sourceBinding, _source);

    // Get the formName
    String formName = getFormName(context, node);
    if (formName == null)
      return null;

    // Get submitForm() arguments
    boolean unvalidated = isUnvalidated(context);
    Parameter[] parameters = _parameters;
    String event = getEventValue(context);
    int length = getBufferSize(context,
                               startScript,
                               endScript,
                               null,
                               formName,
                               event,
                               source,
                               null, null, null, null,
                               returnScript,
                               parameters);

    // Create the buffer
    StringBuilder buffer = new StringBuilder(length);

    // Build up the script
    buffer.append(startScript);
    buffer.append(formName);
    buffer.append("\',");
    buffer.append(unvalidated ? "0" : "1");
    // need to pass formName for FormEncoder
    ActionUtils.appendClientParameters(context, buffer, parameters,
                                       event, source, formName);
    buffer.append(endScript);
    buffer.append(returnScript);

    return buffer.toString();
  }

  protected String getChangeScript
    (UIXRenderingContext context,
     UINode node,
     String returnScript)
  {
    return getChangeScript(context, node, returnScript,
                           FULL_START_SCRIPT, FULL_END_SCRIPT, null, null);
  }

  protected String getChangeScript
    (UIXRenderingContext context,
     UINode node,
     String returnScript,
     String startScript,
     String endScript,
     String extraKey,
     String extraValue
     )
  {
    // Get the event name
    String event = getEventValue(context);

    // Pull the needed attributes from the node
    String source = ActionUtils.getSource(context, node,
                                          _sourceBinding, _source);
    String destination = getDestination(context, node);

    // Get the encoded names for our event parameters
    URLEncoder encoder = context.getURLEncoder();
    String eventKey  = encoder.encodeParameter(UIConstants.EVENT_PARAM);
    String sourceKey = getSourceKey(encoder, source);

    // Get the parameters
    Parameter[] parameters = _parameters;

    // Figure out how big our buffer needs to be
    int length = getBufferSize(context,
                               startScript,
                               endScript,
                               destination,
                               null,
                               event,
                               source,
                               extraValue,
                               eventKey,
                               sourceKey,
                               extraKey,
                               returnScript,
                               parameters);

    // Create the buffer
    StringBuilder buffer = new StringBuilder(length);
    StringBuilder urlBuffer = new StringBuilder(length);
    // Add the start script
    buffer.append(startScript);

    urlBuffer.append(destination);

    // Make sure we are in the query string portion of the URL
    if (destination.indexOf('?') == -1)
      urlBuffer.append("?");

    appendURLParameter(urlBuffer, eventKey, event);
    appendURLParameter(urlBuffer, sourceKey, source);
    if ((extraKey != null) && (extraValue != null))
      appendURLParameter(urlBuffer, extraKey, extraValue);
    appendClientParameters(context, urlBuffer, parameters);
    
    String url = urlBuffer.toString();
    FacesContext facesContext = context.getFacesContext();
    if(facesContext != null)
      url = facesContext.getExternalContext().encodeActionURL(url);

    buffer.append(url);
    buffer.append(endScript);
    buffer.append(returnScript);

    return buffer.toString();
  }

  /**
   * Returns the parameters that are submitted when the
   * action is invoked.
   */
  public final Parameter[] getParameters()
  {
    return ActionUtils.cloneParameterArray(_parameters);
  }

  @Override
  public Parameter[] getParameters(
    UIXRenderingContext context,
    UINode           node)
  {
    // Get the encoded names for our event parameters
    URLEncoder encoder = context.getURLEncoder();
    String eventKey  = encoder.encodeParameter(UIConstants.EVENT_PARAM);
    String sourceKey = encoder.encodeParameter(UIConstants.SOURCE_PARAM);

    Parameter eventParam = ActionUtils.buildParameter(context, node,
                                                      _eventBinding, _event,
                                                      eventKey);
    int numParams = 1;

    // If the parent element doesn't have a name or ID, there'll be no source
    Parameter sourceParam = null;
    if ((_sourceBinding != null) || (_source != null))
    {
      sourceParam = ActionUtils.buildParameter(context, node,
                                               _sourceBinding, _source,
                                               sourceKey);
      numParams++;
    }

    Parameter[] localParams = new Parameter[numParams];

    localParams[0] = eventParam;
    if (sourceParam != null)
      localParams[1] = sourceParam;

    return ActionUtils.joinParameterArrays(localParams, _parameters);
  }

  /**
   * Returns the name of the source that is sent when this
   * ClientAction is activated.
   */
  public final String getSource()
  {
    return _source;
  }

  /**
   * Returns the name of the event that is sent when this
   * ClientAction is activated.
   */
  public final String getEvent()
  {
    return _event;
  }

  /**
   * Tests whether or not this event will be delivered via a form submit.
   * <p>
   * @return Returns true to indicate that the action will cause
   *         a form submit.
   */
  public final boolean isFormSubmitted()
  {
    return _submitted;
  }


  /**
   * Tests whether or not validation is performed when the
   * action is executed.
   * <p>
   * @return Returns true to indicate that the action will
   *   be invoked without performing validation.  Returns false
   *   to indicate that validation will be performed.
   */
  public final boolean isUnvalidated()
  {
    return _unvalidated;
  }

  /**
   * Override of ClientAction.renderAsEvent().
   */
  @Override
  public boolean renderAsEvent(
    UIXRenderingContext context,
    UINode           node)
  {
    // If this action is formSubmitted, we have to render on an event handler.
    // Otherwise, we can render on an href. If there's no scripting support,
    // no we shouldn't render on an event.
    return _submitted && BaseLafRenderer.supportsScripting(context);
  }

  /**
   * Sets the source of this action.
   */
  public void setFormName(String formName)
  {
    _formName = formName;
  }

  /**
   * Sets a BoundValue that will dynamically evaluate the source
   */
  public void setFormNameBinding(BoundValue formNameBinding)
  {
    _formNameBinding = formNameBinding;
  }

  /**
   * Sets the event name.
   */
  public void setEvent(String event)
  {
    _event = event;
  }

  /**
   * Sets a BoundValue that will dynamically evaluate the
   * event name.
   */
  public void setEventBinding(BoundValue eventBinding)
  {
    _eventBinding = eventBinding;
  }

  /**
   * Sets the source of this action.
   */
  public void setSource(String source)
  {
    _source= source;
  }

  /**
   * Sets a BoundValue that will dynamically evaluate the source
   */
  public void setSourceBinding(BoundValue sourceBinding)
  {
    _sourceBinding = sourceBinding;
  }

  /**
   * Sets parameters to submit with the form when the
   * action is invoked.
   */
  public void setParameters(Parameter[] parameters)
  {
    _parameters = parameters == null ? null : parameters.clone();
  }

  /**
   * Sets whether or not the event is sent with a form submit or a GET.
   * <p>
   * @param submitted Boolean value indicating whether or
   *  not this event should be passed via a form submit.
   */
  public void setFormSubmitted(boolean submitted)
  {
    _submitted = submitted;
  }

  /**
   * Sets whether or not validation is performed when the
   * action is executed.
   * <p>
   * @param unvalidated Boolean value indicating whether or
   *  not validation should be performed when invoking the
   *  action.  If true, no validation is performed.  By
   *  default, unvalidated is false, in which case validation
   *  is performed.
   */
  public void setUnvalidated(boolean unvalidated)
  {
    _unvalidated = unvalidated;
  }

  /**
   * Sets a { @link BoundValue } that will dynamically evaluate the
   * unvalidated value.
   */
  public void setUnvalidatedBinding(BoundValue unvalidatedBinding)
  {
    _unvalidatedBinding = unvalidatedBinding;
  }

  // Gets the event value
  protected String getEventValue(UIXRenderingContext context)
  {
    return (String) ActionUtils.getValue(context, _eventBinding, getEvent());
  }

  // Gets the unvalidated value
  protected boolean isUnvalidated(UIXRenderingContext context)
  {
    Boolean unvalidated = (Boolean) ActionUtils.getValue(context,
                                                         _unvalidatedBinding,
                                                         null);
    if (unvalidated != null)
      return unvalidated.booleanValue();

    return _unvalidated;
  }

  // Appends a parameter to a JavaScript function call buffer
  protected static void appendJSParameter(
    StringBuilder buffer,
    String value
    )
  {
    if (buffer.charAt(buffer.length() - 1) != ',')
      buffer.append(",");

    if (value == null)
    {
      buffer.append("0");
    }
    else
    {
      // double escape in-quotes string
      // eg. "\'" + escapeJS("a'b") + "\'" -> "\'a\\\'b\'"
      buffer.append("\'");
      XhtmlLafUtils.escapeJS(buffer, value, true, 2);
      buffer.append("\'");
    }
  }

  // Append a parameter to a URL buffer
  protected static void appendURLParameter(
    StringBuilder buffer,
    String       name,
    String       value
    )
  {
    ActionUtils.appendURLParameter(buffer, name, value);
  }


  protected static void appendClientParameters(
    UIXRenderingContext context,
    StringBuilder     buffer,
    Parameter[]      parameters
    )
  {
    if ((parameters == null) || (parameters.length == 0))
      return;

    for (int i = 0; i < parameters.length; i++)
    {
      Parameter param = parameters[i];
      appendURLParameter(buffer, param.getKey(), param.getValue(context));
    }
  }

  /**
   * Computes the buffer size for client-defined parameters
   */
  protected static int getClientParametersSize(
    UIXRenderingContext context,
    Parameter[]      parameters
    )
  {
    if ((parameters == null) || (parameters.length == 0))
      return 0;


    // Client parameters for fireAction look like this:
    //    &key1=value1&key2=value2
    // or
    //    'key1':'value1','key2':'value2',
    int length = 0;
    for (int i = 0; i < parameters.length; i++)
    {
      Parameter param = parameters[i];

      String value = param.getValue(context);
      // don't count the length if value is null, since we don't append
      // the parameter if value is null in appendJSParameter
      if (value != null)
      {
        length += value.length();
        length += param.getKey().length();
        // Make sure there is room for the & and the =
        // or the ' : and , chars
        length += 5;
      }
    }
    return length;
  }

  // Computes the size of the buffer that is needed to build up the script
  protected static int getBufferSize(
    UIXRenderingContext context,
    String  startScript,
    String  endScript,
    String  destination,
    String  formName,
    String  event,
    String  source,
    String  partialTargets,
    String  eventKey,
    String  sourceKey,
    String  partialTargetsKey,
    String  returnScript,
    Parameter[] parameters
    )
  {
    int length = startScript.length() +
                 endScript.length()   +
                 returnScript.length();

    if (destination != null)
      length += destination.length();

    if (formName != null)
      // tack on extra for the validate flag
      length += formName.length() + 2;

    // Make room for the event parameters.  We need to make sure that
    // there is enough room for the name, value,  the '=' and possible
    // '&' for each parameter.
    if (eventKey != null)
      length += eventKey.length();
    else
      // otherwise leave space for 'event':  and ,
      length += 9;

    if (event != null)
      length += (event.length() + 2);

    if (sourceKey != null)
      length += sourceKey.length();

    if (source != null)
      length += (source.length() + 2);

    if (partialTargetsKey != null)
      length += partialTargetsKey.length();

    if (partialTargets != null)
      length += (partialTargets.length() + 2);

    length += ActionUtils.getClientParametersSize(context, parameters);

    // add on space for a possible closing quote and a comma.
    length += 2;

    return length;
  }

  // Gets the destination attribute of the specified UINode
  protected static String getDestination(UIXRenderingContext context, UINode node)
  {
    Object destination = node.getAttributeValue(context,
                                                UIConstants.DESTINATION_ATTR);

    if (destination != null)
      return destination.toString();

    URLEncoder encoder = context.getURLEncoder();
    return encoder.getDefaultURL();
  }

  // Returns the source event parameter key, if necessary
  protected static String getSourceKey(
    URLEncoder encoder,
    String source
    )
  {
    if (source == null)
      return null;

    return encoder.encodeParameter(UIConstants.SOURCE_PARAM);
  }

  protected String getSource(UIXRenderingContext context, UINode node)
  {
    return ActionUtils.getSource(context, node, _sourceBinding, _source);
  }

  protected String getFormName(UIXRenderingContext context, UINode node)
  {
    String formName = null;

    // First look to the binding for the formName
    if (_formNameBinding != null)
      formName = (String) _formNameBinding.getValue(context);

    // If nothing bound, check for something hardcoded on the action
    if (formName == null)
      formName = _formName;

    // OK, the client has not set a formName,
    // look for the currently scoped form name instead.
    if (formName == null)
    {
      formName = ActionUtils.getFormName(context);
    }

    return formName;
  }

  public void setBlocking(boolean isBlocked)
  {
    _isBlocked = isBlocked;
  }

  private boolean    _isBlocked;
  private boolean    _submitted;

  private String     _formName;
  private BoundValue _formNameBinding;

  private String     _event;
  private BoundValue _eventBinding;

  private String     _source;
  private BoundValue _sourceBinding;

  private boolean    _unvalidated;
  private BoundValue _unvalidatedBinding;

  // Array of parameters provided by client
  private Parameter[] _parameters;

  protected static final String FULL_START_SCRIPT
                                                = "document.location.href=\'";
  protected static final String FULL_END_SCRIPT      =  "\';";
  protected static final String _SUBMIT_START        = "submitForm(\'";
  protected static final String _BLOCK_SCRIPT
    = "_blockOnEverySubmit=true;" + _SUBMIT_START;
}
