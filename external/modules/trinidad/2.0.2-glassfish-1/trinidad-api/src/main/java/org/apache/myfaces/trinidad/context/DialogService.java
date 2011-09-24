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
package org.apache.myfaces.trinidad.context;

import java.util.Map;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.event.ReturnEvent;



/**
 * The DialogService API defines a number of
 * APIs that are needed to implement Apache Trinidad dialogs,
 * but will only rarely be called by page authors.
 * They are instead intended for developers of controller
 * frameworks and component developers.
 */
abstract public class DialogService
{
  /**
   * Configuration parameter for setting the prefix used in
   * dialog navigation.
   */
  public static final String DIALOG_NAVIGATION_PREFIX_PARAM_NAME =
    "org.apache.myfaces.trinidad.DIALOG_NAVIGATION_PREFIX";

  /**
   * Configuration parameter for setting the prefix used in
   * dialog navigation.  This can either be set as a WEB-INF/web.xml
   * or programatically set as a ServletContext attribute.
   */
  public static final String DISABLE_DIALOG_OUTCOMES_PARAM_NAME =
    "org.apache.myfaces.trinidad.DISABLE_DIALOG_OUTCOMES";

  /**
   * Create an DialogService.
   */
  protected DialogService()
  {
  }

  /**
   * Push a UIViewRoot onto a stack in preparation
   * for navigating to a subflow.
   */
  public abstract void pushView(UIViewRoot viewRoot);

  /**
   * Pop a UIViewRoot from a stack.  If <code>navigateToPopped</code> is true,
   * this method may result in calls to
   * <code>FacesContext.renderResponse()</code> or even
   * <code>FacesContext.responseComplete()</code>.
   *
   * @param navigateToPopped If true, navigate to the view popped
   * of the stack with <code>FacesContext.setViewRoot()</code>.
   * If false, simply drop the view.
   */
  public abstract void popView(boolean navigateToPopped);


  /**
   * Returns the UIViewRoot that is topmost on the stack,
   * of pushed view, without popping it.
   */
  public abstract UIViewRoot peekView();

  /**
   * Creates a {@link org.apache.myfaces.trinidad.event.ReturnEvent} for
   * a component.  This method will generally be called from
   * a Renderer's or UIComponent's decode() method if there is
   * any possibility that it launched a dialog on a prior request.
   * There is no requirement that the component directly supports
   * {@link org.apache.myfaces.trinidad.event.ReturnListener};  decode()
   * can simply use the return value and discard the event.
   * <p>
   * This method will return null in the case where no dialog
   * had been launched with this component as the source.  If
   * it returns a non-null event, the component or renderer should
   * not process the request any further in decode(), but simply
   * process the ReturnEvent, either by using its return value
   * or queueing it for delivery.
   *
   * @param source the component that may have launched a dialog
   * @return a ReturnEvent containing the
   * {@link RequestContext#returnFromDialog return value} from the
   * dialog.
   */
  public abstract ReturnEvent getReturnEvent(UIComponent source);

  /**
   * Returns the value last set by {@link #setCurrentLaunchSource}.
   * =-=AEW Make this abstract?
   */
  public UIComponent getCurrentLaunchSource()
  {
    return _currentLaunchSource;
  }

  /**
   * A component that is
   * delivering an ActionEvent to the default ActionListener
   * should call this method to allow a NavigationHandler to
   * properly launch a dialog, and should reset the
   * value to null afterwards.
   * =-=AEW Make this abstract?
   */
  public void setCurrentLaunchSource(UIComponent component)
  {
    _currentLaunchSource = component;
  }


  /**
   * Launches a dialog without pushing a process scope.
   * This method should only be used by controller framework
   * code;  all others should use
   * {@link RequestContext#launchDialog RequestContext.launchDialog()}.
   * The process scope must be {@link PageFlowScopeProvider#pushPageFlowScope pushed}
   * <em>before</em> calling this method.
   */
  public abstract void launchDialog(
    UIViewRoot  dialogRoot,
    Map<String, Object> dialogParameters,
    UIComponent source,
    boolean     useWindow,
    Map<String, Object> windowProperties);


  /**
   * Returns from a dialog without popping a process scope.
   * This method should only be used by controller framework
   * code;  all others should use
   * {@link RequestContext#returnFromDialog RequestContext.returnFromDialog()}.
   * The process scope must be {@link PageFlowScopeProvider#popPageFlowScope popped}
   * <em>after</em> calling this method.
   *
   * @return true if pages accessing that dialog are necessarily permanently
   *   inaccessible.  For example, a dialog displayed in a popup
   *   window is inacessible once the window has closed, but a dialog
   *   displayed within the same window might be reached by the back button.
   */
  public abstract boolean returnFromDialog(Object returnValue,
                                           Map<Object, Object> returnParameters);


  /**
   * Queues a LaunchEvent that will result in a dialog being started, using
   * {@link #getCurrentLaunchSource current launch source} as the source
   * for launching the dialogRoot parameter.
   * The process scope must be {@link PageFlowScopeProvider#pushPageFlowScope pushed}
   * <em>before</em> calling this method.  If {@link #getCurrentLaunchSource}
   * returns <code>null</code>, a basic dialog will be started without
   * using a window or passing any additional parameters.  Developers
   * <em>should not</em> call
   * {@link javax.faces.context.FacesContext#setViewRoot
   * FacesContext.setViewRoot()} when using this method.
   */
  public abstract void queueLaunchEvent(UIViewRoot dialogRoot);

 /**
   * Queues a ReturnEvent, using
   * {@link #getCurrentLaunchSource current launch source} as the source
   * for launching the dialogRoot parameter.  This method should
   * be used by a <code>NavigationHandler</code> that can identify
   * a return value from a dialog without actually launching the dialog.
   */
  public abstract void queueReturnEvent(
    Object returnValue,
    Map<Object, Object> returnParams);

  /**
   * Returns the prefix that, when used for navigational outcomes,
   * will trigger the dialog framework.
   */
  public String getDialogNavigationPrefix()
  {
    if (_dialogPrefix == null)
    {
      FacesContext context = FacesContext.getCurrentInstance();
      _dialogPrefix = context.getExternalContext().getInitParameter(
                                DIALOG_NAVIGATION_PREFIX_PARAM_NAME);
      
      if(_dialogPrefix == null)
      {
        _dialogPrefix = _DEFAULT_DIALOG_NAVIGATION_PREFIX;
      }
    }

    return _dialogPrefix;
  }

  private UIComponent _currentLaunchSource;
  private static String _dialogPrefix;

  private static final String _DEFAULT_DIALOG_NAVIGATION_PREFIX = "dialog:";
}
