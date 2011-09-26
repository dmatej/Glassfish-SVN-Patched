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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.collection.Parameter;

/**
 * The ClientAction object is used to associate client-side behavior
 * with a UINode.  The ClientAction is responsible for providing a
 * script which can be called in response to user interaction on
 * the client.  The actual user action that results in the execution
 * of the ClientAction's script varies from component to component.
 * For example, the ClientAction's script might be executed in response
 * to clicking on a link or button or selecting an item from a choice.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/action/ClientAction.java#0 $) $Date: 10-nov-2005.18:57:41 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class ClientAction
{
  /**
   * Returns the script which implements this ClientAction.  The
   * returned script is called from client-side event handlers as required
   * by the specified UINode.
   * The returnVal parameter determines whether the final script will return
   * true or false
   * @param context   The current RenderingContext.
   * @param node      The target UINode.
   * @param returnVal Determines whether the final script will return.
   *                  true or false.
   */
  abstract public String getScript
    (UIXRenderingContext context,
     UINode node,
     Boolean returnVal);

  /**
   * Called by the UIX Component rendering engine before <code>getScript</code>
   * in order to give the ClientAction the opportunity to generate
   * its client-side dependencies before <code>getScript</code>
   * and in a more suitable output scope.
   *
   * @param context   The current RenderingContext.
   * @param node      The target UINode.
   */
  public void writeDependencies(
    UIXRenderingContext context,
    UINode node
    ) throws IOException
  {
    // By default we don't write any dependencies
  }

  /**
   * Tests whether the ClientAction should be invoked using an
   * explicit trigger such as a button or link.
   * <p>
   * In general, ClientActions are executed in response to user interaction
   * with an input control, such as selecting an item from a choice component
   * or radio button group, or tabbing out of a text input component. In some
   * cases the ClientAction may provide a script which should not be fired in
   * response to the normal user input, but instead should only be executed in
   * response to an explicit request from the end user. For example, the
   * partial page rendering ClientActions revert to full page rendering in
   * environments where partial page rendering is not supported (such as
   * Netscape 4.x). However, performing a full page render in response to user
   * interaction such as tabbing out of a text input component would be
   * confusing to the end user. Instead, the {@link #isTriggerRequired
   * isTriggerRequired()} method can be used to indicate that an explicit
   * trigger, such as a "Go" button, should be used to invoke the action.
   * <p>
   * Components which support ClientActions should call this method before
   * calling {@link #getScript getScript()} to determine whether or not an
   * explicit trigger is required. If {@link #isTriggerRequired
   * isTriggerRequired()} returns true, {@link #renderTrigger renderTrigger()}
   * should be called to render content which includes a trigger (a link or
   * button) which invokes the action.
   * <p>
   * Components which are themselves explict triggers (link or button
   * components) may call {@link #getScript getScript()} instead of {@link
   * #renderTrigger renderTrigger()} even if {@link #isTriggerRequired
   * isTriggerRequired()} returns true. In some cases the ClientAction
   * implementation may be able to provide a script which can be invoked by
   * link/button components, in which case an auxiliary trigger is not
   * necessary.
   *
   * @see #renderTrigger renderTrigger()
   *
   * @param context   The current RenderingContext.
   * @param node      The target UINode.
   */
  public boolean isTriggerRequired(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // By default we don't need a trigger
    return false;
  }

  /**
   * Renders a trigger (a link or button) which can be used to
   * invoke the ClientAction.
   * <p>
   * This method should be called by components which support ClientActions
   * when {@link #isTriggerRequired isTriggerRequired()} returns true. It is up
   * to each component to determine when/where the trigger should be rendered.
   * For example, a choice component might render the trigger next to the
   * choice, whereas a radio group would probably render the trigger beneath
   * all of the radio buttons in the group.
   *
   * This method renders a button with the text "GO". Use the three argument
   * method to specify alternate button text.
   *
   * @see #isTriggerRequired isTriggerRequired()
   *
   * @param context   The current RenderingContext.
   * @param node      The target UINode.
   */
  public void renderTrigger(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // By default we don't render a trigger
  }


  /**
   * Renders a trigger (a link or button) which can be used to
   * invoke the ClientAction.
   * <p>
   * This method should be called by components which support ClientActions
   * when {@link #isTriggerRequired isTriggerRequired()} returns true. It is up
   * to each component to determine when/where the trigger should be rendered.
   * For example, a choice component might render the trigger next to the
   * choice, whereas a radio group would probably render the trigger beneath
   * all of the radio buttons in the group.
   *
   * This method renders a button with the given text.
   *
   * @see #isTriggerRequired isTriggerRequired()
   *
   * @param context   The current RenderingContext.
   * @param node      The target UINode.
   */
  public void renderTrigger(
    UIXRenderingContext context,
    UINode           node,
    Object           text
    ) throws IOException
  {
    // By default we don't render a trigger
  }

  /**
   * Returns an array of all parameters to this ClientAction.
   * This array will include any parameters generated by this ClientAction
   * itself (e.g. event, source), and all parameters provided by the client.
   *
   * @param context   The current RenderingContext.
   * @param node      The target UINode.
   */
  public Parameter[] getParameters(
    UIXRenderingContext context,
    UINode           node)
  {
    return null;
  }

  /**
   * Returns true if it's OK to render this ClientAction's script as an event
   * (e.g. onClick, onChange, etc.). If this method ever returns false,
   * interested renderers will try to render in the best way for that renderer
   * using the parameters returned from the {@link #getParameters
   * getParameters()} method, and other information on the parent node. As an
   * example, the {@link org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkRenderer#
   * LinkRenderer} might render an onClick script in the case of a
   * formSubmitted Action, but it can render a simple href if there is nothing
   * special to be done.
   *
   * @see #getParameters getParameters()
   *
   * @param context   The current RenderingContext.
   * @param node      The target UINode.
   */
  public boolean renderAsEvent(
    UIXRenderingContext context,
    UINode           node)
  {
    return true;
  }

  static final String TRUE_RETURN_SCRIPT  =  "return true;";
  static final String FALSE_RETURN_SCRIPT =  "return false;";
}
