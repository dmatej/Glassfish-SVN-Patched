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
package org.apache.myfaces.trinidadinternal.ui.beans.message;

import org.apache.myfaces.trinidadinternal.ui.MutableUINode;

/**
 * Interface implemented by all components that support
 * messaging.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/beans/message/MessageBean.java#0 $) $Date: 10-nov-2005.18:57:40 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface MessageBean extends MutableUINode
{
  /**
   * Gets the label of the bean.
   */
  public String getLabel();

  /**
   * Sets the label of the bean.
   */
  public void setLabel(String label);

  /**
   * Gets the tip text associated with the control
   */
  public String getTip();

  /**
   * Sets the tip text associated with the control
   */
  public void setTip(String tip);

  /**
   * Gets the type of the message; acceptable values
   * are "error", "warning", "info", and "none".  Defaults to "none".
   *
   */
  public String getMessageType();

  /**
   * Sets the type of the message; acceptable values
   * are "error", "warning", "info", and "none".  Defaults to "none".
   *
   */
  public void setMessageType(String messageType);

  /**
   * Binds the the type of the message; acceptable values
   * are "error", "warning", "info", and "none".  Defaults to "none".
   *
   * @param selectKey the key to be passed to <code>selectValue()</code> on the
   *     current <code>DataObject</code>, e.g., the DataObject for the current
   *     table row.
   */
  public void setMessageTypeBinding(Object selectKey);


  /**
   * Binds the the type of the message; acceptable values
   * are "error", "warning", "info", and "none".  Defaults to "none".
   *
   * @param dataNamespace the namespace to be passed to
   *     <code>RenderingContext.getDataObject()</code>
   * @param dataName the name to be passed to
   *     <code>RenderingContext.getDataObject()</code>
   * @param selectKey the key to be passed to <code>selectValue()</code> on the
   *     <code>DataObject</code>.
   */
  public void setMessageTypeBinding(
    String dataNamespace,
    String dataName,
    Object selectKey);


  /**
   * Gets the error, warning, or informational text.
   */
  public String getMessage();

  /**
   * Sets the error, warning, or informational text.
   */
  public void setMessage(String message);

  /**
   * Binds the the error, warning, or informational text.
   * @param selectKey the key to be passed to <code>selectValue()</code> on the
   *     current <code>DataObject</code>, e.g., the DataObject for the current
   *     table row.
   */
  public void setMessageBinding(Object selectKey);


  /**
   * Binds the the error, warning, or informational text.
   * @param dataNamespace the namespace to be passed to
   *     <code>RenderingContext.getDataObject()</code>
   * @param dataName the name to be passed to
   *     <code>RenderingContext.getDataObject()</code>
   * @param selectKey the key to be passed to <code>selectValue()</code> on the
   *     <code>DataObject</code>.
   */
  public void setMessageBinding(
    String dataNamespace,
    String dataName,
    Object selectKey);


  /**
   * Gets an URL to a page with more information about
   * the message.
   */
  public String getLongDescURL();

  /**
   * Sets an URL to a page with more information about
   * the message.
   */
  public void setLongDescURL(String longDescURL);

  /**
   * Binds the an URL to a page with more information about
   * the message.
   * @param selectKey the key to be passed to <code>selectValue()</code> on the
   *     current <code>DataObject</code>, e.g., the DataObject for the current
   *     table row.
   */
  public void setLongDescURLBinding(Object selectKey);


  /**
   * Binds the an URL to a page with more information about
   * the message.
   * @param dataNamespace the namespace to be passed to
   *     <code>RenderingContext.getDataObject()</code>
   * @param dataName the name to be passed to
   *     <code>RenderingContext.getDataObject()</code>
   * @param selectKey the key to be passed to <code>selectValue()</code> on the
   *     <code>DataObject</code>.
   */
  public void setLongDescURLBinding(
    String dataNamespace,
    String dataName,
    Object selectKey);


  /**
   * Gets the target frame for the URL, if any.
   */
  public String getTargetFrame();

  /**
   * Sets the target frame for the URL, if any.
   */
  public void setTargetFrame(String targetFrame);

  /**
   * Gets whether the associated control requires user input.  Three values
   * are allowed: "yes", "no", and "default".  A visual indication will only be
   * displayed if the value is set to "yes".
   */
  public String getRequired();

  /**
   * Sets whether the associated control requires user input.  Three values
   * are allowed: "yes", "no", and "default".  A visual indication will only be
   * displayed if the value is set to "yes".
   */
  public void setRequired(String required);
}
