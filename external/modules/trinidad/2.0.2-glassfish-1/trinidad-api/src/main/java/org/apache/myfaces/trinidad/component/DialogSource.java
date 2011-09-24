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
package org.apache.myfaces.trinidad.component;

import javax.faces.component.ActionSource;

import javax.el.MethodExpression;

import org.apache.myfaces.trinidad.event.LaunchListener;
import org.apache.myfaces.trinidad.event.ReturnListener;


/**
 * Extension of ActionSource for components that fire
 * dialogs, and need to be notified when the dialog
 * begins launching and completes.
 * 
 */
public interface DialogSource extends ActionSource
{
  public void addReturnListener(ReturnListener listener);
  public void removeReturnListener(ReturnListener listener);
  public ReturnListener[] getReturnListeners();
  public void setReturnListener(MethodExpression returnListener);
  public MethodExpression getReturnListener();

  public void addLaunchListener(LaunchListener listener);
  public void removeLaunchListener(LaunchListener listener);
  public LaunchListener[] getLaunchListeners();
  public void setLaunchListener(MethodExpression launchListener);
  public MethodExpression getLaunchListener();
}
