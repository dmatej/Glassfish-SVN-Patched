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

import javax.faces.context.FacesContext;

/**
 * Class that represents a change that is tied to a component in the component tree.
 * This class is used by {@link ComponentContextManager} to be able to suspend and resume
 * the context of a component during an invoke on component or visit tree call.
 * <p>Note that implementing classes are encouraged to override the to string function
 * to describe the change. This enhances the ability to debug should the stack ever become
 * out of sync.</p>
 */
public abstract class ComponentContextChange
{
  /**
   * Suspends any changes that the component has made so that an invoke on component or
   * visit tree call may be made without the context of the component interfering.
   *
   * @param facesContext The faces context
   */
  public abstract void suspend(FacesContext facesContext);

  /**
   * Resumes the suspended context of a component.
   *
   * @param facesContext The faces context
   */
  public abstract void resume(FacesContext facesContext);
}
