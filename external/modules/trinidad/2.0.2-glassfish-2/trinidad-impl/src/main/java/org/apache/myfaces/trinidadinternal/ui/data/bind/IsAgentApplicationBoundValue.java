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
package org.apache.myfaces.trinidadinternal.ui.data.bind;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * BoundValue that returns a Boolean based on the Agent Application.
 * <p>
 *@version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/IsAgentApplicationBoundValue.java#0 $) $Date: 10-nov-2005.18:56:40 $
 *@see org.apache.myfaces.trinidadinternal.share.agent.Agent
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class IsAgentApplicationBoundValue implements BoundValue
{
  /**
   * Construct an instance of AgentApplicationBoundValue.
   * @param application the agent application that is being checked for.
   * The values should be one of the APPLICATION constants in
   *  org.apache.myfaces.trinidadinternal.share.agent.Agent
   */
  public IsAgentApplicationBoundValue( int application  )
  {
    _app = application;
  }

  /**
   * @return a Boolean indicating whether what was passed to the constructor
   * matches what is returned from context.getAgent().getAgentApplication()
   */
  public Object getValue(
    UIXRenderingContext context
    )
  {
    return Boolean.valueOf(_app == context.getAgent().getAgentApplication().ordinal());
  }

  private int _app;
}
