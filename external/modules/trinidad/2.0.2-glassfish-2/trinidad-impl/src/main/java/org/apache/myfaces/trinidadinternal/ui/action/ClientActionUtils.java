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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.collection.Parameter;

/**
 * Public utility methods useful for working with ClientActions
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/action/ClientActionUtils.java#0 $) $Date: 10-nov-2005.18:57:42 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ClientActionUtils
{

  /**
   * Gets the ClientAction associated with this UINode if there is one.
   */
  public static ClientAction getPrimaryClientAction(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return (ClientAction) node.getAttributeValue(context,
                                      UIConstants.PRIMARY_CLIENT_ACTION_ATTR);
  }

  public static String appendURLParameters(
    UIXRenderingContext context,
    String       base,
    Parameter[]  params
    )
  {
    return ActionUtils.appendURLParameters(context, base, params);
  }
}
