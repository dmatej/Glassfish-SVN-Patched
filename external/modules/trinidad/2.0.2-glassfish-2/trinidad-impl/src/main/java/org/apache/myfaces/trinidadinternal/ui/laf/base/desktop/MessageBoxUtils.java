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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.util.Iterator;

import javax.faces.application.FacesMessage;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * Renders a message box.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/MessageBoxUtils.java#0 $) $Date: 10-nov-2005.18:55:26 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class MessageBoxUtils implements UIConstants
{

  public static FacesMessage.Severity sGetMaxSeverity(UIXRenderingContext context)
  {
    // This doesn't seem to work if there is a global message
    // return context.getFacesContext().getMaximumSeverity();
    FacesMessage.Severity max = FacesMessage.SEVERITY_INFO;
    Iterator<MessageWrapper> itr = sGetIterator(context, true);
    while (itr.hasNext())
    {
      FacesMessage nxt = itr.next();
      FacesMessage.Severity sev = nxt.getSeverity();
      if (sev.compareTo(max) > 0)
      {
        max = sev;
        if (FacesMessage.SEVERITY_FATAL.compareTo(max) == 0)
          return FacesMessage.SEVERITY_FATAL;
      }
    }
    return max;
  }

  public static String sGetMaxType(UIXRenderingContext context)
  {
    String type = UIConstants.MESSAGE_TYPE_INFO;
    FacesMessage.Severity sev = sGetMaxSeverity(context);

    if (FacesMessage.SEVERITY_ERROR.compareTo(sev) == 0)
      type = UIConstants.MESSAGE_TYPE_ERROR;
    else if (FacesMessage.SEVERITY_WARN.compareTo(sev) == 0)
      type = UIConstants.MESSAGE_TYPE_WARNING;
    else if (FacesMessage.SEVERITY_FATAL.compareTo(sev) == 0)
      type = UIConstants.MESSAGE_TYPE_ERROR;

    return type;
  }

  public static boolean sIsRendered(
    UIXRenderingContext context,
    UINode node,
    boolean allMessages
    )
  {
    Iterator<MessageWrapper> itr = sGetIterator(context, allMessages);
    return itr.hasNext();
  }

  public static Iterator<MessageWrapper> sGetIterator(
    UIXRenderingContext context,
    boolean allMessages)
  {
    return new AllMessageIterator(context.getFacesContext(),
                                  allMessages, false);
  }

  public static Iterator<MessageWrapper> sGetGlobalsIterator(
    UIXRenderingContext context)
  {
    return new AllMessageIterator(context.getFacesContext(), false, false);
  }

  public static Iterator<MessageWrapper> sGetClientsIterator(
    UIXRenderingContext context)
  {
    return new AllMessageIterator(context.getFacesContext(), true, true);
  }


  public static Object sGetMessage(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // render the message text
    Object message = node.getAttributeValue(context, MESSAGE_ATTR);

    return message;
  }

  public static boolean sMultipleMessages(
    UIXRenderingContext context,
    boolean allMessages)
  {
    boolean isMult = false;
    // First find out if we have multiple messages.
    Iterator<MessageWrapper> itr = sGetIterator(context, allMessages);
    for (int c = 0; (!isMult) && itr.hasNext(); c++)
    {
      itr.next();
      isMult = (c == 1);
    }
    return isMult;
  }
}
