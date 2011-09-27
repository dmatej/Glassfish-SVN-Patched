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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;

// Need pda version of this because version in base.xhtml
// relies on the fact that <div></div> doesn't take up vertical
// space, but it does on a pocket pc

/**
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/StackLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:55:05 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class StackLayoutRenderer extends XhtmlLafRenderer
{  
  @Override
  protected void renderIndexedChild(
    UIXRenderingContext context,
    UINode           node,
    int              currVisChildIndex,
    int              prevVisChildIndex,
    int              nextVisChildIndex,
    int              ithRenderedChild
    ) throws IOException
  {   

    // don't render a div around the first or last item
    // as traditionally in stackLayout these items can flow into 
    // previous and subsequent nodes.
    if ( prevVisChildIndex == NO_CHILD_INDEX ||
         nextVisChildIndex == NO_CHILD_INDEX )
    {
      super.renderIndexedChild(context, node, currVisChildIndex);      
    }
    else
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("div", null);
      super.renderIndexedChild(context, node, currVisChildIndex);
      writer.endElement("div");
    }
  }

  @Override
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node,
    int              nextIndex
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // get the child about to be rendered
    UINode nextChild = node.getIndexedChild( context, nextIndex);


    // if the next child about to be rendered is not visible to the user
    // then don't render anything
    // =-=AEW This doesn't catch formValues inside of contextPopping nodes
    if (!nextChild.getNodeRole(context).satisfiesRole(
                                      USER_INVISIBLE_ROLE))
    {
      UINode separatorChild = getNamedChild(context, node, SEPARATOR_CHILD);
      if (separatorChild != null)
      {
        writer.startElement("div", null);
        renderNamedChild(context, node, separatorChild, SEPARATOR_CHILD);
        writer.endElement("div");
      }
    }    
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return "span";
  }
}
