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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/BorderLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:53:12 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BorderLayoutRenderer extends XhtmlLafRenderer
{
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    Object hasSideNodes = context.getLocalProperty(0, _HAS_SIDE_NODES, null);

    if ( hasSideNodes != null )
      renderLayoutTableAttributes(context, "0", "100%");
  }

  /**
   * writes out the indexed children.
   * @see #renderContent(UIXRenderingContext, UINode)
   */
  protected final void renderIndexedChildren(UIXRenderingContext context,
                                             UINode node) throws IOException
  {
    super.renderContent(context, node);
  }

  /**
   * since this class overrides the method in BaseRenderer, the method
   * renderIndexedChildren is provided to access the superclass method.
   * @see #renderIndexedChildren(UIXRenderingContext, UINode)
   * @see org.apache.myfaces.trinidadinternal.ui.BaseRenderer#renderContent(UIXRenderingContext, UINode)
   */
  @Override
  protected void renderContent(UIXRenderingContext context,
                               UINode node) throws IOException
  {

    Object hasSideNodes = context.getLocalProperty(0, _HAS_SIDE_NODES, null);

    if ( hasSideNodes != null )
    {
      _renderTableContent( context, node );
    }
    else
    {
      ResponseWriter writer = context.getResponseWriter();
      renderNamedChild(context, node, TOP_CHILD);
      writer.startElement(DIV_ELEMENT, null);
      writer.endElement(DIV_ELEMENT);
      renderNamedChild(context, node, INNER_TOP_CHILD);
      writer.startElement(DIV_ELEMENT, null);
      writer.endElement(DIV_ELEMENT);
      
      renderIndexedChildren(context, node);

      writer.startElement(DIV_ELEMENT, null);
      writer.endElement(DIV_ELEMENT);
      renderNamedChild(context, node, INNER_BOTTOM_CHILD);
      writer.startElement(DIV_ELEMENT, null);
      writer.endElement(DIV_ELEMENT);
      renderNamedChild(context, node, BOTTOM_CHILD);
    }
  }

  private void _renderTableContent(UIXRenderingContext context,
                               UINode node) throws IOException
  {

    ResponseWriter writer = context.getResponseWriter();
    Integer rowSpan = _getRowSpan(context, node);
    Integer colSpan = _getColSpan(context, node, rowSpan);
      
    //
    // If we have a header node, render it
    //
    UINode headerNode = getNamedChild(context, node, TOP_CHILD);
    if (headerNode != null)
    {
      writer.startElement("tr", null);
      renderMargin(context, node, null);

      writer.startElement("td", null);
      writer.writeAttribute("colspan", colSpan, null);
      renderNamedChild(context, node, headerNode, TOP_CHILD);
      writer.endElement("td");

      renderMargin(context, node, null);
      writer.endElement("tr");
    }

    //
    // Begin rendering the content row
    //
    writer.startElement("tr", null);
    renderMargin(context, node, rowSpan);


    //
    // Render the left hand side
    //
    String leftName = _getSideNode(context, node, true);
    UINode leftNode = getNamedChild(context, node, leftName);
    renderLeftSideNode(context, node, leftName, leftNode, rowSpan, colSpan);
  
    //
    // Render the inner left side node, if any
    //
    String innerleftName = _getInnerSideNode(context, node, true);
    UINode innerLeftNode = getNamedChild(context, node, innerleftName);

    if (innerLeftNode != null)
    {
      renderSideNode(context, node, innerLeftNode,
                     innerleftName, rowSpan, null);
    }
  
    //
    // Render the child on the inside top of the layout
    //
    boolean isRightSideRendered =
      _renderMiddleNode(context, node, INNER_TOP_CHILD, rowSpan,
                        false, true);

        
    // render the content
    if (node.getIndexedChildCount(context) > 0)
    {
      /* if the right hand side has been rendered then we need to start a new
         row. otherwise, we can still render on to the previous row. */
      if (isRightSideRendered)
      {
        writer.startElement("tr", null);
      }
        
      writer.startElement("td", null);
      writer.writeAttribute("width", "100%", null);
      writer.writeAttribute("valign", "top", null);
        
      super.renderContent(context, node);

      writer.endElement("td");

      /* if the right hand side nodes have not been rendered, then render them
         here */
      if (!isRightSideRendered)
      {
        _renderRightNodes(context, node, rowSpan);
        isRightSideRendered = true;
      }

      writer.endElement("tr");
    }

    /* render the inner bottom child. start a new TR only if the 
       right side nodes have been rendered, and render the 
       right side nodes if they have not been rendered.  */
    isRightSideRendered |=
      _renderMiddleNode(context, node, INNER_BOTTOM_CHILD, rowSpan,
                        isRightSideRendered,
                        !isRightSideRendered);

    /* if a right hand side still has not been rendered then we need
       to render it here */
    if (!isRightSideRendered) 
    {
      _renderRightNodes(context, node, rowSpan);
      writer.endElement("tr");
    }

    //
    // render the  footer node
    //
    UINode footerNode = getNamedChild(context, node, BOTTOM_CHILD);
    if (footerNode != null)
    {       
      writer.startElement("tr", null);
      renderMargin(context, node, null);
        
      writer.startElement("td", null);
      writer.writeAttribute("colspan", colSpan, null);
      renderNamedChild(context, node, footerNode, BOTTOM_CHILD);
      writer.endElement("td");
        
      renderMargin(context, node, null);
      writer.endElement("tr");
    }
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    boolean hasSideNodes = hasSideNodes(context, node );

    if ( hasSideNodes )
    {
      context.setLocalProperty( _HAS_SIDE_NODES, Boolean.TRUE);
      return TABLE_ELEMENT;
    }

    return DIV_ELEMENT;
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.postrender( context, node);
    context.setLocalProperty( _HAS_SIDE_NODES, null);
  }

  /**
   * this method renders white space as the margin.
   * @param rowSpan the numnber of rows this margin must span. maybe null to
   * indicate a single row.
   */
  protected void renderMargin(UIXRenderingContext context,
                              UINode node,
                              Integer rowSpan) throws IOException
  {
    _renderMarginSpacer(context, node, rowSpan);
  }

  /**
   * renders the left most side node in this borderLayout
   * @param leftName the name of the namedChild to use
   * @param leftNode the namedChild node corresponding to <code>leftName</code>
   * @param rowSpan the number of table rows between the TOP and BOTTOM nodes.
   *   there is a row for each innerTop, contents and innerBottom namedChildren.
   * @param colSpan the number of table columns between the margins.
   *  there is a column for each of the two side nodes and inner side nodes and the
   *  contents.
   */
  protected void renderLeftSideNode(UIXRenderingContext context,
                                    UINode node,
                                    String leftName,
                                    UINode leftNode,
                                    Integer rowSpan,
                                    Integer colSpan) throws IOException
  {
    if (leftNode != null)
    {
      renderSideNode(context, node, leftNode, leftName, rowSpan, null);
    }
  }

  /**
   * Renders the right hand side nodes.
   * @return true if at least one node was rendered.
   */
  private boolean _renderRightNodes(
    UIXRenderingContext context,
    UINode           node,
    Integer          rowSpan
    ) throws IOException
  {
    boolean nodeRendered = false;
    
    //
    // Render the inner right node.
    //
    String innerSideName = _getInnerSideNode(context, node, false);
    UINode innerSideNode = getNamedChild(context, node, innerSideName);
    
    if (innerSideNode != null)
    {
      renderSideNode(context,
                      node,
                      innerSideNode,
                      innerSideName,
                      rowSpan,
                      null);

      nodeRendered = true;
    }
    
    //
    // Render the side node
    //
    String sideName = _getSideNode(context, node, false);
    UINode sideNode = getNamedChild(context, node, sideName);

    if (sideNode != null)
    {
      renderSideNode(context, node, sideNode, sideName, rowSpan, null);
      renderMargin(context, node, rowSpan);
      
      nodeRendered = true;
    }
    
    return nodeRendered;
  }
  
  /**
   * renders middle children like innerTop and innerBottom. Always ends a table
   * row.
   * @param middleNodeName the name of the child to render.
   * @param startTableRow if true will start a new table row.
   * @param renderRightNodes if true renders the right hand side nodes before
   *  ending the table row.
   * @param rowSpan the row span of the right hand side nodes.
   * @return true if something was rendered. false otherwise.
   */
  private boolean _renderMiddleNode(UIXRenderingContext context,
                                    UINode node,
                                    String middleNodeName,
                                    Integer rowSpan,
                                    boolean startTableRow,
                                    boolean renderRightNodes)
    throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    UINode middleNode = getNamedChild(context, node, middleNodeName);
    if (middleNode!=null)
    {
      if (startTableRow) 
        writer.startElement("tr", null);
    
      _renderInnerNode(context, node, middleNode, middleNodeName);

      if (renderRightNodes)
        _renderRightNodes(context, node, rowSpan);

      writer.endElement("tr");
      return true;
    }
    return false;
  }

  /**
   * Renders one of the inner nodes.
   */
  private void _renderInnerNode(
    UIXRenderingContext context,
    UINode           node,
    UINode           innerNode,
    String           innerNodeName
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("td", null);
    writer.writeAttribute("width", "100%", null);
    renderNamedChild(context, node, innerNode, innerNodeName);
    writer.endElement("td");
  }
  

  /**
   * Renders one of the side nodes.
   */
  protected void renderSideNode(
    UIXRenderingContext context,
    UINode           node,
    UINode           sideNode,
    String           sideNodeName,
    Integer          rowSpan,
    String           width
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("td", null);
    writer.writeAttribute("valign", "top", null);
    writer.writeAttribute("rowspan", rowSpan, null);
    writer.writeAttribute("width", width, null);
    renderNamedChild(context, node, sideNode, sideNodeName);
    writer.endElement("td");
  }


  /**
   * Returns the row span to use for the left and right children
   */
  private Integer _getRowSpan(
    UIXRenderingContext context,
    UINode           node
    )
  {
    int rowSpan = 0;
    
    // increment the rowspan if we have an inner top child
    if (getNamedChild(context, node, INNER_TOP_CHILD) != null)
    {
      rowSpan++;
    }

    // increment the colspan if we have any content children.
    if (node.getIndexedChildCount(context) > 0)
    {
      rowSpan++;
    }

    // increment the rowspan if we have an inner bottom child
    if (getNamedChild(context, node, INNER_BOTTOM_CHILD) != null)
    {
      rowSpan++;
    }
    
    // convert to an Integer
    return (rowSpan > 0) ? getInteger(rowSpan) : null;
  }


  /**
   * Returns the column span to use for the top and bottom children.
   */
  private Integer _getColSpan(
    UIXRenderingContext context,
    UINode           node,
    Integer          rowSpan
    )
  {
    int colSpan = 0;
    
    // increment the colspan if we have a left side node
    if (hasNamedChild(context, node,
                      _getSideNode(context, node, true)))
    {
      colSpan++;      
    }

    // increment the colspan if we have an inner left side node
    if (hasNamedChild(context, node,
                      _getInnerSideNode(context, node, true)))
    {
      colSpan++;      
    }

    // increment the colspan if we have any center nodes.
    if (rowSpan != null)
    {
      colSpan++;
    }

    // increment the colspan if we have an inner right side node
    if (hasNamedChild(context, node,
                      _getInnerSideNode(context, node, false)))
    {
      colSpan++;      
    }
    
    // increment the colspan if we have a right side node
    if (hasNamedChild(context, node,
                      _getSideNode(context, node, false)))
    {
      colSpan++;
    }

    // convert to an Integer
    return (colSpan > 0) ? getInteger(colSpan) : null;
  }


  private void _renderMarginSpacer(
    UIXRenderingContext context,
    UINode           node,
    Integer          rowSpan
    ) throws IOException
  {
    Integer marginIndent = _getMarginIndent(context, node);
    
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("td", null);
    writer.writeAttribute("rowspan", rowSpan, null);
    
    if (marginIndent != null)
    {
      renderSpacer(context, marginIndent, _ZERO);
    }
    
    writer.endElement("td");
  }
    
  
  private Integer _getMarginIndent(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Integer indent = (Integer)node.getAttributeValue(context, CELL_PADDING_ATTR);
    
    if (indent == null)
    {
      int marginIndent = getDefaultMarginIndent(context, node);
      
      if (marginIndent > 0)
      {
        indent = getInteger(marginIndent);
      }
    }

    return indent;
  }
  
  
  /**
   * Returns the default marign indent to use if no CELL_PADDING_ATTR
   * is specified
   */
  protected int getDefaultMarginIndent(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return 0;
  }
  
  
  /**
   * Returns the name of the node to use for the inner left or right side of the
   * layout, following bi-di rules.
   */
  private String _getInnerSideNode(
    UIXRenderingContext context,
    UINode           parentNode,
    boolean          getLeftName
    )
  {
    boolean isRTL = isRightToLeft(context);

    //
    // A precisely specified name wins over a dynamic name
    //
    // Since the browser flips for us in a bidi environment,
    // we actually flip the left and right child if the
    // direction is exact
    //

    String exactName = (isRTL)
                         ? (getLeftName)
                             ? INNER_RIGHT_CHILD
                             : INNER_LEFT_CHILD
                         : (getLeftName)
                             ? INNER_LEFT_CHILD
                             : INNER_RIGHT_CHILD;

    if (!hasNamedChild(context, parentNode, exactName))
    {
      //
      // if not exact, fall back on the start or end child
      //

      exactName = (getLeftName) ? INNER_START_CHILD : INNER_END_CHILD;
    }

    return exactName;
  }

  /**
   * Returns the name of the node to use for the left or right side of the
   * layout, following bi-di rules.
   */
  private String _getSideNode(
    UIXRenderingContext context,
    UINode           parentNode,
    boolean          getLeftName
    )
  {
    boolean isRTL = isRightToLeft(context);

    //
    // A precisely specified name wins over a dynamic name
    //
    // Since the browser flips for us in a bidi environment,
    // we actually flip the left and right child if the
    // direction is exact
    //

    String exactName = (isRTL)
                         ? (getLeftName)
                             ? RIGHT_CHILD
                             : LEFT_CHILD
                         : (getLeftName)
                             ? LEFT_CHILD
                             : RIGHT_CHILD;

    if (!hasNamedChild(context, parentNode, exactName))
    {
      //
      // if not exact, fall back on the start or end child
      //

      exactName = (getLeftName) ? START_CHILD : END_CHILD;
    }

    return exactName;
  }

  /**
   * @return true if this renderer has any side nodes, or has a quick-search
   *  node
   */
  protected boolean hasSideNodes(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return (hasRenderedNamedChild(context, node, LEFT_CHILD)         ||
            hasRenderedNamedChild(context, node, RIGHT_CHILD)        ||
            hasRenderedNamedChild(context, node, START_CHILD)        ||
            hasRenderedNamedChild(context, node, END_CHILD)          ||
            hasRenderedNamedChild(context, node, INNER_LEFT_CHILD)   ||
            hasRenderedNamedChild(context, node, INNER_RIGHT_CHILD)  ||
            hasRenderedNamedChild(context, node, INNER_START_CHILD)  ||
            hasRenderedNamedChild(context, node, INNER_END_CHILD));
  }

  private static final Integer _ZERO = Integer.valueOf(0);
  private static final Object _HAS_SIDE_NODES = new Object();
}
