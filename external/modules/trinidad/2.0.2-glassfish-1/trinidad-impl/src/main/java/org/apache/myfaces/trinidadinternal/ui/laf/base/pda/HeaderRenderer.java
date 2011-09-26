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
import java.util.HashMap;
import java.util.Map;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.core.layout.CorePanelHeader;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/HeaderRenderer.java#0 $) $Date: 10-nov-2005.18:54:56 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class HeaderRenderer
        extends org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.HeaderRenderer
{
    private int _getSize(
            UIXRenderingContext context,
            UINode           node
            )throws IOException
    {
        setPrevHeaderSize( context );

        Number sizeNumber = (Number)getAttributeValue(context, node,
                SIZE_ATTR, null);

        int size;
        if (sizeNumber != null )
        {
            size = sizeNumber.intValue();
            setContextHeaderSize( context, sizeNumber);
        }
        else
        {
            sizeNumber = getContextHeaderSize(context, null);

            if ( sizeNumber == null )
                size = 0;
            else
                size = sizeNumber.intValue() + 1;

            setContextHeaderSize( context, size);
        }

        return size;
    }

    @Override
    protected void prerender(
            UIXRenderingContext context,
            UINode           node
            ) throws IOException
    {
        int size = _getSize(context, node);

        String messageType =
                XhtmlLafUtils.getStringAttributeValue(context,
                        node,
                        MESSAGE_TYPE_ATTR);

        Object label = getText( context, node, messageType);




        ResponseWriter writer = context.getResponseWriter();


        writer.startElement(DIV_ELEMENT, node.getUIComponent());
        super.renderAttributes( context, node);
        super.prerender(context, node);
        
       //jmw use H1, H2, etc instead. String headerElement = DIV_ELEMENT;
             // add the h1 tags so I can use the same styles!
        String headerElement = (size < HEADER_ELEMENT.length)
                                ? HEADER_ELEMENT[size] 
                                : HEADER_ELEMENT[HEADER_ELEMENT.length-1];        
        writer.startElement(headerElement, null);
        Object styleClass = AF_PANEL_HEADER_STYLE_CLASS;

        if ( MESSAGE_TYPE_ERROR.equals( messageType ))
        {
            styleClass = AF_PANEL_HEADER_ERROR_STYLE_CLASS;
        }
        else
        {

            styleClass = AF_PANEL_HEADER_STYLE_CLASS;
        }

        if (renderStyleElements(context))
            startRenderingStyleElements(context, null, styleClass);
        else
            renderStyleClassAttribute(context, styleClass);
   
        renderIcon(context, node);

        if (label != null)
          writer.writeText(label, CorePanelHeader.TEXT_KEY.getName());

        if (renderStyleElements(context))
            XhtmlLafUtils.endRenderingStyleElements(context);

        writer.endElement(headerElement);


        // increment header nesting
        incrementHeaderNestLevel(context);


    }


    protected void renderIcon(
            UIXRenderingContext context,
            UINode           node
            )throws IOException
    {
        ResponseWriter writer = context.getResponseWriter();

        String messageType =
                XhtmlLafUtils.getStringAttributeValue(context,
                        node,
                        MESSAGE_TYPE_ATTR);
        Object iconURI = getIconURI( context, node, messageType );
        if( iconURI != null )
        {
            writer.startElement(IMAGE_ELEMENT, null);
            writer.writeAttribute(ALT_ATTRIBUTE, EMPTY_STRING_ATTRIBUTE_VALUE, null);
            renderEncodedResourceURI(context, SOURCE_ATTRIBUTE, iconURI);
            writer.endElement(IMAGE_ELEMENT);
        }

    }
    
    @Override
    protected void postrender(
            UIXRenderingContext context,
            UINode           node
            ) throws IOException
    {

        decrementHeaderNestLevel(context);
        resetHeaderSize(context);
        ResponseWriter writer = context.getResponseWriter();
        super.postrender(context, node);
        writer.endElement(DIV_ELEMENT);


    }

    @Override
    protected void renderContent(
       UIXRenderingContext context,
       UINode           node
       ) throws IOException
     {
       Boolean isDisclosed = (Boolean)context.getLocalProperty(0,
                                                               _DISCLOSED_KEY,
                                                               Boolean.TRUE );

       if ( Boolean.TRUE.equals( isDisclosed ) )
         super.renderContent(context, node);
     }

    /**
     * Copies an attribute from a source node to a destination UINode
     */
    // -= Simon Lessard =-
    // TODO: Never used locally as of 2006-08-09. Remove permanently
    //       if no problem show up.
    /*private void _copyAttr(
            UIXRenderingContext context,
            UINode           sourceNode,
            AttributeKey     attrKey,
            MutableUINode    destNode
            )
    {
        Object value = sourceNode.getAttributeValue(context, attrKey);

        if (value != null)
        {
            destNode.setAttributeValue(attrKey, value);
        }
    }*/

    private static final Object _DISCLOSED_KEY = new Object();
    
    private static final Map<String, String> _RESOURCE_KEY_MAP;
    static 
    {
      _RESOURCE_KEY_MAP = new HashMap<String, String>();
      
      _RESOURCE_KEY_MAP.put("af_showDetail.DISCLOSED" , 
                                "af_showDetailHeader.DISCLOSED");
      _RESOURCE_KEY_MAP.put("af_showDetail.UNDISCLOSED" , 
                                "af_showDetailHeader.UNDISCLOSED");
      _RESOURCE_KEY_MAP.put("af_showDetail.DISCLOSED_TIP" , 
                                "af_showDetailHeader.DISCLOSED_TIP");
      _RESOURCE_KEY_MAP.put("af_showDetail.UNDISCLOSED_TIP" , 
                                "af_showDetailHeader.UNDISCLOSED_TIP");
      _RESOURCE_KEY_MAP.put(XhtmlLafConstants.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME,
                                XhtmlLafConstants.AF_SHOW_DETAIL_HEADER_SD_DISCLOSED_ICON_NAME);
      _RESOURCE_KEY_MAP.put(XhtmlLafConstants.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME,
                                XhtmlLafConstants.AF_SHOW_DETAIL_HEADER_SD_UNDISCLOSED_ICON_NAME); 
    }

}
