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

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.component.UIXProgress;
import org.apache.myfaces.trinidad.model.BoundedRangeModel;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.skin.icon.NullIcon;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/ProcessingRenderer.java#0 $) $Date: 10-nov-2005.18:55:36 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ProcessingRenderer extends HtmlLafRenderer
{
  /**
   */
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderID(context, node);
    renderLayoutTableAttributes(context, "0", null);
    context.getResponseWriter().writeAttribute("align", "center", null);
  }
  /**
   * Render a table, with the first row being the processing icon
   * and the second row being the indexed children.
   */
  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("tr", null);
    writer.startElement("td", null);

    // place the process image above the contents
    // render the image in the first row
    writer.writeAttribute("align", "center", null);
    int percentComplete = _getPercentComplete(context, node);
    if (percentComplete != PERCENT_UNKNOWN)
    {
      // render the determinate image (i.e., the one where we know the
      // percent complete.)
      renderDeterminateStatus(context, percentComplete);
    }
    else
    {
      renderIndeterminateStatus(context, percentComplete);
    }
    writer.endElement("td");
    writer.endElement("tr");

    // render the processing element's content in the second row
    //=-=pu: Do we support indexed children for this component?
    writer.startElement("tr", null);
    writer.startElement("td", null);

    writer.writeAttribute("align", "center", null);

    super.renderContent(context, node);

    writer.endElement("td");
    writer.endElement("tr");
  }

  protected void renderDeterminateStatus(
    UIXRenderingContext context,
    int              percentComplete
    ) throws IOException
  {
    // render the determinate image (i.e., the one where we know the
    // percent complete.)
    String iconName = _getProcessingIconName(percentComplete);

    Icon determinateIcon = context.getIcon(iconName);
    String altText =  getDeterminateAltText(context,
                                            String.valueOf(percentComplete));
    // render the determinateIcon if we have one
    if ((determinateIcon != null) &&
        (!determinateIcon.equals(NullIcon.sharedInstance())))
    {
      RenderingContext arc = RenderingContext.getCurrentInstance();
      FacesContext fContext = context.getFacesContext();
      OutputUtils.renderIcon(fContext,
                             arc,
                             determinateIcon,
                             altText,
                             null);
    }
    else
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("div", null);
      renderStyleClassAttribute(context, PROCESS_STATUS_STYLE_CLASS);
      writer.writeText(_getDeterminateText(context,
                                           String.valueOf(percentComplete)),
                                           null);
      writer.endElement("div");
    }
  }

  protected void renderIndeterminateStatus(
    UIXRenderingContext context,
    int              percentComplete
    ) throws IOException
  {
    // Grab the indeterminate Icon
    Icon indeterminateIcon = context.getIcon(
                                AF_PROGRESS_INDICATOR_INDETERMINATE_ICON_NAME);
    String processingString =
      getTranslatedString(context, "af_progressIndicator.PROCESSING");
    // render the indeterminateIcon if we have one
    if ((indeterminateIcon != null) &&
        (!indeterminateIcon.equals(NullIcon.sharedInstance())))
    {
      RenderingContext arc = RenderingContext.getCurrentInstance();
      FacesContext fContext = context.getFacesContext();
      OutputUtils.renderIcon(fContext,
                             arc,
                             indeterminateIcon,
                             processingString,
                             null);
    }
    // otherwise render text in a div.
    else
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("div", null);
      renderStyleClassAttribute(context, PROCESS_STATUS_STYLE_CLASS);
      writer.writeText(processingString, null);
      writer.endElement("div");
    }

  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return "table";
  }

  /**
   * return a string with the alternate text for the determinate processing
   * icon. Given the percentComplete string, the alternate text would be,-
   * for example, "25
   */
  protected String getDeterminateAltText(
    UIXRenderingContext context,
    String           percentCompleteString
    )
  {
    if (!isScreenReaderMode(context))
      return null;

    return _getDeterminateText(context, percentCompleteString);
  }

  /**
  * get the percentComplete attribute value. Defaults to PERCENT_UNKNOWN.
  */
  private static int _getPercentComplete(
      UIXRenderingContext context,
      UINode           node
    )
  {
    int percentComplete = -1;

    UIXProgress progressComponent =
      (UIXProgress) NodeUtils.getUIComponent(context, node);
    Object modelObject = progressComponent.getValue();
    if (modelObject != null && modelObject instanceof BoundedRangeModel)
    {
      BoundedRangeModel model= (BoundedRangeModel) modelObject;
      //pu: Though these are 'long' types, deal with it as double, so as to
      //  correctly compute percent for values towards the maximum of 'long'.
      double value = model.getValue();
      double maximum = model.getMaximum();
      if (value < 0 || maximum < 0)
      {
        percentComplete = PERCENT_UNKNOWN;
      }
      else
      {
        //pu: Loss due to truncation is not a concern here.
        percentComplete = (int)((value/maximum) * 100);
      }
    }
    else
    {
      _LOG.warning("DEFAULT_COMPONENT_TO_INDETERMINDATE_MODE", progressComponent.getId());
      //Just get this to indeterminate state indefinitely on this condition.
      percentComplete = PERCENT_UNKNOWN;
    }

    if (percentComplete < 0)
      return PERCENT_UNKNOWN;

    // return the smaller of the two values
    percentComplete = Math.min(percentComplete, _PERCENT_COMPLETE_MAX);

    // make sure it is a multiple of _PERCENT_MULTIPLE.
    return  (percentComplete - (percentComplete % _PERCENT_MULTIPLE));

  }



  /**
   * return a string with the alternate text for the determinate processing
   * icon. Given the percentComplete string, the alternate text would be,-
   * for example, "25
   */
  private String _getDeterminateText(
    UIXRenderingContext context,
    String           percentCompleteString
    )
  {

    String[] parameters = new String[]
    {
      percentCompleteString
    };

    String text =
      formatString(context,
                   getTranslatedString(context,
                                       "af_progressIndicator.DETERMINATE_TIP"),
                   parameters);
    return text;
  }

  /**
   * given the percentComplete, return the IconKey with the correct
   * processing image.
   */
  private static String _getProcessingIconName(
    int percentComplete
    )
  {
    if (percentComplete < 5)
     return AF_PROGRESS_INDICATOR_ZERO_ICON_NAME;
    else if (percentComplete < 10)
      return AF_PROGRESS_INDICATOR_FIVE_ICON_NAME;
    else if (percentComplete < 15)
      return AF_PROGRESS_INDICATOR_TEN_ICON_NAME;
    else if (percentComplete < 20)
      return AF_PROGRESS_INDICATOR_FIFTEEN_ICON_NAME;
    else if (percentComplete < 25)
      return AF_PROGRESS_INDICATOR_TWENTY_ICON_NAME;
    else if (percentComplete < 30)
      return AF_PROGRESS_INDICATOR_TWENTY_FIVE_ICON_NAME;
    else if (percentComplete < 35)
      return AF_PROGRESS_INDICATOR_THIRTY_ICON_NAME;
    else if (percentComplete < 40)
      return AF_PROGRESS_INDICATOR_THIRTY_FIVE_ICON_NAME;
    else if (percentComplete < 45)
      return AF_PROGRESS_INDICATOR_FORTY_ICON_NAME;
    else if (percentComplete < 50)
      return AF_PROGRESS_INDICATOR_FORTY_FIVE_ICON_NAME;
    else if (percentComplete < 55)
      return AF_PROGRESS_INDICATOR_FIFTY_ICON_NAME;
    else if (percentComplete < 60)
      return AF_PROGRESS_INDICATOR_FIFTY_FIVE_ICON_NAME;
    else if (percentComplete < 65)
      return AF_PROGRESS_INDICATOR_SIXTY_ICON_NAME;
    else if (percentComplete < 70)
      return AF_PROGRESS_INDICATOR_SIXTY_FIVE_ICON_NAME;
    else if (percentComplete < 75)
      return AF_PROGRESS_INDICATOR_SEVENTY_ICON_NAME;
    else if (percentComplete < 80)
      return AF_PROGRESS_INDICATOR_SEVENTY_FIVE_ICON_NAME;
    else if (percentComplete < 85)
      return AF_PROGRESS_INDICATOR_EIGHTY_ICON_NAME;
    else if (percentComplete < 90)
      return AF_PROGRESS_INDICATOR_EIGHTY_FIVE_ICON_NAME;
    else if (percentComplete < 95)
      return AF_PROGRESS_INDICATOR_NINETY_ICON_NAME;
    else if (percentComplete < 100)
      return AF_PROGRESS_INDICATOR_NINETY_FIVE_ICON_NAME;
    else return AF_PROGRESS_INDICATOR_ONE_HUNDRED_ICON_NAME;
  }

  private static final int    _PERCENT_COMPLETE_MAX = 100;
  private static final int    _PERCENT_MULTIPLE = 5;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ProcessingRenderer.class);
}


