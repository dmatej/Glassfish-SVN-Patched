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
package org.apache.myfaces.trinidadinternal.renderkit.core.desktop;

import java.awt.Color;

import java.io.IOException;
import java.io.StringWriter;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.data.CoreChart;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.event.ChartDrillDownEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.ChartModel;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.LibraryScriptlet;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.Scriptlet;


/**
 * Renderer for Chart component
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/desktop/TreeTableRenderer.java#0 $) $Date: 10-nov-2005.19:03:37 $
 */
public class ChartRenderer extends XhtmlRenderer
{
  public ChartRenderer()
  {
    super(CoreChart.TYPE);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    chartLib = new LibraryScriptlet("ApacheChart", null);
    _typeKey = type.findKey("type");
    _templateSourceKey = type.findKey("templateSource");
    _perspectiveKey = type.findKey("perspective");
    _legendPositionKey = type.findKey("legendPosition");
    _animationDurationKey = type.findKey("animationDuration");
    _gradientsUsedKey = type.findKey("gradientsUsed");
    _tooltipsVisibleKey = type.findKey("tooltipsVisible");
    _YMajorGridLineCountKey = type.findKey("YMajorGridLineCount");
    _XMajorGridLineCountKey = type.findKey("XMajorGridLineCount");
    _YMinorGridLineCountKey = type.findKey("YMinorGridLineCount");
    _maxPrecisionKey = type.findKey("maxPrecision");
  }

  /**
   * @todo Decode the chart drill down event
   *
   */
  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    String       clientId)
  {
    Map<String, String> parameters =
      facesContext.getExternalContext().getRequestParameterMap();

    String source = parameters.get(XhtmlConstants.SOURCE_PARAM);
    String id = clientId == null ? component.getClientId(facesContext) : clientId;
    if (!id.equals(source))
      return;
    Object eventParam = parameters.get(XhtmlConstants.EVENT_PARAM);
    if (XhtmlConstants.CHART_DRILL_DOWN_EVENT.equals(eventParam))
    {
      int[] seriesIndices = null;
      int[] yValueIndices = null;
      double[] yValues = null;
      double[] xValues = null;
      String value = parameters.get(XhtmlConstants.VALUE_PARAM);
      String[] tokens = value.split(_DELIMITER);
      for (String token : tokens)
      {
        String[] subTokens = token.split("\t");
        if("seriesIndices".equals(subTokens[0]))
        {
          seriesIndices = _unmarshallEventInts(subTokens);
        }
        else if("yValueIndices".equals(subTokens[0]))
        {
          yValueIndices = _unmarshallEventInts(subTokens);
        }
        else if("yValues".equals(subTokens[0]))
        {
          yValues = _unmarshallEventDoubles(subTokens);
        }
        else if("xValues".equals(subTokens[0]))
        {
          xValues = _unmarshallEventDoubles(subTokens);
        }
      }
      ChartDrillDownEvent event =
        new ChartDrillDownEvent(component, seriesIndices,
                                yValueIndices, yValues, xValues);
      event.queue();
    }
  }

  private int[] _unmarshallEventInts(String[] tokens)
  {
    int[] indices = new int[tokens.length-1];
    for(int i=1; i<tokens.length; ++i)
      indices[i-1] = Integer.parseInt(tokens[i]);
    return indices;
  }

  private double[] _unmarshallEventDoubles(String[] tokens)
  {
    double[] values = new double[tokens.length-1];
    for(int i=1; i<tokens.length; ++i)
      values[i-1] = Double.parseDouble(tokens[i]);
    return values;
  }

   /**
    * @return
    */
   @Override
   public boolean getRendersChildren()
   {
     return true;
   }

   /**
    * Overrriden to always generate an id
    */
  @Override
  protected boolean shouldRenderId(
   FacesContext context,
   UIComponent component)
  {
    return true;
  }

  /**
   * render all pieces of the chart
   */
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (canSkipRendering(context, rc, component))
      return;

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement(XhtmlConstants.DIV_ELEMENT, component);
    renderId(context, component);
    renderStyleAttributes(context, rc, component, bean, SkinSelectors.AF_CHART_STYLE_CLASS);
    // We need the number convertor so that we can format numbers on the client
    XhtmlUtils.addLib(context, rc, _NUMBER_CONVERTER_SCRIPTLET);
    // output the chart javascript library
    chartLib.outputScriptlet(context, rc);

    // We will render the chart using JavaScript
    StringWriter sw = new StringWriter(5000);
    _outputSVGDocumentCreate(context, sw, component, bean);
    _outputJSChartModel(sw, component);
    _outputJSChartObject(context, rc, sw, component, bean);
    // Output the script to the response
    rw.startElement(XhtmlConstants.SCRIPT_ELEMENT, null);
    renderScriptDeferAttribute(context, rc);
    renderScriptTypeAttribute(context, rc);
    rw.write(sw.toString());
    rw.endElement(XhtmlConstants.SCRIPT_ELEMENT);
    rw.endElement(XhtmlConstants.DIV_ELEMENT);
  }

  protected void _outputSVGDocumentCreate(
    FacesContext context,
    StringWriter sw,
    UIComponent  component,
    FacesBean    bean
    ) throws IOException
  {
    String clientId = component.getClientId(context);
    sw.append("if (!document.getElementById('svgChart" + clientId + "')) {");
    sw.append("ApacheChart.createSVG(\"");
    sw.append(clientId);
    sw.append("\",\"svgChart");
    sw.append(clientId);
    sw.append("\",\"");
    String templateURL = getTemplateSource(component, bean);
    templateURL = context.getExternalContext().encodeResourceURL(templateURL);
    sw.append(templateURL);
    sw.append("\",\"width:100%; height:100%;\"");
    sw.append(",null);\n");
    sw.append("}");
  }

  protected void _outputJSChartModel(
    StringWriter sw,
    UIComponent  component
    ) throws IOException
  {
    CoreChart chart = (CoreChart)component;
    ChartModel model = (ChartModel)chart.getValue();
    if(model==null)
    {
      _LOG.severe("MODEL_NOT_SPECIFIED_FOR_CHART_COMPONENT");
      return;
    }
    sw.append("var seriesLabels = ");
    _writeJSObject(sw, model.getSeriesLabels());
    sw.append(";\n");
    sw.append("var groupLabels = ");
    _writeJSObject(sw, model.getGroupLabels());
    sw.append(";\n");
    sw.append("var seriesColors = ");
    _writeJSObject(sw, model.getSeriesColors());
    sw.append(";\n");
    sw.append("var xValues = ");
    _writeJSObject(sw, model.getXValues());
    sw.append(";\n");

    sw.append("var yValues = ");
    _writeJSObject(sw, model.getYValues());
    sw.append(";\n");
    sw.append("var model = new ApacheChartModel(seriesLabels, groupLabels, yValues, xValues, seriesColors);\n");

    sw.append("model.setMinYValue(");
    _writeJSObject(sw, model.getMinYValue());
    sw.append(");\n");
    sw.append("model.setMaxYValue(");
    _writeJSObject(sw, model.getMaxYValue());
    sw.append(");\n");
    sw.append("model.setMinXValue(");
    _writeJSObject(sw, model.getMinXValue());
    sw.append(");\n");
    sw.append("model.setMaxXValue(");
    _writeJSObject(sw, model.getMaxXValue());
    sw.append(");\n");
    sw.append("model.setTitle(");
    _writeJSObject(sw, model.getTitle());
    sw.append(");\n");
    sw.append("model.setSubTitle(");
    _writeJSObject(sw, model.getSubTitle());
    sw.append(");\n");
    sw.append("model.setFootNote(");
    _writeJSObject(sw, model.getFootNote());
    sw.append(");\n");
  }

  protected void _outputJSChartObject(
    FacesContext     context,
    RenderingContext rc,
    StringWriter     sw,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    Integer type = _typeToJSTypeMap.get(getType(bean));
    if(type == null)
      type = 1;
    sw.append("var type = ");
    _writeJSObject(sw, type);
    sw.append(";\n");

    String clientId = component.getClientId(context);
    sw.append("var chartId = ");
    _writeJSObject(sw, "svgChart"+clientId);
    sw.append(";\n");

    sw.append("var isPerspective = ");
    _writeJSObject(sw, isPerspective(component, bean));
    sw.append(";\n");

    sw.append("var legendPosition = ");
    _writeJSObject(sw, getLegendPosition(component, bean));
    sw.append(";\n");

    sw.append("var apacheChart = ApacheChart.createChart(type, model, chartId, isPerspective, legendPosition);");

    sw.append("apacheChart.setYMajorGridLineCount(");
    _writeJSObject(sw, getYMajorGridLineCount(component, bean));
    sw.append(");\n");

    sw.append("apacheChart.setYMinorGridLineCount(");
    _writeJSObject(sw, getYMinorGridLineCount(component, bean));
    sw.append(");\n");

    sw.append("apacheChart.setXMajorGridLineCount(");
    _writeJSObject(sw, getXMajorGridLineCount(component, bean));
    sw.append(");\n");

    sw.append("apacheChart.setGradientsUsed(");
    _writeJSObject(sw, isGradientsUsed(component, bean));
    sw.append(");\n");

    sw.append("apacheChart.setAnimationDuration(");
    _writeJSObject(sw, getAnimationDuration(component, bean));
    sw.append(");\n");

    sw.append("apacheChart.setTooltipsVisible(");
    _writeJSObject(sw, isTooltipsVisible(component, bean));
    sw.append(");\n");

    sw.append("apacheChart.setMaxPrecision(");
    _writeJSObject(sw, getMaxPrecision(component, bean));
    sw.append(");\n");

    String formName;
    FormData fData = rc.getFormData();
    if (fData == null)
      formName =  null;
    else
      formName = fData.getName();

    if(formName!=null)
    {
      sw.append("apacheChart.setFormName(");
      _writeJSObject(sw, formName);
      sw.append(");\n");
    }

    if(!PartialPageUtils.isPPRActive(context))
    {
      sw.append("apacheChart.setPartialSubmit(");
      _writeJSObject(sw, false);
      sw.append(");\n");
    }

    if(TrinidadAgent.AGENT_IE.equals(rc.getAgent().getAgentName()))
    {
      sw.append("apacheChart.setErrorHtml(");
      _writeJSObject(sw, rc.getTranslatedString("af_chart.IE_SVG_PLUGIN_ERROR_HTML"));
      sw.append(");\n");
    }
    else
    {
      sw.append("apacheChart.setErrorHtml(");
      _writeJSObject(sw, rc.getTranslatedString("af_chart.SVG_ENABLED_BROWSER_ERROR_HTML"));
      sw.append(");\n");
    }

    sw.append("apacheChart.setStatusHtml(");
    _writeJSObject(sw, rc.getTranslatedString("af_chart.SVG_LOADING_STATUS_HTML"));
    sw.append(");\n");

    // finally draw the chart
    sw.append("apacheChart.draw();\n");

    sw.append("if (typeof jQuery != 'undefined') { jQuery('body').data('" + clientId + "', apacheChart)}\n");
  }

  @SuppressWarnings("unchecked")
  static private void _writeJSObject(
    StringWriter sw,
    Object       attrValue
    ) throws IOException
  {
    if (attrValue == null)
    {
      sw.append("null");
    }
    else
    {
      if (attrValue instanceof String)
      {
        _writeJSString(sw, ((String)attrValue));
        return;
      }
      Class valueClass = attrValue.getClass();
      if (Boolean.class == valueClass)
      {
        _writeJSBoolean(sw, (Boolean)attrValue);
      }
      else if (Integer.class == valueClass)
      {
        _writeJSInt(sw, (Integer)attrValue);
      }
      else if (Double.class == valueClass)
      {
        _writeJSDouble(sw, (Double)attrValue);
      }
      else if (Color.class == valueClass)
      {
        _writeJSColor(sw, ((Color)attrValue));
      }
      else if (Collection.class.isAssignableFrom(valueClass))
      {
        _writeJSCollection(sw, ((Collection)attrValue));
      }
    }
  }

  /**
   * Encodes a String in JavaScript Object Notation.
   *
   * @param sw           the StringWriter
   * @param value         the String value
   */
  static private void _writeJSString(
    StringWriter sw,
    String       value
    ) throws IOException
  {
    if (value == null)
    {
      sw.append("null");
    }
    else
    {
      // escape chars as necessary
      sw.append('\'');

      for (int i=0; i < value.length(); i++)
      {
        char ch = value.charAt(i);
        _escapeChar(sw, ch);
      }

      sw.append('\'');
    }
  }

  /**
   * Encodes a char in JavaScript Object Notation.
   *
   * @param sw           the StringWriter
   * @param value         the char value
   */
  static private void _escapeChar(
    StringWriter sw,
    char         value)
  {
    switch (value)
    {
      // Escapes needed for XML
      case '&':
        sw.append('&');
        break;
      case '<':
        sw.append('<');
        break;
      case '>':
        sw.append('>');
        break;

      // Double quote
      case '\"':
        sw.append("\\\"");
        break;
      // Apostrophe
      case '\'':
        sw.append("\\\'");
        break;
      // Backslash
      case '\\':
        sw.append("\\\\");
        break;
      case '\b':
        sw.append("\\b");
        break;
      case '\f':
        sw.append("\\f");
        break;
      case '\n':
        sw.append("\\n");
        break;
      case '\r':
        sw.append("\\r");
        break;
      case '\t':
        sw.append("\\t");
        break;
      default:
        if (value >= 32 && value < 128)
        {
          // no escaping necessary
          sw.append(value);
        }
        else
        {
          String hex = Integer.toHexString(value);

          if (value > 0x00FF)
          {
            // use unicode escaping
            sw.append("\\u");
            if (value < 0x1000)
              sw.append('0');
            sw.append(hex);
          }
          else
          {
            // use hex escaping
            sw.append("\\x");
            if (value < 0x10)
              sw.append('0');
            sw.append(hex);
          }
        }
        break;
    }
  }

  /**
   * Encodes a int in JavaScript Object Notation.
   *
   * @param sw           the StringWriter
   * @param value         the Integer value
   */
  static public void _writeJSInt(
    StringWriter sw,
    Integer       value
    ) throws IOException
  {
    sw.append(String.valueOf(value));
  }

  /**
   * Encodes a boolean in JavaScript Object Notation.
   *
   * @param sw           the StringWriter
   * @param value        the Boolean value
   */
  static private void _writeJSBoolean(
    StringWriter sw,
    Boolean      value
    ) throws IOException
  {
    sw.append(String.valueOf(value));
  }


  /**
   * Encodes a float in JavaScript Object Notation.
   *
   * @param sw           the StringWriter
   * @param value         the float value
   */
  static private void _writeJSDouble(
    StringWriter sw,
    Double       value
    ) throws IOException
  {
    sw.append(String.valueOf(value));
  }

  /**
   * Encodes a Color in JavaScript Object Notation.
   *
   * @param sw           the StringWriter
   * @param color        the color value
   */
  static private void _writeJSColor(
    StringWriter sw,
    Color        color
    ) throws IOException
  {
    sw.append("\"RGB(");
    sw.append(String.valueOf(color.getRed()));
    sw.append(",");
    sw.append(String.valueOf(color.getGreen()));
    sw.append(",");
    sw.append(String.valueOf(color.getBlue()));
    sw.append(")\"");
  }

  /**
   * Encodes a Collection in JavaScript Object Notation.
   *
   * @param sw           the StringWriter
   * @param value        the List value
   */
  static private void _writeJSCollection(
    StringWriter  sw,
    Collection<?> value
    ) throws IOException
  {
    if (value == null)
    {
      sw.append("null");
    }
    else if (value.isEmpty())
    {
      sw.append("[]");
    }
    else
    {
      sw.append("[");
      for (Iterator<?> iter = value.iterator(); iter.hasNext();)
      {
        Object item = iter.next();
        _writeJSObject(sw, item);
        if (iter.hasNext())
        {
          sw.append(',');
        }
      }
      sw.append(']');
    }
  }

  private static Object _getProperty(
    FacesBean   bean,
    PropertyKey key)
  {
    Object ret = bean.getProperty(key);
    if (ret==null)
      ret = key.getDefault();
    return ret;
  }

  protected String getType(
    FacesBean bean)
  {
    return toString(_getProperty(bean, _typeKey));
  }

  protected String getTemplateSource(
    UIComponent component,
    FacesBean    bean)
  {
    Object ret = bean.getProperty(_templateSourceKey);
    String uri;
    if (ret==null)
    {
      if (isGradientsUsed(component, bean))
        uri = _TEMPLATE_DOC;
      else
        uri = _TEMPLATE_DOC_NOGRADIENT;
    }
    else
    {
      uri = toString(ret);
    }
    return toResourceUri(FacesContext.getCurrentInstance(), uri);
  }

  protected boolean isPerspective(
    UIComponent component,
    FacesBean   bean)
  {
    return Boolean.TRUE.equals(_getProperty(bean, _perspectiveKey));
  }

  protected String getLegendPosition(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(_getProperty(bean, _legendPositionKey));
  }

  protected Integer getAnimationDuration(
    UIComponent component,
    FacesBean   bean)
  {
    return (Integer)_getProperty(bean, _animationDurationKey);
  }

  protected boolean isGradientsUsed(
    UIComponent component,
    FacesBean   bean)
  {
    return Boolean.TRUE.equals(_getProperty(bean, _gradientsUsedKey));
  }

  protected boolean isTooltipsVisible(
    UIComponent component,
    FacesBean   bean)
  {
    return Boolean.TRUE.equals(_getProperty(bean, _tooltipsVisibleKey));
  }

  protected Integer getYMajorGridLineCount(
    UIComponent component,
    FacesBean   bean)
  {
    return (Integer)_getProperty(bean, _YMajorGridLineCountKey);
  }

  protected Integer getXMajorGridLineCount(
    UIComponent component,
    FacesBean   bean)
  {
    return (Integer)_getProperty(bean, _XMajorGridLineCountKey);
  }

  protected Integer getYMinorGridLineCount(
    UIComponent component,
    FacesBean   bean)
  {
    return (Integer)_getProperty(bean, _YMinorGridLineCountKey);
  }

  protected Integer getMaxPrecision(
    UIComponent component,
    FacesBean   bean)
  {
    return (Integer)_getProperty(bean, _maxPrecisionKey);
  }

  private Scriptlet chartLib;

  private PropertyKey _typeKey;
  private PropertyKey _templateSourceKey;
  private PropertyKey _perspectiveKey;
  private PropertyKey _legendPositionKey;
  private PropertyKey _animationDurationKey;
  private PropertyKey _gradientsUsedKey;
  private PropertyKey _tooltipsVisibleKey;
  private PropertyKey _YMajorGridLineCountKey;
  private PropertyKey _XMajorGridLineCountKey;
  private PropertyKey _YMinorGridLineCountKey;
  private PropertyKey _maxPrecisionKey;

  private static final String _DELIMITER = "\\$adf\\$";
  private static final String _TEMPLATE_DOC = "/adf/svg/chart.svg";
  private static final String _TEMPLATE_DOC_NOGRADIENT = "/adf/svg/chartNoGradient.svg";
  private static final String _NUMBER_CONVERTER_SCRIPTLET = "TrNumberConverter()";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ChartRenderer.class);

  private static final Map<String, Integer>_typeToJSTypeMap = new HashMap<String, Integer>();
  static
  {
    _typeToJSTypeMap.put("verticalBar", 1);
    _typeToJSTypeMap.put("horizontalBar", 2);
    _typeToJSTypeMap.put("stackedVerticalBar", 3);
    _typeToJSTypeMap.put("stackedHorizontalBar", 4);
    _typeToJSTypeMap.put("pie", 5);
    _typeToJSTypeMap.put("area", 6);
    _typeToJSTypeMap.put("stackedArea", 7);
    _typeToJSTypeMap.put("line", 8);
    _typeToJSTypeMap.put("barLine", 9);
    _typeToJSTypeMap.put("XYLine", 10);
    _typeToJSTypeMap.put("scatterPlot", 11);
    _typeToJSTypeMap.put("radar", 12);
    _typeToJSTypeMap.put("radarArea", 13);
    _typeToJSTypeMap.put("funnel", 14);
    _typeToJSTypeMap.put("circularGauge", 15);
    _typeToJSTypeMap.put("semiCircularGauge", 16);
  }
}
