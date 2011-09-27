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
package org.apache.myfaces.trinidad.model;

import java.awt.Color;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


/**
 * The data model used by chart components.
 * The chart is draw using values from the yValues 2D array. The yValues are returned by
 * {@link #getYValues} method. The maximum and the minimum are controlled by 
 * {@link #getMaxYValue} method and {@link #getMinYValue} method. The default maximum value
 * is 120% of the maximum of the yValues if no value is specified. The default minimum value 
 * is 0. For XYLine and Scatter plots xValues are also required. The xValues are controlled by
 * {@link #getXValues} method, {@link #getMaxXValue} method and {@link #getMinXValue} method.
 * 
 * The labels on y-axis of the graph are calculated from the yValues. However the labels on the
 * x-axis are controlled by the group labels array returned by  {@link #getGroupLabels} method.
 * 
 * Each group of values in the chart may contain multiple series. The number of series in a
 * group are controlled by {@link #getSeriesLabels} method. 
 * 
 * The colors of the series are controlled by {@link #getSeriesColors} method.
 * 
 * The chart title, sub-title and the footnote can be specified using 
 * {@link #getTitle} method, {@link #getSubTitle} method and {@link #getFootNote} method
 */
public abstract class ChartModel
{
  public abstract List<String> getSeriesLabels();
  
  public abstract List<String> getGroupLabels();
  
  public List<Color> getSeriesColors()
  {
    return _defaultColors;
  }
  
  public List<List<Double>> getXValues()
  {
    return null; 
  }

  public abstract List<List<Double>> getYValues();

  public Double getMaxYValue()
  {
    return null; 
  }


  public Double getMinYValue()
  {
    return null; 
  }


  public Double getMaxXValue()
  {
    return null; 
  }


  public Double getMinXValue()
  {
    return null; 
  }


  public String getTitle()
  {
    return null; 
  }

  public String getSubTitle()
  {
    return null; 
  }


  public String getFootNote()
  {
    return null; 
  }
  
  
  private static final List<Color> _defaultColors = new ArrayList<Color>();  
  static
  {
    _defaultColors.addAll(
      Arrays.asList(new Color[]{new Color(231,109,72,0),new Color(110,166,243,0),new Color(157,206,110,0),new Color(252,196,111,0),new Color(114,126,142,0),new Color(109,44,145,0)}));
  }  
}
