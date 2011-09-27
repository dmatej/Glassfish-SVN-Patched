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


import java.io.IOException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXProcess;
import org.apache.myfaces.trinidad.component.core.nav.CoreTrain;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.ProcessUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinProperties;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;


/**
 * Renderer for process train components
 *
 */
public class TrainRenderer
  extends XhtmlRenderer
{
  /**
   * Constructor.
   */
  public TrainRenderer()
  {
    super(CoreTrain.TYPE);
  }

  /**
   */
  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    @SuppressWarnings("unused")
    String       clientId)
  {
    Map<String, String> requestMap =
      facesContext.getExternalContext().getRequestParameterMap();

    Object event = requestMap.get(XhtmlConstants.EVENT_PARAM);

    if ((event != null) && event.equals(XhtmlConstants.GOTO_EVENT))
    {
      Object source = requestMap.get(XhtmlConstants.SOURCE_PARAM);

      if (source != null && source.equals(component.getClientId(facesContext)))
      {

        Object valueObject = requestMap.get(XhtmlConstants.VALUE_PARAM);

        // we piggyback on the size parameter.
        // 0 means we are moving to a previous step, 1 means we are
        // moving to the next step.
        Object sizeObject = requestMap.get(XhtmlConstants.SIZE_PARAM);

        if (valueObject != null)
        {
          int value = -1;

          try
          {
            value = Integer.parseInt(valueObject.toString());
          }
          catch (NumberFormatException nfe)
          {
            _LOG.severe(nfe);
          }

          int size = 0;

          try
          {
            size = Integer.parseInt(sizeObject.toString());
          }
          catch (NumberFormatException nfe)
          {
            _LOG.warning(nfe);
          }

          if (size < 0)
            size = 0;

          if (value >= 0)
          {
            UIXProcess process = (UIXProcess) component;
            Object oldPath = process.getRowKey();
            Object focusPath = process.getFocusRowKey();
            process.setRowKey(focusPath);
            UIComponent stamp = process.getNodeStamp();
            int index = process.getRowIndex();

            if (size == 0)
            {
              index = ProcessUtils.getBackIndex(process, stamp, index);
            }
            else
            {
              index = ProcessUtils.getNextIndex(process, stamp, index);
            }

            process.setRowIndex(index);
            new ActionEvent(stamp).queue();
            process.setRowKey(oldPath);
          }
        }
      }
    }
  }

  /**
   * @return
   */
  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
    throws IOException
  {
    // Since Train is a naming container, we can be more
    // efficient about skipping its children
    if (!PartialPageUtils.containsPprTargets(rc,
                                             component,
                                             getClientId(context, component)))
    {
      return;
    }

    if(!(component instanceof UIXProcess))
    {
      throw new ClassCastException(_LOG.getMessage(
        "TRAINRENDERER_ONLY_RENDERS_INSTANCE", new Object[]{UIXProcess.class.getName(), component.getClass().getName()}));
    }

    if (rc.getFormData() == null)
    {
      _LOG.warning("TRAIN_MUST_INSIDE_FORM");
      return;
    }

    UIXProcess process = (UIXProcess) component;
    UIComponent stamp = process.getNodeStamp();

    if (stamp != null)
    {
      Train train = new Train(context, rc, process, stamp);
      try
      {
        process.setRowKey(train.getFocusRowKey());

        // Renders some fields and scripts
        _renderHiddenFields(context, rc, train);

        ResponseWriter writer = context.getResponseWriter();

        // Need to render the frame even if there's no visible station
        // to support PPR.
        writer.startElement(XhtmlConstants.TABLE_ELEMENT, component);
        process.setRowKey(train.getInitialRowKey());
        renderId(context, component);
        renderAllAttributes(context, rc, component, bean);
        // Does not seem to be needed and this is not XHTML 1.0 Strict compliant
        // writer.writeAttribute("align", "center", null);

        if(!train.getStations().isEmpty())
        {
          process.setRowKey(train.getFocusRowKey());

          // There're visible stations currently, let render them.
          writer.startElement(XhtmlConstants.TABLE_BODY_ELEMENT, null);
          _renderTrain(context, rc, process, bean, stamp, train);
          writer.endElement(XhtmlConstants.TABLE_BODY_ELEMENT);
        }

        writer.endElement(XhtmlConstants.TABLE_ELEMENT);
      }
      finally
      {
        // Always restore the model, whatever happened
        process.setRowKey(train.getInitialRowKey());
      }
    }
    else
    {
      _LOG.warning("NODESTAMP_FACET_NOT_FOUND_FOR_TRAIN", component);
    }
    /*
      _encodeChildren(context, arc, process, stamp, trainState, length);
    */
  }

  @Override
  protected void renderAllAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    super.renderAllAttributes(context, rc, component, bean);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
  }

  /**
   * This is how we can render both the user defined styleClass and our
   * component style class
   */
  @Override
  protected void renderStyleAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
    throws IOException
  {
    renderStyleAttributes(
      context,
      rc,
      component,
      bean,
      SkinSelectors.AF_TRAIN_ROOT_STYLE_CLASS);
  }

  private void _preRenderIconBlock(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Icon cell
    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);

    // Icons need to be in a table to stretch well
    writer.startElement(XhtmlConstants.TABLE_ELEMENT, null);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
    writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, "width: 100%", null);

    writer.startElement(XhtmlConstants.TABLE_BODY_ELEMENT, null);
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
  }

  private void _postRenderIconBlock(
    FacesContext context
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
    writer.endElement(XhtmlConstants.TABLE_BODY_ELEMENT);
    writer.endElement(XhtmlConstants.TABLE_ELEMENT);
    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }

  private void _renderHiddenFields(
    FacesContext     context,
    RenderingContext rc,
    Train            train)
    throws IOException
  {
    if((train.getFormName() != null) && supportsScripting(rc))
    {
      // render hidden fields to hold the form data
      FormData formData = rc.getFormData();
      if (formData != null)
      {
        formData.addNeededValue(XhtmlConstants.EVENT_PARAM);
        formData.addNeededValue(XhtmlConstants.SOURCE_PARAM);
        formData.addNeededValue(XhtmlConstants.VALUE_PARAM);
        formData.addNeededValue(XhtmlConstants.SIZE_PARAM);
      }

      // Render script submission code.
      ProcessUtils.renderNavSubmitScript(context, rc);
    }
  }

  private void _renderContentRowLtr(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    UIComponent      stamp,
    Train            train
    ) throws IOException
  {
    ParentTrain parentTrain = train.getParentTrain();

    // Render parent start
    if(parentTrain != null && parentTrain.hasParentStart())
    {
      _renderParentContent(context, rc, parentTrain.getParentStart());
    }

    for(Station station : train.getStations())
    {
      _renderStationContent(context, rc, process, stamp, station);
    }

    // Render parent end
    if(parentTrain != null && parentTrain.hasParentEnd())
    {
      _renderParentContent(context, rc, parentTrain.getParentEnd());
    }
  }

  private void _renderContentRowRtl(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    UIComponent      stamp,
    Train            train
    ) throws IOException
  {
    ParentTrain parentTrain = train.getParentTrain();

    // Render parent start
    if(parentTrain != null && parentTrain.hasParentEnd())
    {
      _renderParentContent(context, rc, parentTrain.getParentEnd());
    }

    List<Station>         stations = train.getStations();
    ListIterator<Station> iterator = stations.listIterator(stations.size());
    while(iterator.hasPrevious())
    {
      _renderStationContent(context, rc, process, stamp, iterator.previous());
    }

    // Render parent end
    if(parentTrain != null && parentTrain.hasParentStart())
    {
      _renderParentContent(context, rc, parentTrain.getParentStart());
    }
  }

  private void _renderIconBlock(
    FacesContext     context,
    RenderingContext rc,
    List<String>     iconNames,
    String           shortDesc,
    String           styleClass,
    String           iconStyleClass,
    List<String>     stateStyleClasses
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);

    stateStyleClasses.add(styleClass);
    stateStyleClasses.add(iconStyleClass);

    renderStyleClasses(context,
                       rc,
                       stateStyleClasses.toArray(_EMPTY_STRING_ARRAY));

    if(iconNames != null)
    {
      // Render the first valid icon found. The list should be in
      // decreasing priority order.
      for(String iconName : iconNames)
      {
        Icon icon = rc.getIcon(iconName);
        if(icon != null)
        {
          OutputUtils.renderIcon(context, rc, icon, shortDesc, null);
          break;
        }
      }
    }

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }

  private void _renderIconRowLtr(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train
    ) throws IOException
  {
    ParentTrain parentTrain = train.getParentTrain();

    // Render parent start
    if(parentTrain != null && parentTrain.hasParentStart())
    {
      _renderParentStartLtr(context, rc, process, train);
    }

    for(Station station : train.getStations())
    {
      _renderStationIconLtr(context, rc, process, station);
    }

    // Render parent end
    if(parentTrain != null && parentTrain.hasParentEnd())
    {
      _renderParentEndLtr(context, rc, process, train);
    }
  }

  private void _renderIconRowRtl(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train
    ) throws IOException
  {
    ParentTrain parentTrain = train.getParentTrain();

    // Render parent end
    if(parentTrain != null && parentTrain.hasParentEnd())
    {
      _renderParentEndRtl(context, rc, process, train);
    }

    List<Station>         stations = train.getStations();
    ListIterator<Station> iterator = stations.listIterator(stations.size());
    while(iterator.hasPrevious())
    {
      _renderStationIconRtl(context, rc, process, iterator.previous());
    }

    // Render parent start
    if(parentTrain != null && parentTrain.hasParentStart())
    {
      _renderParentStartRtl(context, rc, process, train);
    }

  }

  private void _renderJoin(
    FacesContext     context,
    RenderingContext rc,
    String           stateStyleClass,
    boolean          overflow
    ) throws IOException
  {
    if (_STATE_PARENT.equals(stateStyleClass))
    {
      _renderJoin(context,
                  rc,
                  SkinSelectors.AF_TRAIN_PARENT_JOIN_STYLE_CLASS,
                  null);
    }
    else if (overflow)
    {
      _renderJoin(context,
                  rc,
                  SkinSelectors.AF_TRAIN_OVERFLOW_JOIN_STYLE_CLASS,
                  stateStyleClass);
    }
    else
    {
      _renderJoin(context,
                  rc,
                  SkinSelectors.AF_TRAIN_JOIN_STYLE_CLASS,
                  stateStyleClass);
    }
  }

  private void _renderJoin(
    FacesContext     context,
    RenderingContext rc,
    String           joinStyleClass,
    String           stateStyleClass
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    renderStyleClasses(context,
                       rc,
                       new String[]{
                         joinStyleClass,
                         stateStyleClass});

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }

  private void _renderJoinIconBlock(
    FacesContext     context,
    RenderingContext rc,
    String           stateStyleClass,
    boolean          overflow
    ) throws IOException
  {
    if (_STATE_PARENT.equals(stateStyleClass))
    {
      _renderJoinIconBlock(context,
                           rc,
                           SkinSelectors.AF_TRAIN_PARENT_JOIN_STYLE_CLASS,
                           null);
    }
    else if(overflow)
    {
      _renderJoinIconBlock(context,
                           rc,
                           SkinSelectors.AF_TRAIN_OVERFLOW_JOIN_STYLE_CLASS,
                           stateStyleClass);
    }
    else
    {
      _renderJoinIconBlock(context,
                           rc,
                           SkinSelectors.AF_TRAIN_JOIN_STYLE_CLASS,
                           stateStyleClass);
    }
  }

  private void _renderJoinIconBlock(
    FacesContext     context,
    RenderingContext rc,
    String           joinStyleClass,
    String           stateStyleClass
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, "width: 50%", null);
    renderStyleClasses(context,
                       rc,
                       new String[]{
                         joinStyleClass,
                         stateStyleClass});
    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }

  private void _renderParentContent(
    FacesContext     context,
    RenderingContext rc,
    Station          parent) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    String  baseStyleClass = parent.getBaseStyleClass();
    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.COLSPAN_ATTRIBUTE, "3", null);
    renderStyleClasses(context,
                       rc,
                       new String[]{
                         baseStyleClass,
                         baseStyleClass + _SUFFIX_CONTENT});

    /* -= Simon =-
     * FIXME HACK for MSIE CSS bug involving composite style classes.
     *       Since the bug is most obvious with join background images
     *       I hard code background-image to none to fix it.
     *       See Jira for issue ADFFACES-206.
     */
    if(rc.getAgent().getAgentName().equalsIgnoreCase(Agent.AGENT_IE))
    {
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
                            "background-image:none;",
                            null);
    }

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }

  private void _renderParentEnd(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train,
    String           leftState,
    String           rightState
    ) throws IOException
  {
    // Add join
    _renderJoin(context, rc, leftState, false);

    // Icon cell
    _preRenderIconBlock(context, rc);

    // Add join
    _renderJoinIconBlock(context, rc, leftState, false);

    // Add the parent's stop icon
    _renderParentEndIconBlock(context, rc, process, train);

    // Add join
    _renderJoinIconBlock(context, rc, rightState, false);

    // End icon cell
    _postRenderIconBlock(context);

    // Add join
    _renderJoin(context, rc, rightState, false);
  }

  private void _renderParentEndLtr(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train
    ) throws IOException
  {
    _renderParentEnd(context,
                     rc,
                     process,
                     train,
                     _STATE_PARENT,
                     null);
  }

  private void _renderParentEndRtl(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train
    ) throws IOException
  {
    _renderParentEnd(context,
                     rc,
                     process,
                     train,
                     null,
                     _STATE_PARENT);
  }

  private void _renderParentEndIconBlock(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train
    ) throws IOException
  {
    assert train.getParentTrain().hasParentEnd();

    Station parent = train.getParentTrain().getParentEnd();

    process.setRowKey(parent.getRowKey());

    _renderStationIconBlock(context, rc, process, parent);

    // Restore model
    process.setRowKey(train.getInitialRowKey());
  }

  private void _renderParentStartIconBlock(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train
    ) throws IOException
  {
    assert train.getParentTrain().hasParentStart();

    Station parent = train.getParentTrain().getParentStart();

    process.setRowKey(parent.getRowKey());

    _renderStationIconBlock(context, rc, process, parent);

    // Restore model
    process.setRowKey(train.getInitialRowKey());
  }

  private void _renderParentStart(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train,
    String           leftState,
    String           rightState
    ) throws IOException
  {
    // Add join
    _renderJoin(context, rc, leftState, false);

    // Icon cell
    _preRenderIconBlock(context, rc);

    // Add join
    _renderJoinIconBlock(context, rc, leftState, false);

    // Add the parent's stop icon
    _renderParentStartIconBlock(context, rc, process, train);

    // Add join
    _renderJoinIconBlock(context, rc, rightState, false);

    _postRenderIconBlock(context);
    // End icon cell

    // Add join
    _renderJoin(context, rc, rightState, false);
  }

  private void _renderParentStartLtr(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train
    ) throws IOException
  {
    _renderParentStart(context,
                       rc,
                       process,
                       train,
                       null,
                       _STATE_PARENT);
  }

  private void _renderParentStartRtl(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Train            train
    ) throws IOException
  {
    _renderParentStart(context,
                       rc,
                       process,
                       train,
                       _STATE_PARENT,
                       null);
  }

  private void _renderStationContent(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    UIComponent      stamp,
    Station          station
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);

    writer.writeAttribute(XhtmlConstants.COLSPAN_ATTRIBUTE, "3", null);

    String baseStyleClass = station.getBaseStyleClass();

    List<String> stateStyleClasses = station.getStates();
    stateStyleClasses.add(baseStyleClass);
    stateStyleClasses.add(baseStyleClass + _SUFFIX_CONTENT);

    renderStyleClasses(context,
                       rc,
                       stateStyleClasses.toArray(_EMPTY_STRING_ARRAY));

    /* -= Simon =-
     * FIXME HACK for MSIE CSS bug involving composite style classes.
     *       Since the bug is most obvious with join background images
     *       I hard code background-image to none to fix it.
     *       See Jira for issue ADFFACES-206.
     */
    if(rc.getAgent().getAgentName().equalsIgnoreCase(Agent.AGENT_IE))
    {
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
                            "background-image:none;",
                            null);
    }

    Map<String, String> originalMap = rc.getSkinResourceKeyMap();

    // Init the model
    process.setRowIndex(station.getRowIndex());
    try
    {
      rc.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
      encodeChild(context, stamp);
    }
    finally
    {
      rc.setSkinResourceKeyMap(originalMap);
    }

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }

  private void _renderStationIcon(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Station          station,
    String           leftJoinState,
    String           rightJoinState,
    boolean          overflowLeft,
    boolean          overflowRight
    ) throws IOException
  {
    // Add join
    _renderJoin(context, rc, leftJoinState, overflowLeft);

    // Icon cell
    _preRenderIconBlock(context, rc);

    // Add join
    _renderJoinIconBlock(context, rc, leftJoinState, overflowLeft);

    // Add the parent's stop icon
    _renderStationIconBlock(context, rc, process, station);

    // Add join
    _renderJoinIconBlock(context, rc, rightJoinState, overflowRight);

    // End icon cell
    _postRenderIconBlock(context);

    // Add join
    _renderJoin(context, rc, rightJoinState, overflowRight);
  }

  private void _renderStationIconBlock(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Station          station
    ) throws IOException
  {
    process.setRowIndex(station.getRowIndex());

    String baseStyleClass = station.getBaseStyleClass();

    _renderIconBlock(context,
                     rc,
                     station.getIconNames(),
                     station.getLabel(),
                     baseStyleClass,
                     baseStyleClass + _SUFFIX_ICON_CELL,
                     station.getStates());
  }

  private void _renderStationIconLtr(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Station          station
    ) throws IOException
  {
    _renderStationIcon(context,
                       rc,
                       process,
                       station,
                       station.getStartJoinState(),
                       station.getEndJoinState(),
                       station.hasPrevious() && station.getPrevious().isOverflowStart(),
                       station.hasNext()     && station.getNext().isOverflowEnd());
  }

  private void _renderStationIconRtl(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    Station          station
    ) throws IOException
  {
    _renderStationIcon(context,
                       rc,
                       process,
                       station,
                       station.getEndJoinState(),
                       station.getStartJoinState(),
                       station.hasNext()     && station.getNext().isOverflowEnd(),
                       station.hasPrevious() && station.getPrevious().isOverflowStart());
  }

  private void _renderTrain(
    FacesContext     context,
    RenderingContext rc,
    UIXProcess       process,
    FacesBean        bean,
    UIComponent      stamp,
    Train            train
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Start of the icon row
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);

    if(rc.isRightToLeft())
    {
      _renderIconRowRtl(context, rc, process, train);
    }
    else
    {
      _renderIconRowLtr(context, rc, process, train);
    }

    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);

    // Start of the content row
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);

    if(rc.isRightToLeft())
    {
      _renderContentRowRtl(context, rc, process, stamp, train);
    }
    else
    {
      _renderContentRowLtr(context, rc, process, stamp, train);
    }

    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
  }

  private static class Train
  {
    public Train(
        FacesContext     context,
        RenderingContext arc,
        UIXProcess       process,
        UIComponent      stamp)
    {
      // Save the model state
      int activeIndex      = _loadStations(process, stamp);
      int visibleStopCount = _getVisibleStopCount(arc);

      _formName     = arc.getFormData().getName();
      _isSubTrain   = _loadIsSubTrain(process);

      if(!_stations.isEmpty())
      {
        // There's something visible in the train
        if(_stations.size() > visibleStopCount)
        {
          // We have overflow, let resolve it
          _resolveOverflow(visibleStopCount, activeIndex);
        }
        else
        {
          // No overflow, yay!
          _resolveStandard();
        }

        _initLabels(arc, process, stamp);
        _initParentTrain(arc, process, stamp);
      }
    }

    public Object getFocusRowKey()
    {
      return _focusRowKey;
    }

    public String getFormName()
    {
      return _formName;
    }

    public Object getInitialRowKey()
    {
      return _initialRowKey;
    }

    public ParentTrain getParentTrain()
    {
      return _parent;
    }

    public List<Station> getStations()
    {
      return _stations;
    }

    public boolean isSubTrain()
    {
      return _isSubTrain;
    }

    private void _createStation(
        UIXProcess  process,
        UIComponent stamp,
        int         index,
        boolean     active)
    {
      process.setRowIndex(index);
      if(stamp.isRendered())
      {
        // The station will be visible.
        _stations.add(new Station(this,
                                  stamp,
                                  index,
                                  process.getRowKey(),
                                  active));
      }
    }

    private int _getVisibleStopCount(RenderingContext arc)
    {
      Object propValue =
        arc.getSkin().getProperty(SkinProperties.AF_TRAIN_VISIBLE_STOP_COUNT);

      if(propValue == null)
      {
        return DEFAULT_MAX_VISIBLE_STOP_COUNT;
      }

      try
      {
        int count = Integer.parseInt(propValue.toString());
        if(count <= 0)
        {
          _LOG.warning("VISIBLE_STOP_COUNT_MUST_ABOVE_ZERO", count);
          return DEFAULT_MAX_VISIBLE_STOP_COUNT;
        }

        return count;
      }
      catch(NumberFormatException e)
      {
        _LOG.warning("VISIBLE_STOP_COUNT_MYST_INTEGER", propValue);
        return DEFAULT_MAX_VISIBLE_STOP_COUNT;
      }
    }

    private void _initLabels(
        RenderingContext arc,
        UIXProcess       process,
        UIComponent      stamp)
    {
      for(Station s : _stations)
      {
        process.setRowIndex(s.getRowIndex());
        s.initLabel(arc, stamp);
      }
    }

    private void _initParentTrain(
        RenderingContext arc,
        UIXProcess       process,
        UIComponent      stamp)
    {
      if(_isSubTrain)
      {
        if(_shouldRenderParentTrain(arc))
        {
          _parent = new ParentTrain(arc, process, stamp, this);
          if(!_parent.hasParentStart() && !_parent.hasParentEnd())
          {
            _isSubTrain = false;
          }
        }
        else
        {
          _isSubTrain = false;
        }
      }
    }

    /**
     * Determine if this train is a sub-train.
     */
    private boolean _loadIsSubTrain(UIXProcess process)
    {
      Object focusRowKey = process.getFocusRowKey();
      if (focusRowKey != null && (process.getDepth(focusRowKey) > 0))
      {
        return true;
      }

      return false;
    }

    private int _loadStations(
        UIXProcess  process,
        UIComponent stamp)
    {
      _initialRowKey = process.getRowKey();
      try
      {
        // Set the model on the focus item
        _focusRowKey = process.getFocusRowKey();
        process.setRowKey(_focusRowKey);

        int count       = process.getRowCount();
        int activeIndex = process.getRowIndex();
        int index       = 0;

        assert activeIndex < count;

        _stations = new ArrayList<Station>(count);
        boolean bActiveStop = false;

        // Process visited stations
        for(; index < count; index++)
        {
          bActiveStop = (index == activeIndex);
          _createStation(process, stamp, index, bActiveStop);
          if (bActiveStop)
          {
            // Might have an invisible active station. Thsi is weird, but still.
            // You never know what users want to do, but let support
            // it nevertheless for now.
            // selectedIndex is either the active station index or the index
            // of the station just before the selected one if active is not visible.
            activeIndex = _stations.size() - 1;
          }
        }
        return activeIndex;
      }
      finally
      {
        // Restore the model's state
        process.setRowKey(_initialRowKey);
      }
    }

    private void _resolveOverflow(
        int visibleStopCount,
        int activeIndex)
    {
      assert _stations != null;
      assert activeIndex >= -1;
      assert activeIndex < _stations.size();

      // First, resolve chaining
      _resolveStandard();

      // We have more stations than the max available, so we have an overflow
      // for sure.
      if(activeIndex <= 0)
      {
        // Overflow to the following group only
        _resolveOverflowEnd(visibleStopCount);
        _stations = _stations.subList(0, visibleStopCount + 1);
      }
      else
      {
        // Get the visible group index
        int groupIndex = activeIndex / visibleStopCount;
        int startIndex = 0;
        int endIndex   = _stations.size();
        if(groupIndex > 0)
        {
          // We have a previous overflow
          startIndex = groupIndex * visibleStopCount - 1;
          _resolveOverflowStart(startIndex);
        }

        int maxGroupIndex = (_stations.size() - 1) / visibleStopCount;

        if(groupIndex < maxGroupIndex)
        {
          // We have a next overflow
          int overflowIndex = (groupIndex + 1) * visibleStopCount;

          // endIndex is exclusive
          endIndex = overflowIndex + 1;

          _resolveOverflowEnd(overflowIndex);
        }

        _stations = _stations.subList(startIndex, endIndex);
      }
    }

    private void _resolveOverflowEnd(int index)
    {
      assert _stations != null;
      assert index >= 0;
      assert index < _stations.size();

      Station station = _stations.get(index);
      station.setOverflowEnd(true);
      if(station.hasPrevious() && station.getPrevious().isDisabled())
      {
        // If previous stop is disabled, so is the overflow
        station.setDisabled(true);
      }

      station.setNext(null);
    }

    private void _resolveOverflowStart(int index)
    {
      assert _stations != null;
      assert index >= 0;
      assert index < _stations.size();

      Station station = _stations.get(index);
      station.setOverflowStart(true);
      if(station.hasNext() && station.getNext().isDisabled())
      {
        // If next stop is disabled, so is the overflow
        station.setDisabled(true);
      }

      station.setPrevious(null);
    }

    private void _resolveStandard()
    {
      if(_stations.size() > 1)
      {
        Iterator<Station> iterator = _stations.iterator();

        Station previous = null;
        Station current  = iterator.next();
        Station next     = iterator.next();

        _updateStation(current, previous, next);

        while(iterator.hasNext())
        {
          previous = current;
          current  = next;
          next     = iterator.next();
          _updateStation(current, previous, next);
        }

        next.setPrevious(current);
      }
    }

    private boolean _shouldRenderParentTrain(RenderingContext arc)
    {
      Object propValue =
        arc.getSkin().getProperty(SkinProperties.AF_TRAIN_RENDER_PARENT_TRAIN);

      if(propValue == null)
      {
        return DEFAULT_RENDER_PARENT_TRAIN;
      }

      return Boolean.TRUE.equals(propValue);
    }

    private void _updateStation(
        Station current,
        Station previous,
        Station next)
    {
      current.setPrevious(previous);
      current.setNext(next);
    }

    private Object        _focusRowKey;
    private String        _formName;
    private Object        _initialRowKey;
    private boolean       _isSubTrain;
    private ParentTrain   _parent;
    private List<Station> _stations;
  }

  private static class Station
  {
    public Station(
        Train  train,
        int    index,
        Object rowKey)
    {
      _rowIndex    = index;
      _rowKey      = rowKey;
      _active      = false;
      _visited     = false;
      _disabled    = false;
      _parentEnd   = false;
      _parentStart = false;
      _train       = train;
    }

    @SuppressWarnings("unchecked")
    public Station(
        Train       train,
        UIComponent stamp,
        int         index,
        Object      rowKey,
        boolean     active)
    {
      Map<String, Object> attributes = stamp.getAttributes();

      _rowIndex    = index;
      _rowKey      = rowKey;
      _active      = active;
      _visited     = _getBooleanAttribute(attributes, "visited", false);
      _disabled    = _getBooleanAttribute(attributes, "disabled", false);
      _parentEnd   = false;
      _parentStart = false;
      _train       = train;
    }

    public String getBaseStyleClass()
    {
      if(isOverflowEnd())
      {
        return SkinSelectors.AF_TRAIN_OVERFLOW_END_STYLE_CLASS;
      }
      else if(isOverflowStart())
      {
        return SkinSelectors.AF_TRAIN_OVERFLOW_START_STYLE_CLASS;
      }
      else if(isParentStart())
      {
        return SkinSelectors.AF_TRAIN_PARENT_START_STYLE_CLASS;
      }
      else if(isParentEnd())
      {
        return SkinSelectors.AF_TRAIN_PARENT_END_STYLE_CLASS;
      }
      else
      {
        return SkinSelectors.AF_TRAIN_STOP_STYLE_CLASS;
      }
    }

    public String getEndJoinState()
    {
      if(isOverflowEnd())
      {
        return null;
      }
      else if(!hasNext())
      {
        ParentTrain parent = _train.getParentTrain();
        if(parent != null && parent.hasParentEnd())
        {
          return _STATE_PARENT;
        }
        else
        {
          return null;
        }
      }
      else if(getNext().isVisited())
      {
        return _STATE_VISITED;
      }
      else
      {
        if (getNext().isDisabled())
        {
          return _STATE_DISABLED;
        }
        return _STATE_UNVISITED;
      }
    }

    public List<String> getIconNames()
    {
      if(isOverflowEnd())
      {
        return _getOverflowEndIconNames();
      }
      else if(isOverflowStart())
      {
        return _getOverflowStartIconNames();
      }
      else if(isParentStart())
      {
        return Collections.singletonList(SkinSelectors.AF_TRAIN_PARENT_START_ICON_NAME);
      }
      else if(isParentEnd())
      {
        return Collections.singletonList(SkinSelectors.AF_TRAIN_PARENT_END_ICON_NAME);
      }
      else
      {
        return _getStopIconNames();
      }
    }

    public String getLabel()
    {
      return _label;
    }

    public Station getNext()
    {
      return _next;
    }

    public Station getPrevious()
    {
      return _previous;
    }

    public int getRowIndex()
    {
      return _rowIndex;
    }

    public Object getRowKey()
    {
      return _rowKey;
    }

    public String getStartJoinState()
    {
      if(isOverflowStart())
      {
        return null;
      }
      else if(!hasPrevious())
      {
        ParentTrain parent = _train.getParentTrain();
        if(parent != null && parent.hasParentStart())
        {
          return _STATE_PARENT;
        }
        else
        {
          return null;
        }
      }
      else if(isVisited())
      {
        return _STATE_VISITED;
      }
      else
      {
        if(isDisabled())
        {
          return _STATE_DISABLED;
        }
        return _STATE_UNVISITED;
      }
    }

    public List<String> getStates()
    {
      List<String> states = new ArrayList<String>(5);
      if(isParentStart() || isParentEnd())
      {
        return states;
      }

      if(isActive())
      {
        states.add(_STATE_ACTIVE);
        return states;
      }
      else if(isVisited())
      {
        states.add(_STATE_VISITED);
      }
      else
      {
        states.add(_STATE_UNVISITED);
      }

      if(isDisabled())
      {
        states.add(_STATE_READ_ONLY);
      }

      return states;
    }

    public boolean hasNext()
    {
      return _next != null;
    }

    public boolean hasPrevious()
    {
      return _previous != null;
    }


    /**
     * return the string to use for the text of the station
     * it is the text of the link or "Previous" or "More"
     */
    public void initLabel(
      RenderingContext arc,
      UIComponent      stamp)
    {
      if(isOverflowStart())
      {
        _label = arc.getTranslatedString(_PREVIOUS_KEY);
      }
      else if(isOverflowEnd())
      {
        _label = arc.getTranslatedString(_MORE_KEY);
      }
      else
      {
        Object text = stamp.getAttributes().get("text");
        if(text != null)
        {
          _label = text.toString();
          if (isScreenReaderMode(arc))
          {
            _label = _getDisabledUserText(arc, _label);
          }
        }
        else
        {
          _label = null;
        }
      }
    }

    public boolean isActive()
    {
      return _active;
    }

    public boolean isDisabled()
    {
      return _disabled;
    }

    public boolean isNextDisabled()
    {
      return hasNext() && _next.isDisabled();
    }

    public boolean isOverflowEnd()
    {
      return _overflowEnd;
    }

    public boolean isOverflowStart()
    {
      return _overflowStart;
    }

    public boolean isParentEnd()
    {
      return _parentEnd;
    }

    public boolean isParentStart()
    {
      return _parentStart;
    }

    public boolean isPreviousDisabled()
    {
      return hasPrevious() && _previous.isDisabled();
    }

    public boolean isVisited()
    {
      return _visited;
    }

    public void setDisabled(boolean disabled)
    {
      _disabled = disabled;
    }

    public void setNext(Station next)
    {
      _next = next;
    }

    public void setOverflowEnd(boolean overflowEnd)
    {
      _overflowEnd = overflowEnd;
    }

    public void setOverflowStart(boolean overflowStart)
    {
      _overflowStart = overflowStart;
      _visited       = true;
    }

    public void setParentEnd(boolean parentEnd)
    {
      _parentEnd = parentEnd;
    }

    public void setParentStart(boolean parentStart)
    {
      _parentStart = parentStart;
    }

    public void setPrevious(Station previous)
    {
      _previous = previous;
    }

    private boolean _getBooleanAttribute(
        Map<String, Object> attributes,
        String              attributeName,
        boolean             defaultValue)
    {
      Object value = attributes.get(attributeName);
      if(value == null)
      {
        return defaultValue;
      }

      return Boolean.TRUE.equals(value);
    }

    private String _getDisabledUserText(
      RenderingContext arc,
      String           text)
    {
      String altTextKey;
      if(isActive())
      {
        altTextKey = _ACTIVE_KEY;
      }
      else if(isVisited())
      {
        altTextKey = _VISITED_KEY;
      }
      else
      {
        altTextKey = _NEXT_KEY;
      }

      String altText =
        XhtmlUtils.getFormattedString(arc.getTranslatedString(altTextKey),
                                      new String[]{text});

      return altText;
    }

    private List<String> _getIconNames(String baseSelector)
    {
      LinkedList<String> names = new LinkedList<String>();

      StringBuilder builder = new StringBuilder(64);
      builder.append(baseSelector);

      int suffixLength = SkinSelectors.ICON_SUFFIX.length();
      int baseIndex    = builder.length();

      builder.append(SkinSelectors.ICON_SUFFIX);
      names.addFirst(builder.toString());
      builder.delete(baseIndex, baseIndex + suffixLength);

        if(isActive())
        {
          builder.append(_SUFFIX_ACTIVE);
        }
        else if(isVisited())
        {
          builder.append(_SUFFIX_VISITED);
        }
        else
        {
          builder.append(_SUFFIX_UNVISITED);
        }

        baseIndex = builder.length();

        builder.append(SkinSelectors.ICON_SUFFIX);
        names.addFirst(builder.toString());

        builder.delete(baseIndex, baseIndex + suffixLength);

      if (isDisabled())
        {
          builder.append(_SUFFIX_READ_ONLY);
          builder.append(SkinSelectors.ICON_SUFFIX);
          names.addFirst(builder.toString());
        }
      return names;
    }

    private List<String> _getOverflowEndIconNames()
    {
      return _getIconNames(SkinSelectors.AF_TRAIN_OVERFLOW_END_STYLE_CLASS);
    }

    private List<String> _getOverflowStartIconNames()
    {
      return _getIconNames(SkinSelectors.AF_TRAIN_OVERFLOW_START_STYLE_CLASS);
    }

    private List<String> _getStopIconNames()
    {
      return _getIconNames(SkinSelectors.AF_TRAIN_STOP_STYLE_CLASS);
    }

    private boolean _active;       // Is this station the active one?
    private boolean _disabled;     // Disabled attribute
    private boolean _overflowEnd; // Is this station the next step set link?
    private boolean _overflowStart; // Is this station the prev step set link?
    private boolean _parentEnd;    // Is this station a parent end?
    private boolean _parentStart;  // Is this station a parent start?
    private boolean _visited;      // visited attribute

    private int _rowIndex; // Row index

    private Object _rowKey; // Row key

    private String _label; // This station's label

    private Station _next;
    private Station _previous;

    private Train _train;
  }

  private static class ParentTrain
  {
    public ParentTrain(
        RenderingContext arc,
        UIXProcess       process,
        UIComponent      stamp,
        Train            train)
    {
      List<Station> stations     = train.getStations();
      int           stationCount = stations.size();

      boolean hasParentStart = !stations.get(0).isOverflowStart();
      boolean hasParentEnd   = !stations.get(stationCount - 1).isOverflowEnd();

      if(hasParentStart || hasParentEnd)
      {
        Object parentStartRowKey = process.getContainerRowKey();
        process.setRowKey(parentStartRowKey);
        int rowIndex = process.getRowIndex();
        if(hasParentStart)
        {
          _parentStart = new Station(train, rowIndex, parentStartRowKey);
          _parentStart.setParentStart(true);
          _parentStart.initLabel(arc, stamp);
        }

        rowIndex = rowIndex + 1;

        // Check if the parent has more steps, render it only if it does
        hasParentEnd = rowIndex < process.getRowCount();
        if(hasParentEnd)
        {
          process.setRowIndex(rowIndex);
          _parentEnd = new Station(train, rowIndex, process.getRowKey());
          _parentEnd.setParentEnd(true);
          _parentEnd.initLabel(arc, stamp);
        }

        // Restore the model
        process.setRowKey(train.getInitialRowKey());
      }
    }

    public Station getParentEnd()
    {
      return _parentEnd;
    }

    public Station getParentStart()
    {
      return _parentStart;
    }

    public boolean hasParentEnd()
    {
      return _parentEnd != null;
    }

    public boolean hasParentStart()
    {
      return _parentStart != null;
    }

    private Station _parentEnd;
    private Station _parentStart;
  }

  /**
   * Gives the amount of visible stops that get rendered by default if no
   * amount is specified by the -tr-visible-stop-count skin property.
   */
  public static final int DEFAULT_MAX_VISIBLE_STOP_COUNT  = 6;

  /**
   * Determines if the parent train of sub-trains should be rendered by
   * default if not specified by the -tr-render-parent-train skin property.
   */
  public static final boolean DEFAULT_RENDER_PARENT_TRAIN = false;

  private static final String _STATE_ACTIVE    = SkinSelectors.STATE_PREFIX + "Selected";
  private static final String _STATE_DISABLED  = SkinSelectors.STATE_PREFIX + "Disabled";
  private static final String _STATE_PARENT    = SkinSelectors.STATE_PREFIX + "Parent";
  private static final String _STATE_READ_ONLY = SkinSelectors.STATE_PREFIX + "ReadOnly";
  private static final String _STATE_UNVISITED = SkinSelectors.STATE_PREFIX + "Unvisited";
  private static final String _STATE_VISITED   = SkinSelectors.STATE_PREFIX + "Visited";

  private static final String _SUFFIX_CONTENT    = "-content";
  private static final String _SUFFIX_ICON_CELL  = "-icon-cell";

  private static final String _SUFFIX_ACTIVE     = ":selected";
  private static final String _SUFFIX_READ_ONLY  = ":read-only";
  private static final String _SUFFIX_UNVISITED  = ":unvisited";
  private static final String _SUFFIX_VISITED    = ":visited";

  /**
   * The following keys are used to get at the corresponding translated
   * strings.
   */
  private static final String _VISITED_KEY = "af_train.VISITED_TIP";
  private static final String _ACTIVE_KEY = "af_train.ACTIVE_TIP";
  private static final String _NEXT_KEY = "af_train.NEXT_TIP";
  private static final String _MORE_KEY = "af_train.MORE";
  private static final String _PREVIOUS_KEY = "af_train.PREVIOUS";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TrainRenderer.class);

  private static final String[] _EMPTY_STRING_ARRAY;

  private static final Map<String, String> _RESOURCE_KEY_MAP;

  static
  {
    _EMPTY_STRING_ARRAY = new String[0];

    // Not adding the base link classes as before, those are a nuisance
    // while defining the skin since oyu cannot inhibit them as they're
    // on the same level as the train selectors.
    _RESOURCE_KEY_MAP = new TreeMap<String, String>();
    _RESOURCE_KEY_MAP.put(SkinSelectors.LINK_STYLE_CLASS,
                          SkinSelectors.AF_TRAIN_LINK_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.LINK_DISABLED_STYLE_CLASS,
                          SkinSelectors.AF_TRAIN_LINK_STYLE_CLASS);
  }
}
