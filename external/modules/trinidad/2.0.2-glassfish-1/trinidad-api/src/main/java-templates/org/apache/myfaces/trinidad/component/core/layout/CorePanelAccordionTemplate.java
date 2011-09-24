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
package org.apache.myfaces.trinidad.component.core.layout;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitContextWrapper;
import javax.faces.component.visit.VisitHint;
import javax.faces.component.visit.VisitResult;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;

import org.apache.myfaces.trinidad.component.FlattenedComponent;
import org.apache.myfaces.trinidad.component.UIXPanel;
import org.apache.myfaces.trinidad.component.UIXShowDetail;
import org.apache.myfaces.trinidad.event.DisclosureEvent;


abstract public class CorePanelAccordion extends UIXPanel
{
/**/ public abstract boolean isDiscloseMany();

  /**
   * Queues an event recursively to the root component.
   * @param event
   * @throws javax.faces.event.AbortProcessingException
   */
  @Override
  public void queueEvent(FacesEvent event)
    throws AbortProcessingException
  {
    // For a "show-one" panel accordion, handle an "expanding"
    // DisclosureEvent specifically, only if the source
    // is one of its immediate children
    if ((event instanceof DisclosureEvent) &&
        !isDiscloseMany() &&
        (this == event.getComponent().getParent()) &&
        ((DisclosureEvent) event).isExpanded())

    {
      for (UIComponent comp : ((List<UIComponent>) getChildren()))
      {
        // Skip over the show detail that is the source of this event
        if (comp == event.getComponent())
          continue;

        if (comp instanceof UIXShowDetail)
        {
          UIXShowDetail showDetail = (UIXShowDetail) comp;
          // Queue an event to hide the currently expanded showDetail
          if (showDetail.isDisclosed())
            (new DisclosureEvent(showDetail, false)).queue();
        }
      }
    }
    super.queueEvent(event);
  }

  @Override
  public boolean visitTree(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    if (visitContext.getHints().contains(VisitHint.SKIP_UNRENDERED) &&
     !isDiscloseMany())
    {
      // Filter which children to be visited so that only one show detail
      // is visited for this accordion
      visitContext = new PartialVisitContext(visitContext);
    }
    return super.visitTree(visitContext, callback);
  }

  protected boolean isChildSelected(
    UIXShowDetail component)
  {
    return component.isDisclosed();
  }

  private class PartialVisitContext
    extends VisitContextWrapper
  {
    PartialVisitContext(
      VisitContext wrapped)
    {
      _wrapped = wrapped;
    }

    public VisitContext getWrapped()
    {
      return _wrapped;
    }

    @Override
    public VisitResult invokeVisitCallback(
      UIComponent   component,
      VisitCallback visitCallback)
    {
      if (component instanceof UIXShowDetail)
      {
        UIXShowDetail showDetail = (UIXShowDetail)component;
        if (_isShowDetailForCurrentComponent(showDetail))
        {
          if (_foundItemToRender || !isChildSelected(showDetail))
          {
            // We already visited the one to be shown
            return VisitResult.REJECT;
          }
          else
          {
            _foundItemToRender = true;
          }
        }
      }

      return super.invokeVisitCallback(component, visitCallback);
    }

    private boolean _isShowDetailForCurrentComponent(
      UIXShowDetail showDetail)
    {
      for (UIComponent parent = showDetail.getParent(); parent != null;
           parent = parent.getParent())
      {
        if (parent == CorePanelAccordion.this)
        {
          return true;
        }

        if (parent instanceof FlattenedComponent &&
          ((FlattenedComponent)parent).isFlatteningChildren(getFacesContext()))
        {
          continue;
        }

        // The first-non flattened component is not the show one, do not filter it
        return false;
      }

      return false;
    }

    private boolean      _foundItemToRender;
    private VisitContext _wrapped;
  }
}