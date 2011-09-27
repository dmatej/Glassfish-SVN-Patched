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
package org.apache.myfaces.trinidadinternal.context;

import java.io.IOException;
import java.io.Writer;

import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.faces.FacesException;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitHint;
import javax.faces.component.visit.VisitResult;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.PartialResponseWriter;
import javax.faces.context.PartialViewContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.PhaseId;
import javax.faces.render.RenderKit;

import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.PPRResponseWriter;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.XmlResponseWriter;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;


public class PartialViewContextImpl
  extends PartialViewContext
{
  public PartialViewContextImpl(FacesContext context)
  {
    _context = context;

    ExternalContext extContext = context.getExternalContext();

    _requestType = ReqType.FULL;

    if (_PARTIAL_AJAX.equals(extContext.getRequestHeaderMap().get(_FACES_REQUEST)) ||                        
        _PARTIAL_AJAX.equals(extContext.getRequestParameterMap().get(_FACES_REQUEST)))                   
    {                                                                                     
      // This request was sent with jsf.ajax.request()
      if (extContext.getRequestParameterMap().get(_TRINIDAD_PPR) != null)
        _requestType = ReqType.AJAX_LEGACY;
      else
        _requestType = ReqType.AJAX;
    }
    else if (CoreRenderKit.isLegacyPartialRequest(extContext))
    {
      _requestType = ReqType.LEGACY;
    }
  }

  /**
   * To force a full execute we always return an empty execute list.
   * @return
   */
  public Collection<String> getExecuteIds()
  {
    _assertNotReleased();
    if (_executeIds == null)
    {
      _executeIds = _getIds(PARTIAL_EXECUTE_PARAM_NAME);

      // force view parameter facet ID to be executed if we are executing anything else
      if (!_executeIds.isEmpty())
      {
        UIViewRoot root = _context.getViewRoot();
        if (root.getFacetCount() > 0)
        {
          UIComponent facet =
            root.getFacet(UIViewRoot.METADATA_FACET_NAME);
          if (facet != null)
          {
            _executeIds.add(facet.getClientId(_context));
          }
        }
      }
    }

    return _executeIds;
  }

  public Collection<String> getRenderIds()
  {
    _assertNotReleased();
    if (_renderIds == null)
    {
      _renderIds = _getIds(PARTIAL_RENDER_PARAM_NAME);
    }
    return _renderIds;
  }


  public boolean isAjaxRequest()
  {
    _assertNotReleased();
    return (_requestType != ReqType.FULL);
  }

  public boolean isPartialRequest()
  {
    _assertNotReleased();
    if (_partialRequest == null)
    {
      _partialRequest =
          isAjaxRequest() || _PARTIAL_PROCESS.equals(_context.getExternalContext().getRequestHeaderMap().get(_FACES_REQUEST));
    }
    return _partialRequest;
  }

  @Override
  public boolean isExecuteAll()
  {
    _assertNotReleased();
    if (_executeAll == null)
    {
      if (_requestType == ReqType.AJAX_LEGACY ||
          _requestType == ReqType.LEGACY)
      {
        // We are preserving old behavior (execute everything) for the legacy 'partialSubmit=true'
        // Trinidad requests
        _executeAll = true;
      }
      else
      {

        String execute =
          _context.getExternalContext().getRequestParameterMap().get(PARTIAL_EXECUTE_PARAM_NAME);

        _executeAll = execute.equals(ALL_PARTIAL_PHASE_CLIENT_IDS);
      }
    }
    return _executeAll;
  }

  public boolean isRenderAll()
  {
    _assertNotReleased();
    if (_renderAll == null)
    {
      String render =
        _context.getExternalContext().getRequestParameterMap().get(PARTIAL_RENDER_PARAM_NAME);
      _renderAll = ALL_PARTIAL_PHASE_CLIENT_IDS.equals(render);
    }

    return _renderAll;
  }

  @Override
  public void processPartial(PhaseId phaseId)
  {
    UIViewRoot viewRoot = _context.getViewRoot();
    if (phaseId == PhaseId.APPLY_REQUEST_VALUES ||
        phaseId == PhaseId.PROCESS_VALIDATIONS ||
        phaseId == PhaseId.UPDATE_MODEL_VALUES)
    {
      _processExecute(viewRoot, phaseId);
    }
    else if (phaseId == PhaseId.RENDER_RESPONSE)
    {
      _processRender(viewRoot);
    }

  }

  @Override
  public PartialResponseWriter getPartialResponseWriter()
  {
    ResponseWriter current = _context.getResponseWriter();
    if (current != null && current instanceof PartialResponseWriter)
    {
      return (PartialResponseWriter)current;
    }

    if (current != null)
    {
      if (_context.getCurrentPhaseId() == PhaseId.RENDER_RESPONSE)
      {
        _LOG.warning("getPartialResponseWriter() called during render_reponse. " +
                     "The returned writer is not integrated with PPRResponseWriter");
      }
      return new PartialResponseWriter(current);
    }

    ExternalContext extContext = _context.getExternalContext();
    String encoding = "UTF-8";
    extContext.setResponseCharacterEncoding(encoding);
    Writer out = null;
    try
    {
      out = extContext.getResponseOutputWriter();
    }
    catch (IOException ioe)
    {
      _LOG.severe(ioe.toString(), ioe);
    }

    if (out != null)
    {
      ResponseWriter responseWriter = null;
      RenderKit kit = _context.getRenderKit();
      if (kit != null)
      {
        responseWriter = kit.createResponseWriter(out, "text/xml", encoding);
      }
      else
      {
        responseWriter = new XmlResponseWriter(out, encoding);
      }

      return new PartialResponseWriter(responseWriter);
    }

    return null;
  }

  public void setRenderAll(boolean renderAll)
  {
    // Ignore setRenderAll() from  Mojarra's NavigationHandler
    // to preserve old (redirect) behavior
    //_renderAll = renderAll;
  }

  public void setPartialRequest(boolean isPartialRequest)
  {
    _partialRequest = isPartialRequest;
  }

  public void release()
  {
    _executeIds = null;
    _context = null;
    _partialRequest = null;
    _released = true;
    _renderAll = null;
    _renderIds = null;
  }

  private void _processExecute(UIViewRoot component, PhaseId phaseId)
  {
    // We are only handling visit-based (partial) execution here.
    // Full execution (isExecuteAll() == true) is handled by the UIViewRoot
    // Note that this is different from the render phase

    Collection<String> executeIds = getExecuteIds();
    if (executeIds == null || executeIds.isEmpty())
    {
      _LOG.warning("No execute Ids were supplied for the Ajax request");
      return;
    }

    VisitContext visitContext = VisitContext.createVisitContext(_context, executeIds,
                        EnumSet.of(VisitHint.SKIP_UNRENDERED, VisitHint.EXECUTE_LIFECYCLE));
    VisitCallback visitCallback = new ProcessPhaseCallback(_context, phaseId);

    component.visitTree(visitContext, visitCallback);
  }


  private void _processRender(UIComponent viewRoot)
  {
    ExternalContext extContext = _context.getExternalContext();
    extContext.setResponseContentType(_RESPONSE_CONTENT_TYPE);
    extContext.addResponseHeader("Pragma", "no-cache");
    extContext.addResponseHeader("Cache-control", "no-cache");

    if (isRenderAll())
    {
      _renderAll(_context, viewRoot);
      return;
    }

    ResponseWriter origResponseWriter = _context.getResponseWriter();

    RenderingContext rc = RenderingContext.getCurrentInstance();
    assert (rc != null);

    boolean bufferScripts = _requestType == ReqType.LEGACY;
    PPRResponseWriter pprWriter =
      new PPRResponseWriter(origResponseWriter, rc, bufferScripts);

    _context.setResponseWriter(pprWriter);

    if (_requestType == ReqType.AJAX)
    {
      // Add render Ids as partial targets for the request from <f:ajax>
      _addRenderIdsAsPartialTargets(rc);

      // Force visit-based rendering for the <f:ajax> requests
      PartialPageUtils.forceOptimizedPPR(_context);
    }

    try
    {
      pprWriter.startDocument();

      // Note that PanelPartialRootRenderer will perform partial visit for the optimized PPR
      // if it is enabled
      _renderChildren(_context, viewRoot);

      // PDA's JavaScript DOM is not capable of updating the ViewState just by
      // using ViewState's value, so for PDAs, FormRenderer will again render
      // the ViewState as a hidden element during its postscript element rendering
      if (!CoreRenderer.isPDA(rc))
      {
        // Always write out ViewState as a separate update element.
        String state =
                _context.getApplication().getStateManager().getViewState(_context);
        pprWriter.writeViewState(state);
      }

      pprWriter.endDocument();
    }
    catch (IOException e)
    {
      // launder the IOException as a FacesException, we'll unwrap this later
      throw new FacesException(e);
    }
    finally
    {
      _context.setResponseWriter(origResponseWriter);
    }
  }

  private void _renderAll(FacesContext context, UIComponent viewRoot)
  {
    ResponseWriter origResponseWriter = context.getResponseWriter();

    // Use JSF-supplied PartialResponseWriter, since we are rendering the full page
    PartialResponseWriter rw = new PartialResponseWriter(origResponseWriter);

    context.setResponseWriter(rw);

    try
    {
      rw.startDocument();

      rw.startUpdate(PartialResponseWriter.RENDER_ALL_MARKER);
      _renderChildren(context, viewRoot);
      rw.endUpdate();

      //write out JSF state
      rw.startUpdate(PartialResponseWriter.VIEW_STATE_MARKER);
      String state = context.getApplication().getStateManager().getViewState(context);
      rw.write(state);
      rw.endUpdate();

      rw.endDocument();
    }
    catch(IOException e)
    {
      throw new FacesException(e);
    }
    finally
    {
      context.setResponseWriter(origResponseWriter);
    }
  }

  private void _addRenderIdsAsPartialTargets(RenderingContext rc)
  {
    Collection<String> renderIds = getRenderIds();
    PartialPageContext pc = rc.getPartialPageContext();
    for (String id: renderIds)
    {
      pc.addPartialTarget(id);
    }
  }


  private static void _renderChildren(FacesContext context, UIComponent root) throws IOException
  {
    Iterator<UIComponent> iterator = root.getFacetsAndChildren();
    while (iterator.hasNext())
    {
      UIComponent child = iterator.next();
      if (child.isRendered())
        child.encodeAll(context);
    }
  }

  private void _assertNotReleased()
  {
    if (_released)
    {
      throw new IllegalStateException();
    }
  }

  private Set<String> _getIds(String parameter)
  {
    String param =
      _context.getExternalContext().getRequestParameterMap().get(parameter);
    if (param != null)
    {
      String ids[] = param.split("[ \t]+");
      if (ids != null && ids.length > 0)
      {
        HashSet<String> idSet = new HashSet<String>(ids.length);
        for (int i = 0; i < ids.length; i++)
        {
          idSet.add(ids[i]);
        }
        return idSet;
      }
    }

    return Collections.emptySet();
  }

  private static final class ProcessPhaseCallback
    implements VisitCallback
  {
    ProcessPhaseCallback(FacesContext context, PhaseId phaseId)
    {
      _context = context;
      _phaseId = phaseId;
    }

    public VisitResult visit(VisitContext context, UIComponent target)
    {
      if (_phaseId == PhaseId.APPLY_REQUEST_VALUES)
      {
        target.processDecodes(_context);
      }
      else if (_phaseId == PhaseId.PROCESS_VALIDATIONS)
      {
        target.processValidators(_context);
      }
      else if (_phaseId == PhaseId.UPDATE_MODEL_VALUES)
      {
        target.processUpdates(_context);
      }


      // No need to visit children, since they will be executed/rendred by their parents
      return VisitResult.REJECT;
    }

    private FacesContext _context;
    private PhaseId _phaseId;
  }

  private ReqType _requestType;
  private Set<String> _executeIds;
  private Set<String> _renderIds;
  private FacesContext _context = null;
  private Boolean _partialRequest;
  private boolean _released;
  private Boolean _renderAll = null;
  private Boolean _executeAll = null;


  private static final String _RESPONSE_CONTENT_TYPE = "text/xml";
  private static final String _FACES_REQUEST = "Faces-Request";
  private static final String _PARTIAL_AJAX = "partial/ajax";
  private static final String _PARTIAL_PROCESS = "partial/process";
  private static final String _TRINIDAD_PPR = "Tr-PPR-Message";


  private static enum ReqType
  {
    FULL,
    AJAX,
    AJAX_LEGACY,
    LEGACY;
  }

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(PartialViewContextImpl.class);

}
