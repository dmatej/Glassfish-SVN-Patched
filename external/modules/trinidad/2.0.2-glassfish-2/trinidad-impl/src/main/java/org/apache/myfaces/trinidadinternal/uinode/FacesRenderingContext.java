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
package org.apache.myfaces.trinidadinternal.uinode;

import java.io.IOException;

import java.util.Map;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidad.context.PartialPageContext;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.share.config.ContextBasedConfiguration;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.RootRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.style.StyleContext;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;

/**
 * RenderingContext implementation that supports JSF.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/FacesRenderingContext.java#0 $) $Date: 10-nov-2005.18:49:15 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class FacesRenderingContext extends RootRenderingContext
{
  /**
   * Gets the current RenderingContext.
   * @todo Rename to getCurrentInstance()
   * @todo Why are passing a UIComponent?  In some places,
   *   we're passing null for the component!
   */
  static public UIXRenderingContext getRenderingContext(
    FacesContext fContext,
    UIComponent  component) throws IOException
  {
    return getRenderingContext(fContext, component, true);
  }

  /**
   * Gets the current RenderingContext.
   * @todo Rename to getCurrentInstance()
   * @todo Why are passing a UIComponent?  In some places,
   *   we're passing null for the component!
   */
  static public UIXRenderingContext getRenderingContext(
    FacesContext fContext,
    UIComponent  component,
    boolean      createIfNull) throws IOException
  {
    return UINodeRendererBase.getRenderingContext(fContext,
                                                  component,
                                                  createIfNull);
  }


  static public FacesRenderingContext createRenderingContext(
    FacesContext fContext) throws IOException
  {
    if (UINodeRendererBase.__getRenderingContext(fContext) != null)
      throw new IllegalStateException(_LOG.getMessage(
        "RENDERINGCONTEXT_HAS_BEEN_CREATED"));

    FacesRenderingContext rContext = new FacesRenderingContext(fContext);

    UINodeRendererBase.__setRenderingContext(fContext, rContext);

    return rContext;
  }


  /**
   * Gets the current PartialPageContext.
   */
  static public PartialPageContext getPartialPageContext(FacesContext fContext)
  {
    return RenderingContext.getCurrentInstance().getPartialPageContext();
  }

  /**
   * Creates a FacesRenderingContext.
   * @param fContext the context
   */
  private FacesRenderingContext(FacesContext fContext)
  {
    super();

    _adfRenderingContext = RenderingContext.getCurrentInstance();

    init(fContext);

    RequestContext context = RequestContext.getCurrentInstance();
    String outputMode = context.getOutputMode();
    if (outputMode != null)
      setFacet(outputMode);

    _initializeConfiguration(fContext, context);

    _initializePPR(fContext);
  }

  public PartialPageContext getPartialPageContext()
  {
    return _adfRenderingContext.getPartialPageContext();
  }

  @Override
  public LocaleContext getLocaleContext()
  {
    return _adfRenderingContext.getLocaleContext();
  }


  /**
   * Get an interface that can be used for style lookups and generation.
   */
  public StyleContext getStyleContext()
  {
    return ((CoreRenderingContext) _adfRenderingContext).getStyleContext();
  }

  /**
   * Returns the Skin to use for this render.
   */
  public Skin getSkin()
  {
    return _adfRenderingContext.getSkin();
  }

  public TrinidadAgent getAgent()
  {
    return ((CoreRenderingContext) _adfRenderingContext).getTrinidadAgent();
  }

  @Override
  protected Object getRenderingProperty(Object key)
  {
    return _adfRenderingContext.getProperties().get(key);
  }

  @Override
  protected void setRenderingProperty(Object key, Object value)
  {
    _adfRenderingContext.getProperties().put(key, value);
  }
  /**
   * Store a Map that maps a skin's resource keys from one key to another.
   * For example, if the renderer uses a new HideShowBean, it will need
   * to map the HideShowBean's keys to its keys. It can store the map
   * here, so that context.getTranslatedValue(key) can use this map to get
   * the correct translated value key.
   * @param mapping
   */
  public void setSkinResourceKeyMap(Map<String, String> mapping)
  {
    _adfRenderingContext.setSkinResourceKeyMap(mapping);
  }


  /**
   * Get the _skinResourceKeyMap Map.
   * @param mapping
   */
  public Map<String, String> getSkinResourceKeyMap()
  {
    return _adfRenderingContext.getSkinResourceKeyMap();
  }


  private void _initializeConfiguration(FacesContext fContext,
                                        RequestContext context)
  {
    setConfiguration(new ContextBasedConfiguration(fContext, context));
  }

  //
  // Initialize PPR, if needed
  //
  @SuppressWarnings("unchecked")
  private void _initializePPR(
    FacesContext    fContext)
  {
    PartialPageContext pprContext =
      _adfRenderingContext.getPartialPageContext();
    if (pprContext != null)
    {
      // For compatibility with our current renderers, look for
      // the PARTIAL_TARGETS parameter, and add any that are found
      Map<String, String> parameters = 
        fContext.getExternalContext().getRequestParameterMap();
      String param = parameters.get(UIConstants.PARTIAL_TARGETS_PARAM);
      if ((null != param) && !"".equals(param))
      {
        _LOG.finer("Adding partial targets from parameter: {0}", param);
        // Parse the parameter value to a String[]
        String[] partialTargets = XMLUtils.parseNameTokens(param);
        for (int i = 0; i < partialTargets.length; i++)
          pprContext.addPartialTarget(partialTargets[i]);
      }
    }
  }

  private RenderingContext _adfRenderingContext;

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(FacesRenderingContext.class);

}
