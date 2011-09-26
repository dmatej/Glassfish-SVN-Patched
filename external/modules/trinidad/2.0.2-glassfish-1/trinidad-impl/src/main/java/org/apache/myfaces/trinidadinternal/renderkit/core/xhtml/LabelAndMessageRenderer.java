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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXGroup;
import org.apache.myfaces.trinidad.component.UIXPanel;
import org.apache.myfaces.trinidad.component.html.HtmlTableLayout;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableRenderingContext;


/**
 * @todo Support "end" facet???
 */
public abstract class LabelAndMessageRenderer extends XhtmlRenderer
{
  static public final String INLINE_MESSAGE_DEFAULT_GAP = "12";
  static public final String INLINE_MESSAGE_PDA_GAP     = "2";

  public LabelAndMessageRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _labelKey        = type.findKey("label");
    _requiredKey = type.findKey("required");
    _showRequiredKey = type.findKey("showRequired");

    _message = new Message(type);
    _label = new Label(type, false);
    _labelInTable = new Label(type, true);
  }

  private boolean _needsTableTag(
    UIComponent component)
  {
    // Find the first content-generating parent
    UIComponent parent = XhtmlUtils.getStructuralParent(component);
    if (parent == null)
      return true;

    // =-=AEW We should review this code.
    // Either the parent should mark down that it rendered
    // a table, or we should lean on the ResponseWriter
    // to tell us if a table had been used.

    // Hardcoding some packages 'cause I need this code to change!
    String family = parent.getFamily();
    if (HtmlTableLayout.COMPONENT_FAMILY.equals(family))
    {
      return false;
    }

    return true;
  }

  private boolean _isParentPanelForm(
    FacesContext context,
    UIComponent  component)
  {
    if (PanelFormLayoutRenderer.__isInPanelFormLayout(context, component))
    {
      return true;
    }

    // We must know if the immediate parent is a PanelForm because if there is
    // even just one other component inbetween this component and a PanelForm,
    // we must render different DOM--the same DOM as if there were no parent
    // PanelForm.
    UIComponent parentComponent = component.getParent();
    String family = parentComponent.getFamily();
    // FIXME: OK... This is another strong coupling
    //        We could add a an interface instead like ComponentHolder or something
    //        instead of checking against a specific component family.
    while (UIXGroup.COMPONENT_FAMILY.equals(family))
    {
      // Special case:
      // Since UIXGroup components are purely organizational, it is valid to
      // have them inbetween panelForm-friendly components and the panelForm,
      // so we need to look further up the chain:
      parentComponent = parentComponent.getParent();
      if (parentComponent == null)
      {
        return false;
      }
      family = parentComponent.getFamily();
    }
    if (UIXPanel.COMPONENT_FAMILY.equals(family))
    {
      String rendererType = parentComponent.getRendererType();
      if (_isFormRendererType(rendererType))
        return true;
    }
    return false;
  }

  /**
   * Retrieves whether the labels should be rendered above fields as opposed to
   * the side of the fields.
   * @param context the Faces context
   * @param needsPanelFormLayout true if using a panelFormLayout
   * @return true if labels are stacked above fields
   */
  private boolean _isLabelStartAligned(
    FacesContext context,
    boolean      needsPanelFormLayout)
  {
    // For narrow-screen PDAs, always render the Label above fields.
    if (supportsNarrowScreen(RenderingContext.getCurrentInstance()))
      return false;

    if (needsPanelFormLayout)
    {
      Map requestMap = context.getExternalContext().getRequestMap();
      Object labelsStartAligned = requestMap.get(
        PanelFormLayoutRenderer.PANEL_FORM_LAYOUT_LABELS_START_ALIGNED_KEY);
      return Boolean.TRUE.equals(labelsStartAligned);
    }
    return true;
  }

  // Put the outer style class here, like af_selectManyRadio, styleClass,
  // inlineStyle, 'state' styles like p_AFDisabled, etc.
  protected void renderRootDomElementStyles(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // do nothing for now
  }

  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  private boolean _isInTable()
  {
    TableRenderingContext tContext =
      TableRenderingContext.getCurrentInstance();
    if (tContext != null)
    {
      return TableRenderingContext.isInsideContentOfTable();
    }

    return false;
  }

  /**
   */
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String clientId = component.getClientId(context);

    // If we're a leaf component, see if we can skip our rendering
    if (isLeafRenderer() && canSkipRendering(rc, clientId))
    {
      // Except we really do have a "help" facet, so render that one...
      UIComponent help = getFacet(component, "help");
      if (help != null)
        encodeChild(context, help);
      return;
    }

    String saved = rc.getCurrentClientId();
    rc.setCurrentClientId(clientId);

    boolean isInTable = _isInTable();

    if (hasOwnLabel(component, bean) || isInTable)
    {
      String value = getLabel(component, bean);
      FormData fd = rc.getFormData();
      if (fd != null)
        fd.addLabel(clientId, value);
    }

    RequestContext requestContext = RequestContext.getCurrentInstance();
    boolean needsPanelFormLayout = _isParentPanelForm(context, component);
    boolean isInline = (requestContext.getClientValidation() ==
                        RequestContext.ClientValidation.INLINE);
    if (isInTable && !needsPanelFormLayout)
    {
      ResponseWriter rw = context.getResponseWriter();
      delegateRenderer(context, rc, component, bean, _labelInTable);
      renderFieldCellContents(context, rc, component, bean);

      if (isInline || hasMessage(context, rc, component, bean))
      {
        rw.startElement("div", null);
        rw.endElement("div");
        _renderMessageCellContents(context, rc, component, bean);
      }

      // In the case of narrow-screen PDAs, to reduce the component's width,
      // End facet is always rendered below.
      renderEndFacetForNarrowPDA(context, rc, component, true);
    }
    else
    {
      ResponseWriter rw = context.getResponseWriter();
      boolean isLabelStartAligned = _isLabelStartAligned(context,
                                                       needsPanelFormLayout);

      // need a table if
      // 1) panelForm-friendly component with stacked prompt
      // 2) not a non-panelForm-friendly component
      boolean needsTableTag = !isLabelStartAligned ||
                    (!needsPanelFormLayout && _needsTableTag(component));
                    
      boolean isPIE = Agent.PLATFORM_PPC.equalsIgnoreCase(
                                        rc.getAgent().getPlatformName());

      if (needsTableTag)
      {
        // While handling a PPR response, Windows Mobile cannot DOM replace
        // a table element. Wrapping a table element with a div element fixes
        // the problem.
        if (isPIE)
        {
          rw.startElement("div", component);
          renderId(context, component);
          rw.startElement("table", null);
        }
        else
        {
          rw.startElement("table", component);
        }
        
        // =-=AEW THIS DOESN'T SEEM RIGHT - IT SHOULD GO ON THE INPUT FIELD
        // ONLY, RIGHT?  Matching UIX 2.2 behavior here.
        rw.writeAttribute("title", getShortDesc(component, bean), "title");
        if (!isDesktop(rc))
        {
          // On PDA browsers label and message pair is always
          // rendered in full width.
          rw.writeAttribute("width", "100%", null);
        }
        
        if (!isPIE)
          renderId(context, component);

        // put the outer style class here, like af_inputText, styleClass,
        // inlineStyle, 'state' styles like p_AFDisabled, etc.
        renderRootDomElementStyles(context, rc, component, bean);

        OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
      }

      rw.startElement("tr", component);

      if (!needsTableTag)
      {
        // Render the ID or else the component cannot be PPR-ed in a future update (both the label
        // its associated icons, and the field should update if this component is PPR-ed):
        renderId(context, component);

        // -= Simon =- HACK
        // It's ugly, I hate it, but it works and it's better than pushing
        // Cannot use a span either because td is not a valid span child.
        //rw.startElement("td", component);
        //rw.startElement("table", component);
        //OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
        //rw.startElement("tbody", component);
        //rw.startElement("tr", component);
        //renderRootDomElementStyles(context, rc, component, bean);

        // Basically, we're screwed unless we specify panelForm to keep a
        // class-less container opened to receive the rootDomStyles.
        // Even if this is the case we need a way to detect if a new
        // element get opened from encodeBetweenLabelAndFieldCells call.
        // Since the above option is so ugly, I'll assume that.
        // FIXME: That's too strongly coupled to my taste. Even being stuck
        //        with a parent tr is too strongly coupled to my taste.
        renderRootDomElementStyles(context, rc, component, bean);
      }

      boolean labelExists = (getLabel(component, bean) != null);

      _renderLabelCell(context, rc, component, bean, labelExists);

      if (!isLabelStartAligned)
      {
        rw.endElement("tr");
        rw.startElement("tr", null);
      }

      //This part is necessary to make work hspace on tr:tableFormLayout
      Map<String, Object> requestMap = context.getExternalContext()
          .getRequestMap();

      Integer hspaceObject = (Integer) requestMap.get(
        "org.apache.myfaces.trinidadinternal.TableFormHspace");

      Boolean percentWidthObject = (Boolean) requestMap.get(
        "org.apache.myfaces.trinidadinternal.TableFormPercentWidth");

      if (hspaceObject != null)
      {
        rw.startElement("td", null);
        if (percentWidthObject != null && percentWidthObject == true)
        {
          rw.writeAttribute("width", hspaceObject +"%", null);
        }
        else
        {
          rw.writeAttribute("width", hspaceObject, null);
        }
        rw.endElement("td");
      }

      _renderFieldCell(context, rc, component, bean, labelExists, needsPanelFormLayout, isInline);

      rw.endElement("tr");

      // End encoding of the non-panelForm-friendly wrappers:
      if (!needsPanelFormLayout)
      {
        if (isInline || hasMessage(context, rc, component, bean))
        {
          rw.startElement("tr", null);
          if (isLabelStartAligned)
          {
            // We need an empty cell to prevent the label from spanning down into the footer area and
            // to make sure the footer appears below the field cell.
            rw.startElement("td", null);
            rw.endElement("td");
          }

          rw.startElement("td", null);
          renderStyleClass(context, rc,
                           SkinSelectors.AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS);
          _renderMessageCellContents(context, rc, component, bean);
          rw.endElement("td");

          rw.endElement("tr");
        }
      }

      // In the case of narrow-screen PDAs, to reduce the component's width,
      // End facet is always rendered below.
      renderEndFacetForNarrowPDA(context, rc, component, false);

      if (needsTableTag)
      {
        rw.endElement("table");
                
        if (isPIE)
          rw.endElement("div");
      }
    }

    rc.setCurrentClientId(saved);
  }

  // subclasses should override this
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  /**
   * @todo Get cell alignment from skin property.
   */
  private void _renderLabelCell(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    boolean          labelExists
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("td", null);

    // render labelStyleClass and defaultStyleClass.
    renderStyleClasses(context, rc, new String[]
      {
        getLabelStyleClass(component, bean),
        _getDefaultLabelStyleClass(rc, SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS)
      });

    String labelInlineStyle = getLabelInlineStyleKey(component, bean);

    // In the case of narrow-screen PDAs, the Label is rendered above fields.
    // So Label should be left aligned to be in align with fields below.
    if (supportsNarrowScreen(rc))
    {
      labelInlineStyle = labelInlineStyle + ";text-align: left;";
    }

    rw.writeAttribute("style", labelInlineStyle, null);

    String valign = getDefaultLabelValign(component, bean);

    rw.writeAttribute("valign", valign, null);
    if (isDesktop(rc))
    {
      // On PDA browsers where width is limited, the label is allowed to wrap.
      rw.writeAttribute("nowrap", Boolean.TRUE, null);
    }

    if (labelExists)
    {
      rw.writeAttribute("width",
                        rc.getProperties().get(_LABEL_CELL_WIDTH_KEY),
                        null);
    }

    delegateRenderer(context, rc, component, bean, _label);
    rw.endElement("td");
  }

  /**
   * @todo see if bug 2484841 still applies!
   */
  private void _renderFieldCell(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    boolean          labelExists,
    boolean          needsPanelFormLayout,
    boolean          isInline
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("td", null);

    rw.writeAttribute("valign", "top", null);
    if (isDesktop(rc))
    {
      // On PDA browsers where width is limited, the field data is
      // allowed to wrap.
      rw.writeAttribute("nowrap", Boolean.TRUE, null);
    }

    renderStyleClass(context, rc, SkinSelectors.AF_CONTENT_CELL_STYLE_CLASS );

    if (labelExists)
      rw.writeAttribute("width",
                        rc.getProperties().get(_FIELD_CELL_WIDTH_KEY),
                        null);

    renderFieldCellContents(context, rc, component, bean);

    // The panelForm places messages below the fields, not on a separate
    // row:
    if (needsPanelFormLayout)
    {
      // =-= mcc PPR PROBLEM!!!  We should always be rendering the "div",
      //     and always rendering an ID, if we ever want it to be PPR
      //     replaceable:
      if (isInline || hasMessage(context, rc, component, bean))
      {
        rw.startElement("div", null);
        renderStyleClass(context, rc,
                         SkinSelectors.AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS);
        _renderMessageCellContents(context, rc, component, bean);
        rw.endElement("div");
      }
    }

    // bug 2484841: PDA: TOO MUCH WHITESPACE BETWEEN
    //                   INPUT ELEMENTS IN LABELEDFIELD
    // This is a browser bug workaround, hopefully we can remove it eventually
    if (isPDA(rc) && isIE(rc))
    {
      rw.startElement("div", null);
      renderSpacer(context, rc, "1", "0");
      rw.endElement("div");
    }

    rw.endElement("td");
  }

  static String __getCachedClientId(
    RenderingContext rc)
  {
    String clientId = rc.getCurrentClientId();
    assert(clientId != null);
    return clientId;
  }

  protected String getFieldCellContentsStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    // Override if you want a "af_panelLabelAndMessage_content" style class
    return null;
  }

  protected String getFooterContentsStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    // Override if you want a style class for the footer container
    return null;
  }

  protected boolean hasMessage(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    UIComponent help = getFacet(component, "help");
    if (help != null)
      return true;

    String id  = getLabelFor(context, rc, component, bean);
    return context.getMessages(id).hasNext();
  }

  private void _renderMessageCellContents(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
    throws IOException
  {
    delegateRenderer(context, rc, component, bean, _message);
  }

  /**
   * Returns the client ID.
   */
  @Override
  protected String getClientId(
    FacesContext context,
    UIComponent  component)
  {
    return (super.getClientId(context, component) +
            XhtmlConstants.COMPOSITE_ID_EXTENSION);
  }

  protected boolean isLeafRenderer()
  {
    return true;
  }

  protected String getDefaultLabelValign(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  abstract protected void renderFieldCellContents(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException;

  /**
   * Renders footer contents if isFooterPresent() is true.
   * @param context   the FacesContext
   * @param rc        the RenderingContext
   * @param component the component to render
   * @param bean      the FacesBean of the component to render
   * @throws IOException if there are problems rendering the contents
   */
  protected void renderFieldFooterContents(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // render nothing by default
  }

  /**
   * If it's known that the field content is not editable, return false.
   * Otherwise, assume it is editable and return true
   */
  protected boolean isContentEditable(
    UIComponent component,
    FacesBean   bean)
  {
    return true;
  }

  protected boolean isIndented()
  {
    return false;
  }

  /**
   * Override and return "true" to indicate that the component
   * has its own internal label - and that therefore there
   * shouldn't be an HTML &lt;label> tag, for instance.
   */
  protected boolean hasOwnLabel(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  protected boolean showAccessKeyOnLabel(
    UIComponent component,
    FacesBean   bean)
  {
    // By default, if we have our own label, don't show the
    // access key on the label (but that's not always true)
    return !hasOwnLabel(component, bean);
  }

  /**
   * Returns the ID (clientId) of the component that
   * should receive the label.
   */
  abstract protected String getLabelFor(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean);

  static void __setLabelWidth(
    RenderingContext rc,
    Object           width)
  {
    rc.getProperties().put(_LABEL_CELL_WIDTH_KEY, width);
  }

  static void __setFieldWidth(
    RenderingContext rc,
    Object           width)
  {
    rc.getProperties().put(_FIELD_CELL_WIDTH_KEY, width);
  }

  static void __clearProperties(
    RenderingContext rc)
  {
    rc.getProperties().remove(_LABEL_CELL_WIDTH_KEY);
    rc.getProperties().remove(_FIELD_CELL_WIDTH_KEY);
  }

  private class Label extends OutputLabelRenderer
  {
    public Label(
      FacesBean.Type type,
      boolean        inTable)
    {
      super(type);
      _inTable = inTable;
    }

    @Override
    protected boolean shouldRenderId(
      FacesContext context,
      UIComponent  component)
    {
      return false;
    }

    @Override
    protected boolean needComponentInStartElement()
    {
      // Because we're not rendering the ID on the label (at this time)
      // there's no point in passing the component to startElement() - it
      // just makes PPR unhappy to do so
      return false;
    }

    @Override
    protected void renderAllAttributes(
      FacesContext     context,
      RenderingContext rc,
      UIComponent      component,
      FacesBean        bean
      ) throws IOException
    {
      // Block everything
    }

    @Override
    protected String getDefaultValign(
      UIComponent component,
      FacesBean   bean)
    {
      // get the defaultLabelValign from the form component.
      return getDefaultLabelValign(component, bean);
    }

    @Override
    protected String getConvertedString(
      FacesContext context,
      UIComponent  component,
      FacesBean    bean)
    {
      if (_inTable)
        return null;

      return LabelAndMessageRenderer.this.getLabel(component, bean);
    }

    /**
     * Only display the required icon indicator if we're required
     * or showRequired is on.
     */
    @Override
    protected boolean getShowRequired(
      UIComponent component,
      FacesBean   bean)
    {
      // Inside the table, never show the required icon.
      if (_inTable)
        return false;

      return (LabelAndMessageRenderer.this.labelShowRequired(component, bean));
    }

    @Override
    protected char getAccessKey(
      UIComponent component,
      FacesBean   bean)
    {
      if (LabelAndMessageRenderer.this.showAccessKeyOnLabel(component, bean))
        return super.getAccessKey(component, bean);

      return CHAR_UNDEFINED;
    }

    @Override
    protected String getShortDesc(
      UIComponent component,
      FacesBean   bean)
    {
      String shortDesc = super.getShortDesc(component, bean);
      // =-=AEW Apparently, we're supposed to do this
      // for screenReader selectOneRadio and selectBooleanRadio!?!
      if ((shortDesc == null) && _inTable)
      {
        shortDesc = LabelAndMessageRenderer.this.getLabel(component, bean);
      }

      return shortDesc;
    }

    @Override
    protected String getForId(
      FacesContext context,
      UIComponent  component,
      FacesBean    bean)
    {
      return getLabelFor(context,
                         RenderingContext.getCurrentInstance(),
                         component,
                         bean);
    }

    @Override
    protected boolean isLabelTagNeeded(
      RenderingContext rc,
      UIComponent      component,
      FacesBean        bean,
      String           forId,
      int              accessKeyIndex
    )
    {
      if (LabelAndMessageRenderer.this.hasOwnLabel(component, bean))
        return false;

      return super.isLabelTagNeeded(rc, component, bean, forId, accessKeyIndex);
    }

    private final boolean _inTable;
  }

  private class Message extends MessageRenderer
  {
    public Message(
      FacesBean.Type type)
    {
      super(type);
    }

    @Override
    protected boolean shouldRenderId(
      FacesContext context,
      UIComponent  component)
    {
      return false;
    }

    @Override
    protected String getShortDesc(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected boolean getIndented(
      UIComponent component,
      FacesBean   bean)
    {
      return LabelAndMessageRenderer.this.isIndented();
    }

    @Override
    protected void renderAllAttributes(
      FacesContext     context,
      RenderingContext rc,
      UIComponent      component,
      FacesBean        bean
      ) throws IOException
    {
    }

    @Override
    protected String getForId(
      FacesContext context,
      UIComponent  component,
      FacesBean    bean)
    {
      return getLabelFor(context,
                         RenderingContext.getCurrentInstance(),
                         component,
                         bean);
    }
  }

  protected String getLabelInlineStyleKey(
    UIComponent component,
    FacesBean   bean)
  {
    return null; // overridden by PanelLabelAndMessageRenderer
  }

  /**
   * Hook for resolving whether we should show the "required" icon.
   */
  protected boolean labelShowRequired(
    UIComponent component,
    FacesBean   bean)
  {
    // If we're required (from the input side of things),
    // or showRequired is true, then show the "required" icon.
    // Unless we're read-only, in which case never show it.
    if (getRequired(component, bean) || getShowRequired(component, bean))
    {
      return isContentEditable(component, bean);
    }

    return false;
  }

  protected boolean getShowRequired(
    UIComponent component,
    FacesBean   bean)
  {
    if(_showRequiredKey == null)
    { // showRequired is not supporte on the element
      return false;
    }

    Object o = bean.getProperty(_showRequiredKey);
    if (o == null)
    {
      o = _showRequiredKey.getDefault();
    }

    return Boolean.TRUE.equals(o);
  }

  protected boolean getRequired(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_requiredKey);
    if (o == null)
      o = _requiredKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected String getLabel(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_labelKey));
  }

  /**
   * This gets the rootStyleClass from the bean, appends ::label to it,
   * @param bean
   * @return
   */
  protected String getLabelStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    String styleClass = getRootStyleClass(component, bean);
    if(styleClass != null)
    {
      styleClass += _LABEL_PSEUDO_ELEMENT;
    }

    return styleClass;
  }

  /* This method is responsible for rendering the End facet for narrow-screen
   * PDAs. In the case of narrow-screen PDAs, End facet is rendered after the
   * Help facet as shown below
   * +------+
   * |Label |
   * +------+
   * |Field |
   * +----------+
   * |Help facet|
   * +----------+
   * |End facet |
   * ------------
   * @param context a <code>FacesContext</code>
   * @param arc a <code>RenderingContext</code>
   * @param component a <code>UIComponent</code> the component to render
   * @param insideTableData a <code>boolean</code> indicates whether End
   *        Facet to be rendered is in inside a table data(<TD>)
   * @throws IOException if there are problems in rendering contents
   */

  protected void renderEndFacetForNarrowPDA(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    boolean          insideTableData)
    throws IOException
  {
    // Nothing by default. Applicable only for the
    // components that support End facet.
  }

  // If we have mapped this style (like in panelForm),
  // then return the style, otherwise return null
  private String _getDefaultLabelStyleClass(
    RenderingContext rc,
    String           styleClass)
  {
    Map<String, String> keyMap = rc.getSkinResourceKeyMap();
    return (keyMap != null) ?
            keyMap.get(styleClass) :
            null;
  }

  private boolean _isFormRendererType(String rendererType)
  {
    return "org.apache.myfaces.trinidad.Form".equals(rendererType) ||
        "org.apache.myfaces.trinidad.FormLayout".equals(rendererType) ||
        "org.apache.myfaces.trinidad.rich.Form".equals(rendererType) ||
        "org.apache.myfaces.trinidad.TableLayout".equals(rendererType);
  }


  // THESE VALUES MUST MATCH THOSE IN INLINEMESSAGERENDERER
  // (at least for as long as both classes exist)
  static private final Object _LABEL_CELL_WIDTH_KEY = "_imLCWidth";
  static private final Object _FIELD_CELL_WIDTH_KEY = "_imFCWidth";

  private static final String _LABEL_PSEUDO_ELEMENT = "::label";

  private PropertyKey   _labelKey;
  private PropertyKey   _requiredKey;
  private PropertyKey   _showRequiredKey;
  private XhtmlRenderer _message;
  private XhtmlRenderer _label;
  private XhtmlRenderer _labelInTable;
}
