/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelCaptionGroup;
import org.apache.myfaces.trinidad.context.RenderingContext;


/**
 * @author Danny Robinson
 */
public class PanelCaptionGroupRenderer extends XhtmlRenderer
{
  public PanelCaptionGroupRenderer()
  {
    this(CorePanelCaptionGroup.TYPE);
  }

  protected PanelCaptionGroupRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _captionTextKey = type.findKey("captionText");
  }

  protected String getCaptionText(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(this.resolveProperty(bean, _captionTextKey));
  }

  protected String getRootStyle()
  {
    return SkinSelectors.AF_PANEL_CAPTION_GROUP_ROOT_STYLE_CLASS;
  }

  protected String getCaptionStyle()
  {
    return SkinSelectors.AF_PANEL_CAPTION_GROUP_CAPTION_STYLE_CLASS;
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.FIELDSET_ELEMENT, component);
    renderAllAttributes(context, rc, component, bean, false);
    renderStyleAttributes(context, rc, component, bean, getRootStyle());

    UIComponent captionFacet = getFacet(component, CorePanelCaptionGroup.CAPTION_FACET);
    String captionText = getCaptionText(component, bean);

    // Render either the caption facet or the captionText
    if (captionFacet != null || captionText != null)
    {
      writer.startElement(XhtmlConstants.LEGEND_ELEMENT, null);
      renderStyleClass(context, rc, getCaptionStyle());

      if (captionFacet != null)
        encodeChild(context, captionFacet);
      else
        writer.writeText(captionText, "captionText");

      writer.endElement(XhtmlConstants.LEGEND_ELEMENT);
    }

    // Output all the body of the component
    encodeAllChildren(context, component);

    writer.endElement(XhtmlConstants.FIELDSET_ELEMENT);
  }

  private PropertyKey _captionTextKey;
}
