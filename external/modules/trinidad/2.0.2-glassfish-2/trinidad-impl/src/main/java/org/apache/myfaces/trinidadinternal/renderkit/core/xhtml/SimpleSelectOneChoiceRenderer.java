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

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.model.SelectItem;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectOneChoice;
import org.apache.myfaces.trinidad.context.RenderingContext;


/**
 */
public class SimpleSelectOneChoiceRenderer extends SimpleSelectOneRenderer
{
  public SimpleSelectOneChoiceRenderer()
  {
    this(CoreSelectOneChoice.TYPE);
  }

  public SimpleSelectOneChoiceRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _unselectedLabelKey = type.findKey("unselectedLabel");
  }

  //
  // ENCODE BEHAVIOR
  //
  @Override
  protected void encodeElementContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    int              selectedIndex,
    Converter        converter,
    boolean          valuePassThru
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("select", component);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean, false);

    encodeSelectItems(context, rc, component, bean,
                      selectItems, selectedIndex, converter,
                      valuePassThru);

    writer.endElement("select");
  }

  protected void encodeSelectItems(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    int              selectedIndex,
    Converter        converter,
    boolean          valuePassThru
    ) throws IOException
  {
    int size = (selectItems == null) ? 0 : selectItems.size();

    String unselectedLabel = getUnselectedLabel(component, bean);

    // Figure out if we need a label for "nothing is selected".
    // This happens if:
    // (1) Nothing is selected, and there are actually some items
    //   present. (If there's an empty item and value is null,
    //   selectedIndex will already be pointing there)
    // (2) "unselectedLabel" is set.
    // =-=AEW PROBLEM WITH #1: if there wasn't anything selected, but
    // then something gets selected and the page is submitted,
    // this bonus value will disappear.  That's weird.  A user
    // can simply set "unselectedLabel" to avoid this problem.
    boolean needsUnselectedLabel = false;
    if ((size > 0) && (selectedIndex < 0))
      needsUnselectedLabel = true;
    else if (unselectedLabel != null)
      needsUnselectedLabel = true;

    if (needsUnselectedLabel)
    {
      if (unselectedLabel == null)
        unselectedLabel = "";
      SelectItem item = new SelectItem("", unselectedLabel, "", false);
      // @todo Restore the logic below
      encodeOption(context, rc, component, item, null, true, -1,
                   false /*(selectedIndex < 0)*/);
    }

    for (int i = 0; i < size; i++)
    {
      SelectItem item = selectItems.get(i);
      encodeOption(context, rc, component, item, converter,
                   valuePassThru, i, selectedIndex == i);
    }
  }

  /**
   * In Internet Explorer, handle autosubmit.
   */
  @Override
  protected String getOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    RenderingContext rc = RenderingContext.getCurrentInstance();
    String onclick = super.getOnclick(component, bean);

    //PH: onclick should be included only for desktop IE since PIE and
    //IE mobile do not support onclick on a select component
    if (isIE(rc) && isDesktop(rc) && isAutoSubmit(component, bean))
    {
      String auto = getAutoSubmitScript(rc, component,  bean);
      // On IE, if we autosubmit,
      // we get onChange events whenever a user keys through a SELECT
      // element. Therefore, we can't just fire in response to the
      // onChange event, we have to catch at least one change, then fire on the
      // next click or blur. The ACTION_HANDLER_PREFIX makes a call to a method
      // that returns true if state has changed. If there hasn't been a change,
      // the PREFIX will immediately return true, otherwise fall through. If we
      // allow the action script to follow, then the fall through will call it
      // only when there has been a change.
      auto = _IE_ACTION_HANDLER_PREFIX + auto;
      onclick = XhtmlUtils.getChainedJS(onclick, auto, true);
    }

    return onclick;
  }

  /**
   * In Internet Explorer, handle autosubmit.
   */
  @Override
  protected String getOnblur(
    UIComponent component,
    FacesBean   bean)
  {
    RenderingContext rc = RenderingContext.getCurrentInstance();
    String onblur = super.getOnblur(component, bean);

    //PH: onblur should be included only for desktop IE since PIE and
    //IE mobile do not support onblur on a select component
    if (isIE(rc) &&  isDesktop(rc) && isAutoSubmit(component, bean))
    {
      String auto = getAutoSubmitScript(rc, component, bean);
      // See getOnclick()
      auto = _IE_ACTION_HANDLER_PREFIX + auto;
      onblur = XhtmlUtils.getChainedJS(onblur, auto, true);
    }

    return onblur;
  }

  /**
   * Add autosubmit script, and autosync script.
   */
  @Override
  protected String getOnchange(
    UIComponent component,
    FacesBean   bean)
  {
    RenderingContext rc = RenderingContext.getCurrentInstance();

    String onchange = super.getOnchange(component, bean);
    String auto = null;
    // Prepend the autosubmit script
    if (isAutoSubmit(component, bean))
    {
       // See getOnclick() for the explanation of the IE code.
       // PH: The weird behaviour of an IE SELECT element as mentioned in
       // the getOnclick method above is not seen on PIE and IE Mobile.
       // Therefore, _CHOICE_CHANGE_TRACKER is not needed.
       // Also, since PIE and IE Mobile do not support an 'onclick' javascript
       // handler on a SELECT element,
       // autoSubmit script is added in the 'onchange' javascript handler.
      if (isIE(rc) && isDesktop(rc) )
        auto = _CHOICE_CHANGE_TRACKER;
      else
        auto = getAutoSubmitScript(rc, component, bean);
    }

    // And if we're in a repeating region, add in the synchronization
    // function
    if (_isRepeatingRegion(rc))
    {
      if (auto == null)
        auto = _SYNC_FUNC;
      else
        auto = (_SYNC_FUNC + ";") + auto;
    }

    // And chain everything together
    return XhtmlUtils.getChainedJS(onchange, auto, true);
  }

  @Override
  protected String getUnselectedLabel(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_unselectedLabelKey));
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|selectOneChoice::content";
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|selectOneChoice";
  }

  // Is this choice in a branch of the tree that is repeated:
  static private boolean _isRepeatingRegion(
    RenderingContext rc)
  {
    // check to make sure that repeating property is set, and that this choice
    // has a name (see bug 3194812):
    return (rc.getProperties().get(XhtmlConstants.REPEAT_PROPERTY) != null);
  }

  private PropertyKey _unselectedLabelKey;

  private static final String _SYNC_FUNC = "_syncChoiceIndex(this)";
  private static final String _IE_ACTION_HANDLER_PREFIX =
    "if(!_pprChoiceAction(event))return true;";
  private static final String _CHOICE_CHANGE_TRACKER =
    "return _pprChoiceChangeEvent(event);";
}
