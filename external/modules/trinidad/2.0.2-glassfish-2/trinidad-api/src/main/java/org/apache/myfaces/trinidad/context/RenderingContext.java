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
package org.apache.myfaces.trinidad.context;

import java.util.Map;
import java.util.MissingResourceException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.style.Styles;
import org.apache.myfaces.trinidad.util.ThreadLocalUtils;

/**
 * The RenderingContext holds context information about the current rendering.
 * The RenderingContext is useful for Renderers. The 
 * RenderingContext is passed in to the CoreRenderer. For example, 
 * a renderer may need to render differently if it is rendering to a browser
 * in right-to-left mode, or if accessibility mode is on, etc.
 * Some values are promoted up from RequestContext for ease of use while rendering.
 * @see org.apache.myfaces.trinidad.render.CoreRenderer#encodeAll
 * @see RequestContext
 */
abstract public class RenderingContext
{
  /**
   * Retrieves the RenderingContext active for the current thread.
   */
  static public RenderingContext getCurrentInstance()
  {
    return _CURRENT_CONTEXT.get();
  }

  public RenderingContext()
  {
    attach();
  }

  /**
   * Get a map of properties specific to rendering. You can then call the
   * Map's put to save things on the RenderingContext for later retrieval.
   */
  abstract public Map<Object, Object> getProperties();
  /**
   * Get the Agent attached to the RenderingContext.
   * @return the Agent attached to the RenderingContext
   */
  abstract public Agent getAgent();
  
  /**
   * Get the LocaleContext attached to the RenderingContext
   * @return the LocaleContext attached to the RenderingContext
   */  
  abstract public LocaleContext getLocaleContext();

  /**
   * Get the FormData that is on the RenderingContext. You can then add more
   * form data once you have the FormData object. This is a way to render 
   * hidden fields form data.
   * @return returns the FormData that is on the RenderingContext. This could 
   * return null if there is no FormData
   */
  abstract public FormData getFormData();
  
  /**
   * Set FormData on the RenderingContext. This is a way to render hidden fields
   * form data.  This is a cleaner way to handle form data than using the
   * getProperties.put mechanism.
   * @param data
   */
  abstract public void setFormData(FormData data);
  
  /**
   * Clear the FormData that is on the RenderingContext
   */
  abstract public void clearFormData();

  //
  // Skin methods.
  //

  /**
   * Get the Skin that is attached to this RenderingContext.  
   * Icons, properties, etc. should never be retrieved directly
   * from the skin, but always through the RenderingContext so they
   * can be properly transformed.
   * @return the Skin object attached to this RenderingContext
   */
  abstract public Skin getSkin();

  /**
   * Get the translated String given a translation key.
   * The default implementation
   * gets the string from the Skin for the current LocaleContext
   * that is used during this rendering.
   * @param key
   * @return the translated String attached to this RenderingContext
   */
  public String getTranslatedString(String key)
  {
    if (key == null)
      return null;

    try
    {
      return getSkin().getTranslatedString(getLocaleContext(), key);
    }
    catch (MissingResourceException mre)
    {
      // Instead of halting execution, return "???<key>???",
      // just like JSF and JSTL will do, and log a severe error
      _LOG.severe("CANNOT_GET_RESOURCE_KEY", new String[]{key, getSkin().getId()});
      return "???" + key + "???";
    }
  }
  
  /**
   * Get the Icon attached to this RenderingContext, give the iconName.
   * This could be the Icon that is on the Skin objec that is on the 
   * current RenderingContext.
   * @param iconName
   * @return the Icon attached to this RenderingContext
   */
  abstract public Icon getIcon(String iconName);

  /**
   * Get a styleClass given a styleClass. With other information that is on
   * the RenderingContext, the styleClass may be transformed.
   * For example, you might want to return a compressed styleclass 
   * given the full styleClass.
   * e.g., getStyleClass("SomLongName") could return "x1"
   * You should always run your styleClass through this method before rendering
   * it out.
   * @param styleClass the styleClass name that you want to render 
   * @return the StyleClass that may be the given styleClass transformed to
   * be suitable for this rendering context.
   */
  abstract public String getStyleClass(String styleClass);
  
  /**
   *  Get the Styles object that is attached to this RenderingContext. 
   *  Styles is a useful object if you need to know the css properties for a given
   *  selector. (For backward compatibility this is not an abstract method.)
   * @return Styles or null if there are no Styles.
   * 
   */
  public Styles getStyles() 
  {
    return null;
  }
  
  /**
   * Set the skin resource key map on the RenderingContext. 
   * Store a Map that maps a skin's resource keys from one key to another 
   * on the RenderingContext. This way we can share Rendering code, but use
   * unique skinning keys for the Renderer. For example, we can use tree's 
   * rendering code for treeTable, but we want to be sure to use tree skinning
   * keys (aka selectors) for tree and treeTable skinning keys for the treeTable
   * component. You can create a map of tree's selectors to treeTable's selectors,
   * and then save off the current map (getSkinResourceKeyMap), 
   * set the new map, render using the tree's renderer code, then set back
   * the original map.
   * @param mapping
   * @see #getSkinResourceKeyMap()
   */
  abstract public void   setSkinResourceKeyMap(Map<String, String> mapping);
  
  /**
   * Get the skin resource key map that is attached to the RenderingContext.
   * This will usually be a Map of Skinning selectors. The key is the selector
   * that the renderer code is rendering, and the value is the selector that
   * that will be the actual selector to render. This facilitates sharing 
   * renderer code between components but render unique selectors for each
   * component. The convention is to have each component have their own unique
   * skinning selectors. Tree has af|tree selectors, treeTable has af|treeTable
   * selectors, for example.
   * @return a Map of selector to selector to faciliate renderer-code sharing.
   * @see #setSkinResourceKeyMap
   */
  abstract public Map<String, String> getSkinResourceKeyMap();
  
  /**
   * Returns a boolean to indicate whether or not the RenderingContext is 
   * in right-to-left reading direction mode. This can be true if the browser 
   * requesting  rendering is set to a right-to-left language.
   * @return true if right-to-left is true
   */
  abstract public boolean isRightToLeft();
  /**
   * Returns the output mode for this RenderingContext. Example output-modes 
   * are printable and portlet.
   * @return the outputMode for this RenderingContext.
   */
  abstract public String getOutputMode();
  
  /**
   * Get the accessibility mode (e.g., DEFAULT, INACCESSIBLE, etc)
   * that is attached to this RenderingContext
   * @return RequestContext.Accessibility enum attached to this RenderingContext
   */
  abstract public RequestContext.Accessibility getAccessibilityMode();
  
  /**
   * Get the AccessibilityProfile (a set of accessibility-related properties 
   * that are applied to the current request, e.g., color contrast, font size, etc) 
   * that is attached to this RenderingContext.
   * @return AccessibilityProfile attached to this RenderingContext
   */
  abstract public AccessibilityProfile getAccessibilityProfile();
  
  /**
   * Returns true or false for whether animation is enabled.
   * @return true if animation is enabled for this RenderingContext
   */
  abstract public boolean isAnimationEnabled();

  // TODO This is a hack API to enable caching of the client ID.
  // All fine, but we should have a more general mechanism.
  /**
   * Get the current client id for this RenderingContext
   * @return
   */
  public String getCurrentClientId() { return _currentClientId; }
  /**
   * Set the current client id for this RenderingContext
   * @param currentClientId
   */
  public void setCurrentClientId(String currentClientId)
  {
    _currentClientId = currentClientId;
  }

  private String _currentClientId;


  /**
   * Get the ParialPageContext that is attached to this RenderingContext
   * @return
   */
  abstract public PartialPageContext getPartialPageContext();


  public void release()
  {
    Object o = _CURRENT_CONTEXT.get();
    // Clean up first...
    _CURRENT_CONTEXT.remove();

    // Then see if there's a problem, and scream if there is one
    if (o == null)
      throw new IllegalStateException(_LOG.getMessage(
        "RENDERINGCONTEXT_ALREADY_RELEASED_OR_NEVER_ATTACHED"));
    if (o != this)
      throw new IllegalStateException(_LOG.getMessage(
        "TRY_RELEASING_DIFFERENT_RENDERINGCONTEXT"));
  }

  /**
   * Attaches an RenderingContext to the current thread.  This method is
   * protected, and therefore can only be called by an RenderingContext
   * object itself.
   */
  protected void attach()
  {
    Object o = _CURRENT_CONTEXT.get();
    // We want to catch two different problems:
    // (1) A failure to call release()
    // (2) An attempt to attach an instance when the thread already has one
    // For #1, anything more than a warning is dangerous, because throwing
    // an exception would permanently make the thread unusable.
    // For #2, I'd like to throw an exception, but I can't distinguish
    // this scenario from #1.
    if (o != null)
    {
      _LOG.warning("TRYING_ATTACH_RENDERERINGCONTEXT");
    }

    _CURRENT_CONTEXT.set(this);
  }


  static private final ThreadLocal<RenderingContext> _CURRENT_CONTEXT = 
                                                           ThreadLocalUtils.newRequestThreadLocal();  
  
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(RenderingContext.class);
}
