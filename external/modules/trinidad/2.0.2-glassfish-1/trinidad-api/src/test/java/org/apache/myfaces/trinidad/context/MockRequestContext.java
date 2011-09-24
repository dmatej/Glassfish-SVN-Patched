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

import java.awt.Color;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.config.RegionManager;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;


public class MockRequestContext extends RequestContext
{
  public MockRequestContext()
  {
    attach();
  }

  // Support setting the agent so we can create one RequestContext
  // and mutate it

  public void setAgent(Agent agent)
  {
    _agent = agent;
  }

  @Override
  public Agent getAgent()
  {
    return _agent;
  }

  @Override
  public PageResolver getPageResolver()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public PageFlowScopeProvider getPageFlowScopeProvider()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public DialogService getDialogService()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Map<String, Object> getPageFlowScope()
  {
    return new HashMap<String, Object>();
  }

  @Override
  public void returnFromDialog(Object returnValue, Map<Object, Object> returnParam)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public void launchDialog(
      UIViewRoot dialogRoot,
      Map<String, Object> dialogParameters,
      UIComponent source,
      boolean useWindow,
      Map<String, Object> windowProperties)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public boolean isPostback()
  {
    return false;
  }

  @Override
  public boolean isPartialRequest(FacesContext context)
  {
    return false;
  }

  @Override
  public boolean isDebugOutput()
  {
    return false;
  }

  @Override
  public ClientValidation getClientValidation()
  {
    return ClientValidation.ALERT;
  }

  @Override
  public boolean isClientValidationDisabled()
  {
    return false;
  }

  @Override
  public String getOutputMode()
  {
    return null;
  }

  public void setSkinFamily(String skin)
  {
    _skin = skin;
  }

  @Override
  public String getSkinFamily()
  {
    return _skin;
  }

  @Override
  public Accessibility getAccessibilityMode()
  {
    return _accMode;
  }

  public void setAccessibilityMode(Accessibility accMode)
  {
    _accMode = accMode;
  }

  @Override
  public AccessibilityProfile getAccessibilityProfile()
  {
    return _accProfile;
  }

  public void setAccessibilityProfile(AccessibilityProfile accProfile)
  {
    _accProfile = accProfile;
  }

  @Override
  public boolean isRightToLeft()
  {
    return _rtl;
  }

  public void setRightToLeft(boolean rtl)
  {
    _rtl = rtl;
  }

  public void setAnimationEnabled(boolean animationEnabled)
  {
    _animationEnabled = animationEnabled;
  }

  @Override
  public boolean isAnimationEnabled()
  {
    return _animationEnabled;
  }

  @Override
  public Locale getFormattingLocale()
  {
    return _formattingLocale;
  }

  public void setFormattingLocale(Locale formattingLocale)
  {
    _formattingLocale = formattingLocale;
  }

  @Override
  public char getNumberGroupingSeparator()
  {
    return _numberGroupingSeparator;
  }

  public void setNumberGroupingSeparator(char sep)
  {
    _numberGroupingSeparator = sep;
  }

  @Override
  public char getDecimalSeparator()
  {
    return _decimalSeparator;
  }

  public void setDecimalSeparator(char sep)
  {
    _decimalSeparator = sep;
  }

  @Override
  public String getCurrencyCode()
  {
    return _currencyCode;
  }

  public void setCurrencyCode(String code)
  {
    _currencyCode = code;
  }

  @Override
  public int getTwoDigitYearStart()
  {
    return _twoDigitYearStart;
  }

  public void setTwoDigitYearStart(int start)
  {
    _twoDigitYearStart = start;
  }

  @Override
  public String getOracleHelpServletUrl()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Map<String, Object> getHelpTopic()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Map<String, Object> getHelpSystem()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public TimeZone getTimeZone()
  {
    return _timeZone;
  }

  public void setTimeZone(TimeZone timeZone)
  {
    _timeZone = timeZone;
  }

  public void setUploadedFileMaxMemory(Long maxMemory)
  {
    _maxMemory = maxMemory;
  }

  @Override
  public Long getUploadedFileMaxMemory()
  {
    return _maxMemory;
  }

  public void setUploadedFileMaxDiskSpace(Long maxDiskSpace)
  {
    _maxDiskSpace = maxDiskSpace;
  }

  @Override
  public Long getUploadedFileMaxDiskSpace()
  {
    return _maxDiskSpace;
  }

  public void setUploadedFileTempDir(String tempDir)
  {
    _tempDir= tempDir;
  }

  @Override
  public String getUploadedFileTempDir()
  {
    return _tempDir;
  }

  @Override
  public void addPartialTarget(UIComponent newTarget)
  {
    // throw new UnsupportedOperationException("Not implemented yet");
  }

  /**
   * @see org.apache.myfaces.trinidad.context.RequestContext#addPartialTargets(javax.faces.component.UIComponent, java.lang.String[])
   */
  @Override
  public void addPartialTargets(UIComponent from, String... targets)
  {

  }

  @Override
  public Set<UIComponent> getPartialTargets(UIComponent from)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public void addPartialTriggerListeners(UIComponent listener, String[] trigger)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public void partialUpdateNotify(UIComponent updated)
  {
    // Do nothing
  }

  @Override
  public UploadedFileProcessor getUploadedFileProcessor()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public Map<String, List<Color>> getColorPalette()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Map<Object, Map<Object,String>> getFormatter()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public ChangeManager getChangeManager()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public RegionManager getRegionManager()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Object saveComponent(UIComponent component)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }


  @Override
  public UIComponent restoreComponent(Object state)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public boolean isInternalViewRequest(FacesContext context)
  {
    return false;
  }

  @Override
  public Map<String, Object> getViewMap()
  {
    return getViewMap(true);
  }

  @Override
  @SuppressWarnings("unchecked")
  public Map<String, Object> getViewMap(boolean create)
  {
    // Note: replace this method body with a call to UIViewRoot.getViewMap(boolean) when
    // Trinidad is upgraded to use JSF 2.0

    FacesContext facesContext = FacesContext.getCurrentInstance();
    UIViewRoot viewRoot = facesContext.getViewRoot();
    Map<String, Object> viewMap = null;

    if (viewRoot != null)
    {
      Map<String, Object> attrs = viewRoot.getAttributes();

      viewMap = (Map<String, Object>)attrs.get(_VIEW_MAP_KEY);
      if (viewMap == null && create)
      {
        // Note, it is not valid to refer to the request context from outside of the request's
        // thread. As such, synchronization and thread safety is not an issue here.
        // This coincides with the JSF 2.0 code not using syncronization and using the non-thread
        // safe HashMap.
        viewMap = new HashMap<String, Object>();
        attrs.put(_VIEW_MAP_KEY, viewMap);
      }
    }

    return viewMap;
  }

  static private final TimeZone _FIXED_TIME_ZONE =
    TimeZone.getTimeZone("America/Los_Angeles");

  static private final String _VIEW_MAP_KEY =
    MockRequestContext.class.getName() + ".VIEW_MAP";

  private char _numberGroupingSeparator = ',';
  private char _decimalSeparator = '.';
  private String _currencyCode = null;
  private int _twoDigitYearStart = 1950;
  private TimeZone _timeZone = _FIXED_TIME_ZONE;

  private String _skin;
  private Accessibility _accMode;
  private AccessibilityProfile _accProfile;
  private Agent _agent;
  private boolean _rtl = false;
  private boolean _animationEnabled = true;
  private Locale _formattingLocale;
  private Long _maxMemory;
  private Long _maxDiskSpace;
  private String _tempDir;
}
