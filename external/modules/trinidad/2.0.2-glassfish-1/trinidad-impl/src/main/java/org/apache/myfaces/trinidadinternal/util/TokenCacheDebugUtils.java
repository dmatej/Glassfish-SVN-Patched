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
package org.apache.myfaces.trinidadinternal.util;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import javax.servlet.http.HttpSession;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.Window;
import org.apache.myfaces.trinidad.context.WindowManager;
    
 /**
  * ViewExpiredExceptions are fairly common, and the token cache is used for 
  * page state tokens, but the tokens aren't really human readable. 
  * In order to make it easier to understand what is in the cache
  * we've added a system property for debugging purposes. When enabled
  * we store a map of token -> viewId on the session which we use
  * to log something more human readable.
  * 
  * in order to use this the tester would set the system property to:
  * -Dorg.apache.myfaces.trinidadinternal.DEBUG_TOKEN_CACHE=true
  * 
  * TokenCacheDebugUtils provides methods to log human readable debugging info.
  * 
  * 
  * The goal is to provide as much logging information in a single log message as possible. 
  * In order to do this we are pushing log info into a buffer and when we're ready to log 
  * we can get the string. To start the log call like so:
  *        TokenCacheDebugUtils.startLog("My Custom String");
  * 
  * Then you can call methods like:
  *        TokenCacheDebugUtils.logCacheInfo(stateMap, null, "hello world"); 
  *        TokenCacheDebugUtils.addToLog("foo bar");
  *        
  * Then when you're ready to actually put it in the log you would do this:
  *        _LOG.severe(TokenCacheDebugUtils.getLogString());      
  */
public final class TokenCacheDebugUtils 
{
  
  private TokenCacheDebugUtils(){}


  /**
   * Checks whether we are debugging the token cache.
   * No other method in TokenCacheDebugUtils should be called unless this method returns true.
   */
  public static boolean debugTokenCache()
  {
    return _DEBUG_TOKEN_CACHE;
  }
  
  /**
   * In order to provide human readable information there is a map which 
   * has token to viewId information.
   * 
   * Add the token passed in and associate it in the map with the current view id. 
   * 
   * This method should only be called when debugTokenCache() is true
   */
  public static void addTokenToViewIdMap(String token)
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    FacesContext context = FacesContext.getCurrentInstance();
    Map<String,String> tokenToViewIdMap = _getTokenToViewIdMap(context);      
    UIViewRoot root = context.getViewRoot();
    String viewId = root.getViewId();    
    tokenToViewIdMap.put(token, viewId);

    StringBuffer logBuffer = _getLogBuffer(context);
    logBuffer.append("\nADDING ").append(_getTokenToViewIdString(tokenToViewIdMap, token));
  }

  /**
   * In order to provide human readable information there is a map which 
   * has token to viewId information.
   * 
   * Remove the view id info from the map for the given token.
   * 
   * This method should only be called when debugTokenCache() is true
   */
  public static void removeTokenFromViewIdMap(String token)
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);

    FacesContext context = FacesContext.getCurrentInstance();
    Map<String,String> tokenToViewIdMap = _getTokenToViewIdMap(context);  

    StringBuffer logBuffer = _getLogBuffer(context);
    logBuffer.append( "\nREMOVING ").append( _getTokenToViewIdString(tokenToViewIdMap, token));
    tokenToViewIdMap.remove(token);
  }  

  /**
   * Generate a string showing the token and the view id we have saved for that token
   * 
   * This method should only be called when debugTokenCache() is true
   */
  public static String getTokenToViewIdString(String token)
  {      
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    FacesContext context = FacesContext.getCurrentInstance();
    return _getTokenToViewIdString( _getTokenToViewIdMap(context), token);
  }


  /**
   * 
   * Add info about the cache to the log buffer.
   * 
   * For the target store map, the keys are tokens. For each token we will call 
   * getTokenToViewIdString to show what's in the map.
   * 
   * For the pinned map the keys and values are tokens, for each key/value pair call 
   * getTokenToViewIdString to show what is pinned.
   * 
   * This method should only be called when debugTokenCache() is true
   */
  public static <V> void logCacheInfo(Map<String, V> targetStore, Map<String, String> pinned, String logAddition)
  {  
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);

    FacesContext context = FacesContext.getCurrentInstance();
    Map<String,String> tokenToViewId = _getTokenToViewIdMap(context);
    // TODO - add this directly to the log buffer
    StringBuffer logString =  _getLogBuffer(context);
    logString.append("\n");

    if (logAddition != null)
    {
      logString.append(logAddition).append("\n");
    }
    
    logString.append("cached token keys:");
    
    for (String targetStoreToken: targetStore.keySet()) 
    {
      logString.append("\n    ");
      logString.append(_getTokenToViewIdString(tokenToViewId, targetStoreToken));
    }  
    
    if (pinned != null)
    {
      logString.append("\n_pinned token keys:");
      
      for (String pinnedKeyToken: pinned.keySet()) 
      {
        logString.append("\n    ");
        logString.append(_getTokenToViewIdString(tokenToViewId, pinnedKeyToken));
        
        logString.append("   pinned to     ");
        String pinnedValueToken = pinned.get(pinnedKeyToken);
        logString.append(_getTokenToViewIdString(tokenToViewId, pinnedValueToken));
      }
    }    
  }  


  /**
   * Add a string to the current log buffer
   * 
   * This method should only be called when debugTokenCache() is true
   */
  public static void addToLog(String addString)
  {     
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    FacesContext context = FacesContext.getCurrentInstance();
    _getLogBuffer(context).append(addString);
  }

  /**
   * Start a log buffer.
   * The startString passed in will be printed, along with session id and window id information
   * 
   * This method should only be called when debugTokenCache() is true
   */
  public static void startLog(String startString)
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    FacesContext context = FacesContext.getCurrentInstance();
    StringBuffer logBuffer = _getLogBuffer(context, true);
    logBuffer.append("-------------- ").append(startString).append(" ----------\n");
    _logIdString(context);    
  }

  
  /**
   * get the string from the log buffer.
   * 
   * This method should only be called when debugTokenCache() is true
   */
  public static String getLogString()
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    FacesContext context = FacesContext.getCurrentInstance();
    return _getLogBuffer(context).toString();
  }  

  private static StringBuffer _getLogBuffer(FacesContext context)
  { 
    return _getLogBuffer(context, false);
  }
  
  private static StringBuffer _getLogBuffer(FacesContext context, boolean startNewBuffer)
  { 
    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();   
    
    StringBuffer buff = null;
    
    if (!startNewBuffer)
    { 
      buff = (StringBuffer)requestMap.get(_STRINGBUFFER_KEY);
    }
    
    if ( buff == null)
    {
      buff = new StringBuffer();
      requestMap.put(_STRINGBUFFER_KEY, buff);
    }
    
    return buff;
  }  

  /**
   */
  private static void _logIdString(FacesContext context)
  {   
    ExternalContext externalContext = context.getExternalContext();    
    String sessionId = "";
    Object session = externalContext.getSession(false);
    
    if (session instanceof HttpSession)
    {
      sessionId = ((HttpSession)session).getId();
    }      

    StringBuffer buff = _getLogBuffer(context);
    buff.append("Session Id = ").append(sessionId);
    
    WindowManager wm = RequestContext.getCurrentInstance().getWindowManager();
    if (wm != null)
    {
      Window window = wm.getCurrentWindow(externalContext);
      
      if (window != null)
      {
        buff.append("\nWindow Id = ").append(window.getId());   
      }
      else
      {
        buff.append("\nWindow Id could not be determined, window is null" ); 
      }
        
    }
    else
    {
      buff.append("\nWindow Id could not be determined, window manager null" ); 
    }
  }  
  

  /**
   */
  private static Map<String,String> _getTokenToViewIdMap(FacesContext context)
  {    
    Map<String,String> tokenToViewId = (Map<String, String>)context.getExternalContext().getSessionMap().get("org.apache.myfaces.trinidadinternal.util.TOKEN_FOR_VIEW_ID");
    
    if (tokenToViewId == null) 
    {
      tokenToViewId = new ConcurrentHashMap<String, String>();
      context.getExternalContext().getSessionMap().put("org.apache.myfaces.trinidadinternal.util.TOKEN_FOR_VIEW_ID", tokenToViewId);
      
    }
    return tokenToViewId;
  }  


  /**
   */
  private static String _getTokenToViewIdString(Map<String,String> tokenToViewId, String token)
  {      
    StringBuffer tokenBuffer = new StringBuffer();
    tokenBuffer.append(token);
    tokenBuffer.append(" (");
    tokenBuffer.append(tokenToViewId.get(token));
    tokenBuffer.append(")");
    
    return tokenBuffer.toString();
  }
  
  private static final String _UNSUPPORTED_OPERATION_MESSAGE =  
           "Methods in TokenCacheDebugUtils can only be called when " +
           "TokenCacheDebugUtils.debugTokenCache() returns true. " + 
           "TokenCacheDebugUtils.debugTokenCache() returns true when the system property " +
           "'org.apache.myfaces.trinidadinternal.DEBUG_TOKEN_CACHE' is true";


  private static final String _STRINGBUFFER_KEY = TokenCacheDebugUtils.class.getName() + "#StringBuffer";
    

  static private final boolean _DEBUG_TOKEN_CACHE;
  static
  {
    String dtcProp = System.getProperty("org.apache.myfaces.trinidadinternal.DEBUG_TOKEN_CACHE");
    _DEBUG_TOKEN_CACHE = Boolean.valueOf(dtcProp);  
  }
}
