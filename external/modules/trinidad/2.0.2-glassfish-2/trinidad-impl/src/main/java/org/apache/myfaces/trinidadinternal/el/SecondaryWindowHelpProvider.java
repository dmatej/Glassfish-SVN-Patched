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
package org.apache.myfaces.trinidadinternal.el;

import org.apache.myfaces.trinidad.util.IntegerUtils;

/**
 * <p>
 * Most HelpProvider implementations will wish to return a javascript
 * pseudo URL that will launch a secondary window for displaying help
 * topics and help system pages on user agents that support
 * JavaScript.  <p> This functionality is provided in the
 * SecondaryWindowHelpProvider abstract class.  Classes that extend
 * SecondaryWindowHelpProvider need only provide an implementation for
 * the getHelpTopicURL() and getHelpSystemURL() method.
 * SecondaryWindowHelpProvider will wrap the returned value in a
 * javascript pseudo URL if the user agent supports javascript, and
 * will return the string URL directly if the user agent does not.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/el/SecondaryWindowHelpProvider.java#0 $) $Date: 10-nov-2005.19:06:19 $
 */
public abstract class SecondaryWindowHelpProvider extends HelpProvider
{

  /**
   * The SecondaryWindowHelpProvider implementation of getHelpTopicValue()
   * wraps the value returned by getHelpTopicURL() with javascript code to
   * launch a secondary window (if the user agent supports javascript).
   * <p>
   * @param key criterion (topic-id)
   */
  @Override
  public Object getHelpTopicValue(Object key)
  {
    return _wrapURL(getHelpTopicURL(key));
  }

  /**
   * The SecondaryWindowHelpProvider implementation of getHelpSystemValue()
   * wraps the value returned by getHelpSystemURL() with javascript code to
   * launch a secondary window (if the user agent supports javascript).
   * <p>
   * @param key criterion (HelpProvider key constant)
   */
  @Override
  public Object getHelpSystemValue(Object key)
  {
    return _wrapURL(getHelpSystemURL(key));
  }

  /**
   * Returns the requested height of the secondary window
   *
   */
  public int getWindowHeight()
  {
    return _windowHeight;
  }

  /**
   * Set the requested height of the secondary window
   * <p>
   * @param windowHeight the new requested height
   */
  public void setWindowHeight(int windowHeight)
  {
    _windowHeight = windowHeight;
  }

  /**
   * Returns the requested width of the secondary window
   * <p>
   */
  public int getWindowWidth()
  {
    return _windowWidth;
  }

  /**
   * Set the requested width of the secondary window
   * <p>
   * @param windowWidth the new requested height
   */
  public void setWindowWidth(int windowWidth)
  {
    _windowWidth = windowWidth;
  }

  /**
   * The getHelpTopicURL() method should return a string URL
   * for the given key string (topic-id).
   * <p>
   * @param key criterion (topic-id)
   */
  protected abstract String getHelpTopicURL(Object key);

  /**
   * The getHelpSystemURL() method should return a string URL
   * for the given key string (HelpProvider System Key)
   * <p>
   * @param key criterion (HelpProvider System Key)
   */
  protected abstract String getHelpSystemURL(Object key);

  /**
   * @todo Override in subclass or implement for real here.
   */
  protected boolean isJavascriptSupported()
  {
    return true;
  }

  private String _wrapURL(String urlString)
  {
    if (!isJavascriptSupported())
      return urlString;

    if (urlString != null)
    {
      StringBuffer pseudoURL = new StringBuffer(200);
      pseudoURL.append("javascript:(new Function(\'a\', \'b\', \'c\',");
      pseudoURL.append(" \'d\', \'openWindow(a, b, c, d)\'))(top, \'");
      pseudoURL.append(urlString);
      pseudoURL.append("\',\'helpWindow\', {width:");
      pseudoURL.append(IntegerUtils.getString(getWindowWidth()));
      pseudoURL.append(", height:");
      pseudoURL.append(IntegerUtils.getString(getWindowHeight()));
      pseudoURL.append(", menubar:0, location:0, status:0, directories:0");
      pseudoURL.append("});");

      urlString = pseudoURL.toString();
    }

    return urlString;
  }


  public static final int DEFAULT_WINDOW_WIDTH = 650;
  public static final int DEFAULT_WINDOW_HEIGHT = 450;

  private int _windowWidth = DEFAULT_WINDOW_WIDTH;
  private int _windowHeight = DEFAULT_WINDOW_HEIGHT;
}

