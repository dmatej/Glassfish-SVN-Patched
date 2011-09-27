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

import java.util.AbstractMap;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * The HelpProvider abstract class provides flexible mechanism for enabling
 * context-sensitive help from applications created with the UIX UI Components
 * or UIX language.  HelpProvider is a bean that defines
 * two standard Map and a set of standard keys.
 * <p>
 * Similar to most UI frameworks, context sensitive help in the UIX Framework
 * is topic-id based.  Developers use topic-ids in their pages and components,
 * and these topic-ids are passed to the help technology which resolves the
 * topic-id at runtime and displays the desired help topic.  In UIX, developers
 * will be able to use topic-ids as keys to the "helpTopic" Map when
 * data-binding the destination attribute of various components
 * (links, buttons, global buttons).  In addition, the developer will
 * be able to use the standard HelpProvider constant keys (like FRONT_PAGE_KEY)
 * when data-binding destinations using the "helpSystem" Map to
 * display navigation and system pages in the help system.
 * <p>
 * HelpProvider can be extended to support a variety of different help
 * technologies. Subclasses only need to implement the getHelpSystemValue()
 * method and the getHelpTopicValue() method.  Implementations can use
 * various schemes to create the value returned for a given topic-id or
 * system key (remember that the value will be used as a destination).
 * <p>
 * For example, a simple getHelpTopicValue() implementation would simply
 * map the topic-ID to the URL of a static webpage, and return that string URL
 * as the value.  A HelpProvider subclass for a web-based help system, like the
 * OracleHelpProvider, may choose to generate a URL off to a separate
 * servlet and simply pass the topic-id as a parameter.
 * <p>
 * Most HelpProvider subclasses will wish to return a javascript pseudo URL
 * that will launch a secondary window for displaying the help topic.  See
 * the SecondaryWindowHelpProvider abstract class for more information.
 *
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/el/HelpProvider.java#0 $) $Date: 10-nov-2005.19:06:18 $
 */
public abstract class HelpProvider
{
  public static final String FRONT_PAGE_KEY = "frontPage";

  public Map<String, Object> getHelpTopicMap()
  {
    return _topicMap;
  }

  public Map<String, Object> getHelpSystemMap()
  {
    return _systemMap;
  }

  /**
   * Subclasses of HelpProvider must implement this
   * method.  The key passed
   * as the selection criteria should be treated
   * as a topic-id, and the return value should be
   * a destination (URL string or javascript) for the
   * help topic page.
   * @param key the selection criteria (topic-id)
   * @return the value (should be string destination) for
   *         the selection criteria
   */
  protected abstract Object getHelpTopicValue(Object key);

  /**
   * Subclasses of HelpProvider must implement this method.
   * The key passed
   * as the selection criteria should be one of the
   * keys defined as HelpProvider constants, otherwise
   * implementations are free to return null.  The
   * return value should be a destination (URL string
   * or javascript) for the appropriate page in the
   * help system.
   * @param key the selection criteria (HelpProvider key)
   * @return the value (should be string destination) for
   *          the selection criteria
   */
  protected abstract Object getHelpSystemValue(Object key);

  private class HelpTopicMap extends AbstractMap<String, Object>
  {
    @Override
    public Set<Map.Entry<String, Object>> entrySet()
    {
      return Collections.emptySet();
    }

    @Override
    public Object get(Object key)
    {
      return getHelpTopicValue(key);
    }
  }

  private class HelpSystemMap extends AbstractMap<String, Object>
  {
    @Override
    public Set<Map.Entry<String, Object>> entrySet()
    {
      return Collections.emptySet();
    }

    @Override
    public Object get(Object key)
    {
      return getHelpSystemValue(key);
    }
  }

  private Map<String, Object> _topicMap = new HelpTopicMap();
  private Map<String, Object> _systemMap = new HelpSystemMap();
}

