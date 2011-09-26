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
package org.apache.myfaces.trinidadinternal.ui.laf;

import java.util.ArrayList;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIExtension;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.BaseDesktopUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.simple.desktop.SimpleDesktopUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.simple.pda.SimplePdaUtils;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Manages the set of LookAndFeel instances.  LookAndFeel instances can
 * be registered per-Agent basis.
 *
 * @see LookAndFeel
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/LookAndFeelManager.java#1 $) $Date: 07-dec-2005.15:27:47 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class LookAndFeelManager
{
  /**
   * Returns the shared instanceof of LookAndFeelManager.
   */
  static public LookAndFeelManager getDefaultLookAndFeelManager()
  {
    if (_sDefaultInstance == null)
      _sDefaultInstance = createDefaultLookAndFeelManager();

    return _sDefaultInstance;
  }

  /**
   * Creates a LookAndFeelManager with default LookAndFeels registered.
   */
  static public LookAndFeelManager createDefaultLookAndFeelManager()
  {
    LookAndFeelManager manager = new LookAndFeelManager();

    /* =-=AEW DO NOT register the external look-and-feels;  these
       class names are UIX 2.2 look-and-feels, which won't (and can't)
       work in UIX 3, given the different class names.  Only uncomment
       if and when such classes are ported to UIX 3.
    // support requests from iasWireless if laf can be found
    _registerExternalLookAndFeel(manager, iaswLaf, _IASW_SCORER);

    // register OA's Text LAF if laf can be found
    _registerExternalLookAndFeel(manager, _OA_TEXT_LAF, _OA_TEXT_SCORER);
    */

    // Base lafs
    BaseDesktopUtils.registerLookAndFeel(manager);

    SimpleDesktopUtils.registerLookAndFeel(manager);
    SimplePdaUtils.registerLookAndFeel(manager);

    return manager;
  }


  /**
   * Gets the LookAndFeel for the specified RenderingContext.
   */
  public LookAndFeel getLookAndFeel(
    UIXRenderingContext context
    )
  {
    synchronized (_scorersAndLafs)
    {
      int entryCount = _scorersAndLafs.size();

      int maxEntryIndex = -1;

      // Start off with at least NO_MATCH + 1 so that we
      // only match LAFs which score greater than NO_MATCH
      int maxEntryScore = LookAndFeelScorer.NO_MATCH + 1;

      String lafName = "minimal"; 
      for (int i = 0; i < entryCount; i += 2)
      {
        LookAndFeelScorer currScorer = (LookAndFeelScorer)
                                       _scorersAndLafs.get(i);

        int currScore = _score(context, lafName, currScorer);

        // keep the laf with the highest scores, with ties going to the
        // last registered laf
        if (currScore >= maxEntryScore)
        {
          maxEntryScore = currScore;
          maxEntryIndex = i;
        }
      }

      if (maxEntryIndex < 0)
      {
        //
        // try again with no lafname
        //
        for (int i = 0; i < entryCount; i += 2)
        {
          LookAndFeelScorer currScorer = (LookAndFeelScorer)
                                         _scorersAndLafs.get(i);

          int currScore = _score(context, null, currScorer);

          // keep the laf with the highest scores, with ties going to the
          // last registered laf
          if (currScore >= maxEntryScore)
          {
            maxEntryScore = currScore;
            maxEntryIndex = i;
          }
        }
      }

      // return the entry with the highest score
      if (maxEntryIndex >= 0)
      {
        return (LookAndFeel)_scorersAndLafs.get(maxEntryIndex + 1);
      }
      else
      {
        return null;
      }
    }
  }

  /**
   * Returns the LookAndFeel instance with the specified id.
   * If no LookAndFeel with the specified id has been registered
   * with the LookAndFeelManager, returns null.
   *
   * @param id A non-null string which represents the id of a LookAndFeel
   * @see org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel#getId
   */
  public LookAndFeel getLookAndFeelById(String id)
  {
    if (id == null) 
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_ID"));
    }

    synchronized (_scorersAndLafs)
    {
      int entryCount = _scorersAndLafs.size();

      for (int i = 1; i < entryCount; i += 2)
      {
        LookAndFeel laf = (LookAndFeel)_scorersAndLafs.get(i);
        if (id.equals(laf.getId()))
          return laf;
      }
    }

    return null;
  }

  /**
   * Returns the LookAndFeelScorer for the specified LookAndFeel.
   */
  public LookAndFeelScorer getLookAndFeelScorer(LookAndFeel laf)
  {
    synchronized (_scorersAndLafs)
    {
      int entryCount = _scorersAndLafs.size();

      for (int i = 0; i < entryCount; i += 2)
      {
        Object currLaf = _scorersAndLafs.get(i + 1);

        if (currLaf == laf)
          return (LookAndFeelScorer)_scorersAndLafs.get(i);
      }
    }

    return null;
  }

  /**
   * Registers the specified LookAndFeel to be used with the specified
   * scorer.
   */
  public void registerLookAndFeel(
    LookAndFeelScorer scorer,
    LookAndFeel       laf
    )
  {
    if ((scorer == null) || (laf == null))
      throw new IllegalArgumentException();

    synchronized(_scorersAndLafs)
    {
      _scorersAndLafs.add(scorer);
      _scorersAndLafs.add(laf);

      if (!_lafs.contains(laf))
        _lafs.add(laf);

      // Register all extensions
      int extensionCount = _extensions.size();
      for (int i = 0; i < extensionCount; i++)
      {
        UIExtension extension = _extensions.get(i);
        extension.registerSelf(laf);
      }
    }
  }

  /**
   * Unregisters the LookAndFeel for the specified scorer.
   */
  public void unregisterLookAndFeel(
    LookAndFeelScorer scorer
    )
  {
    synchronized(_scorersAndLafs)
    {
      int index = _scorersAndLafs.indexOf(scorer);

      if (index >= 0)
      {
        _scorersAndLafs.remove(index + 1);
        _scorersAndLafs.remove(index);
      }
    }
  }


  /**
   * Adds a UIExtension to this manager.  The extension will automatically
   * be registered with all LookAndFeels associated with this manager.
   */
  public void registerUIExtension(UIExtension extension)
  {
    synchronized (_scorersAndLafs)
    {
      if (!_extensions.contains(extension))
      {
        _extensions.add(extension);

        // Register the extension on all existing LookAndFeels
        int lafCount = _lafs.size();

        for (int i = 0; i < lafCount; i++)
        {
          LookAndFeel laf = _lafs.get(i);
          extension.registerSelf(laf);
        }
      }
    }
  }


  // Computes the total score
  static private int _score(
    UIXRenderingContext  context,
    String            lafName,
    LookAndFeelScorer scorer
    )
  {
    Score score = scorer.scoreLookAndFeel(context, lafName);

    // First, check for any NO_MATCH scores.
    int nameScore = score.getNameScore();
    int agentTypeScore = score.getAgentTypeScore();
    int agentApplicationScore = score.getAgentApplicationScore();
    int agentVersionScore = score.getAgentVersionScore();
    int agentOSScore = score.getAgentOSScore();
    int discriminantScore = score.getDiscriminantScore();

    // If any of the component scores are NO_MATCH, then we
    // don't have a match - return NO_MATCH.
    if ((nameScore == Score.NO_MATCH)             ||
        (agentTypeScore == Score.NO_MATCH)        ||
        (agentApplicationScore == Score.NO_MATCH) ||
        (agentVersionScore == Score.NO_MATCH)     ||
        (agentOSScore == Score.NO_MATCH)          ||
        (discriminantScore == Score.NO_MATCH))
    {
      return Score.NO_MATCH;
    }

    return (nameScore             +
            agentTypeScore        +
            agentApplicationScore +
            agentVersionScore     +
            agentOSScore          +
            discriminantScore);
  }

  // Keep this private for now
  private LookAndFeelManager() {}

  private ArrayList<Object>      _scorersAndLafs = new ArrayList<Object>(20);
  private ArrayList<UIExtension> _extensions     = new ArrayList<UIExtension>();

  // A list (really, a set, but there's no ArraySet) of look-and-feels;
  // this does not contain duplicates, while _scorersAndLafs may
  private ArrayList<LookAndFeel> _lafs           = new ArrayList<LookAndFeel>();

  private static LookAndFeelManager _sDefaultInstance;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    LookAndFeelManager.class);
}
