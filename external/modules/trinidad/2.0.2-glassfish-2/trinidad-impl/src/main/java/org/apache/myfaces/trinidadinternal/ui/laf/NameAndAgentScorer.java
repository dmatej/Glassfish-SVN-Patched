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

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * Scores a LAF based on the Laf name and the agent information.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/NameAndAgentScorer.java#0 $) $Date: 10-nov-2005.18:50:32 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class NameAndAgentScorer extends LookAndFeelScorer
{
  /**
   * Create a Scorer for scoring look and feesl based on the name
   * and agent information.  If a value is supplied for a parameter,
   * the agent or name must be equal to that value in order for
   * the scorer to create a permissable score.  A null value for a
   * parameter means that the scorer doesn't care about the value.
   */
  public NameAndAgentScorer(
    String  requiredLafName,
    Integer requiredAgentApplication,
    Integer requiredAgentMajorVersion,
    Integer requiredAgentOS,
    Integer... requiredAgentTypes
    )
  {
    _lafName           = requiredLafName;
    _agentTypes        = requiredAgentTypes;
    _agentApplication  = requiredAgentApplication;
    _agentMajorVersion = requiredAgentMajorVersion;
    _agentOS           = requiredAgentOS;
  }

  /**
   * Implementation of LookAndFeelScorer that produces a Score
   * based on the required name and Agent information.
   */
  @Override
  public Score scoreLookAndFeel(
    UIXRenderingContext context, 
    String lafName
    )
  {
    // Just for old times sake, try calling score() and check
    // for a NO_MATCH result.  We do this just in case someone
    // has subclassed NameAndAgentScorer and overridden score()
    // to return NO_MATCH for a particular Agent - like the
    // minimal desktop LAF was doing to indicate that Netscape 4
    // wasn't supported.
    int deprecatedScore = score(context, lafName);
    if (deprecatedScore == NO_MATCH)
      return _NO_MATCH_SCORE;

    int nameScore = _scoreName(lafName);

    TrinidadAgent agent = context. getAgent();
    
    int typeScore = NO_MATCH;
    int agentType = agent.getAgentType();
    for (int type: _agentTypes)
    {
      typeScore = Math.max(typeScore, _score(type, agentType));
    }
    
    int appScore = _score(_agentApplication, agent.getAgentApplication().ordinal());
    int versScore = _score(_agentMajorVersion, agent.getAgentMajorVersion());
    int osScore = _score(_agentOS, agent.getAgentOS());

    return new ScoreImpl(nameScore,
                         typeScore,
                         appScore,
                         versScore,
                         osScore,
                         DONT_CARE_MATCH);
  }
  
  /**
   * @deprecated Subclassers should override scoreLookAndFeel() instead.
   */
  @Deprecated
  public int score(
    UIXRenderingContext context,
    String           lafName
    )
  {
    return DONT_CARE_MATCH;
  }

  // Scores the family name
  private int _scoreName(String lafName)
  {
    if ((lafName == null) || (_lafName == null))
      return DONT_CARE_MATCH;

    return (lafName.equals(_lafName)) ? EXACT_MATCH : NO_MATCH;
  }

  // Scores the Integer-based Agent properties
  private int _score(Integer requiredValue, int actualValue)
  {
    // If we don't have a required value, we don't care
    if (requiredValue == null)
      return DONT_CARE_MATCH;

    if (requiredValue.intValue() == actualValue)
      return EXACT_MATCH;

    return NO_MATCH;
  }
  
  private String  _lafName;
  private Integer _agentTypes[];
  private Integer _agentApplication;
  private Integer _agentMajorVersion;
  private Integer _agentOS;

  // The Score object that return if the old score() method returns
  // NO_MATCH.
  private static final Score _NO_MATCH_SCORE = new ScoreImpl(NO_MATCH,
                                                             NO_MATCH,
                                                             NO_MATCH,
                                                             NO_MATCH,
                                                             NO_MATCH,
                                                             NO_MATCH);
}
