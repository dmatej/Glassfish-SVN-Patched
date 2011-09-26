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

/**
 * Implementation of Score which provides storage for all component
 * scores.
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/ScoreImpl.java#0 $) $Date: 10-nov-2005.18:50:34 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ScoreImpl extends Score
{
  /**
   * Creates a ScoreImpl object based on the specified component scores.
   * <p>
   * Each component score must be one of the MATCH constants specified
   * above.
   *
   * @param nameScore The score for the look and feel family name
   * @param agentTypeScore The score for the Agent type
   * @param agentApplicationScore The score for the Agent application
   * @param agentVersionScore The score for the Agent version
   * @param agentOSScore The score for the Agent operating system
   * @param discriminantScore A discriminant score that is used to resolve
   *                          ties between multiple LookAndFeels.
   */
  public ScoreImpl(
    int nameScore,
    int agentTypeScore,
    int agentApplicationScore,
    int agentVersionScore,
    int agentOSScore,
    int discriminantScore
    )
  {
    _nameScore = nameScore;
    _agentTypeScore = agentTypeScore;
    _agentApplicationScore = agentApplicationScore;
    _agentVersionScore = agentVersionScore;
    _agentOSScore = agentOSScore;
    _discriminantScore = discriminantScore;
  }

  /**
   * Returns the score for the look and feel family name.
   */
  @Override
  public int getNameScore()
  {
    return _nameScore;
  }

  /**
   * Returns the score for the Agent type.
   */
  @Override
  public int getAgentTypeScore()
  {
    return _agentTypeScore;
  }

  /**
   * Returns the score for the Agent application.
   */
  @Override
  public int getAgentApplicationScore()
  {
    return _agentApplicationScore;
  }

  /**
   * Returns the score for the Agent version.
   */
  @Override
  public int getAgentVersionScore()
  {
    return _agentVersionScore;
  }

  /**
   * Returns the score for the Agent operating system.
   */
  @Override
  public int getAgentOSScore()
  {
    return _agentOSScore;
  }

  /**
   * Returns a discriminant score that is used as a tie-breaker
   * when multiple LookAndFeels produce the same score.
   */
  @Override
  public int getDiscriminantScore()
  {
    return _discriminantScore;
  }

  private final int _nameScore;
  private final int _agentTypeScore;
  private final int _agentApplicationScore;
  private final int _agentVersionScore;
  private final int _agentOSScore;
  private final int _discriminantScore;
}
