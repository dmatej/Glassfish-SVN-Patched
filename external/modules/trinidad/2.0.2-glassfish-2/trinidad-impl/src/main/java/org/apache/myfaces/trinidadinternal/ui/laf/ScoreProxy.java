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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
/**
 * The Score subclass which is used to wrap another Score.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/ScoreProxy.java#0 $) $Date: 10-nov-2005.18:50:34 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ScoreProxy extends Score
{
  /**
   * Creates a ScoreProxy which wraps the specified "base" Score.
   * By default, all component scores will be pulled from the base Score.
   * Subclasses can modify individual component scores by overriding
   * one of the Score getter methods.
   */
  public ScoreProxy(Score baseScore)
  {
    if (baseScore == null) 
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_BASESCORE"));
    }

    _baseScore = baseScore;
  }

  /**
   * Returns the score for the look and feel family name.
   */
  @Override
  public int getNameScore()
  {
    return _baseScore.getNameScore();
  }

  /**
   * Returns the score for the Agent type.
   */
  @Override
  public int getAgentTypeScore()
  {
    return _baseScore.getAgentTypeScore();
  }

  /**
   * Returns the score for the Agent application.
   */
  @Override
  public int getAgentApplicationScore()
  {
    return _baseScore.getAgentApplicationScore();
  }

  /**
   * Returns the score for the Agent version.
   */
  @Override
  public int getAgentVersionScore()
  {
    return _baseScore.getAgentVersionScore();
  }

  /**
   * Returns the score for the Agent operating system.
   */
  @Override
  public int getAgentOSScore()
  {
    return _baseScore.getAgentOSScore();
  }

  /**
   * Returns a discriminant score that is used as a tie-breaker
   * when multiple LookAndFeels produce the same score.
   */
  @Override
  public int getDiscriminantScore()
  {
    return _baseScore.getDiscriminantScore();
  }

  private Score _baseScore;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ScoreProxy.class);
}

