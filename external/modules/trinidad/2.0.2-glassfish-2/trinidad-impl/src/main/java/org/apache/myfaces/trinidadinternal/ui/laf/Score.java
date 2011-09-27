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
 * The Score object is used by LookAndFeelScorer to report the results of 
 * the look and feel scoring process.
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/Score.java#0 $) $Date: 10-nov-2005.18:50:33 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class Score
{
  /**
   * Constant used to indicate that the value that is being tested is
   * not acceptable to the LookAndFeelScorer.  If any of the individual
   * component values (ie. name, agentType, agentApplication, etc...)
   * return a NO_MATCH score, the corresponding LookAndFeel will not
   * be used for the current request.
   */
  public static final int NO_MATCH = -1000000;

  /**
   * Constant used to indicate that the LookAndFeelScorer has no preference
   * for the value that is being tested, but that the value is acceptable.
   */
  public static final int DONT_CARE_MATCH = 70000;

  /**
   * Constant used to indicate that the value that is being tested is
   * acceptable to the LookAndFeelScorer, and that the match is stronger
   * than a DONT_CARE_MATCH.
   */
  public static final int COMPARISON_MATCH = 80000;

  /**
   * Constant used to indicate that the value that is being tested falls
   * within a range that is acceptable to the LookAndFeelScorer.  This
   * is a stronger match than either a DONT_CARE_MATCH or a 
   * COMPARISON_MATCH.
   */
  public static final int RANGE_MATCH = 90000;

  /**
   * Constant used to indicate that LookAndFeelScorer considers the
   * value that is being test to be an exact match.  This is the 
   * strongest possible match.
   */
  public static final int EXACT_MATCH = 100000;

  /**
   * Returns the score for the look and feel family name.
   */
  abstract public int getNameScore();

  /**
   * Returns the score for the Agent type.
   */
  abstract public int getAgentTypeScore();

  /**
   * Returns the score for the Agent application.
   */
  abstract public int getAgentApplicationScore();

  /**
   * Returns the score for the Agent version.
   */
  abstract public int getAgentVersionScore();

  /**
   * Returns the score for the Agent operating system.
   */
  abstract public int getAgentOSScore();

  /**
   * Returns a discriminant score that is used as a tie-breaker
   * when multiple LookAndFeels produce the same score.
   */
  abstract public int getDiscriminantScore();
}
