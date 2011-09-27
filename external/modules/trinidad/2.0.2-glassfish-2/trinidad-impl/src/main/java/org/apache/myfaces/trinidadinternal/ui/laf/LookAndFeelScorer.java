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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * The LookAndFeelScorer is responsible for producing a score that is
 * used in the LookAndFeel selection process.  A LookAndFeelScorer
 * is specified for each LookAndFeel that is registered with the
 * LookAndFeelManager.  When it is time to pick a LookAndFeel,
 * LookAndFeelScore.score() is called for each LookAndFeel.  The
 * LookAndFeel that produces the highest score is used to render
 * the response.
 *
 * @see org.apache.myfaces.trinidadinternal.ui.laf.Score
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/LookAndFeelScorer.java#0 $) $Date: 10-nov-2005.18:50:31 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class LookAndFeelScorer
{
  /**
   * @see org.apache.myfaces.trinidadinternal.ui.laf.Score#DONT_CARE_MATCH
   */
  public static final int DONT_CARE_MATCH = Score.DONT_CARE_MATCH;

  /**
   * @see org.apache.myfaces.trinidadinternal.ui.laf.Score#COMPARISON_MATCH
   */
  public static final int COMPARISON_MATCH = Score.COMPARISON_MATCH;

  /**
   * @see org.apache.myfaces.trinidadinternal.ui.laf.Score#RANGE_MATCH
   */
  public static final int RANGE_MATCH = Score.RANGE_MATCH;

  /**
   * @see org.apache.myfaces.trinidadinternal.ui.laf.Score#EXACT_MATCH
   */
  public static final int EXACT_MATCH = Score.EXACT_MATCH;

  /**
   * @see org.apache.myfaces.trinidadinternal.ui.laf.Score#NO_MATCH
   */
  public static final int NO_MATCH = Score.NO_MATCH;

  /**
   * Generates a Score which is used to determine whether the
   * LookAndFeel associated with this LookAndFeelScorer is the
   * best LookAndFeel to use for the current request.
   * 
   * @param context The RenderingContext for the current request.
   *                LookAndFeelScorers can use the RenderingContext
   *                to get at information which impacts the score,
   *                such as the Agent information.
   * @param lafName The preferred look and feel family name.
   *                This value may be null if there is no 
   *                preferred look and feel family.  In this case,
   *                LookAndFeelScorer implementations should return
   *                Score.DONT_CARE_MATCH for the name score.
   */
  public abstract Score scoreLookAndFeel(
    UIXRenderingContext context, 
    String lafName
    );
}
