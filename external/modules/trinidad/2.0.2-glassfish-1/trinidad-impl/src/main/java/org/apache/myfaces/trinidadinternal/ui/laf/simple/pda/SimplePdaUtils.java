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
package org.apache.myfaces.trinidadinternal.ui.laf.simple.pda;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager;
import org.apache.myfaces.trinidadinternal.ui.laf.NameAndAgentScorer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.pda.PdaHtmlLafUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/pda/SimplePdaUtils.java#0 $) $Date: 10-nov-2005.18:50:47 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SimplePdaUtils extends PdaHtmlLafUtils
{

  private SimplePdaUtils()
  {
  }

  /**
   * Registers the Pocket PC Look And Feel with the specified
   * LookAndFeelManager.
   */
  public static void registerLookAndFeel(
    LookAndFeelManager manager
    )
  {
    SimplePdaLookAndFeel laf = new SimplePdaLookAndFeel();
    manager.registerLookAndFeel(_PDA_SCORER, laf);
  }

  private static final NameAndAgentScorer _PDA_SCORER =
    new NameAndAgentScorer("simple",
                           null,
                           null,
                           null,
                           TrinidadAgent.TYPE_PDA);


}
