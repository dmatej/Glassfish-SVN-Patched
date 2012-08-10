/*******************************************************************************
 * Copyright (c) 2010-2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *  dclarke - EclipseLink 2.3 - MySports Demo Bug 344608
 ******************************************************************************/
package example.mysports.tests.model.populate;

import example.mysports.tests.model.populate.LeagueDefinition.ExtensionDefinition;

/**
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class PopulateKFL extends BasePopulateTest {

    public PopulateKFL() {
        super(new LeagueDefinition("KFL", 
                new DivisionDefinition[] { 
                    new DivisionDefinition("Mite", new String[] { "Scorpions", "Bears", "Dragons", "Gorillas", "Peaches" }, 26), 
                    new DivisionDefinition("Tyke", new String[] { "Blazers", "Gladiators", "Ducks", "Pirates", "Crusaders" }, 28), 
                    new DivisionDefinition("Mosquito", new String[] { "Riders", "XMen", "Ravens", "Wolverines", "Giants", "Redskins" }, 30) }, 
                new ExtensionDefinition[] { 
                    new ExtensionDefinition("position", String.class, new String[] { "Center", "Offensive Guard", "Offensive Tackle", "Tight End", "Wide Receiver", "Full Back", "Running Back", "Quarter Back", "Defensive End", "Defensive Tackle", "Nose Guard", "Linebacker", "Cornerback", "Safety" })}));
    }

}
