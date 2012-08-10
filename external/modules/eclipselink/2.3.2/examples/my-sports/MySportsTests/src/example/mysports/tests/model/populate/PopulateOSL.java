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
public class PopulateOSL extends BasePopulateTest {

    public PopulateOSL() {
        super(new LeagueDefinition("OSL", 
                new DivisionDefinition[] { 
                    new DivisionDefinition("U4", new String[] { "Monkeys", "Zebras", "Oranges", "Pickles", "Pineapples" }, 10), 
                    new DivisionDefinition("U6", new String[] { "Scorpions", "Bears", "Dragons", "Gorillas", "Peaches" }, 11), 
                    new DivisionDefinition("U8", new String[] { "Blazers", "Gladiators", "Ducks", "Pirates", "Crusaders" }, 12) 
                    }, 
                    new ExtensionDefinition[] { new ExtensionDefinition("allergies", String.class, new String[] { "NONE", "NONE", "NONE", "PEANUT", "MILK" }) }));
    }

}
