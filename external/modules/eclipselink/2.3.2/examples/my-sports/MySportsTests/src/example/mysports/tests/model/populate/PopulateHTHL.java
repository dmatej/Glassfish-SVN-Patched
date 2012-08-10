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
 * 
 * @author dclarke
 */
public class PopulateHTHL extends BasePopulateTest {

    public PopulateHTHL() {
        super(new LeagueDefinition("HTHL", 
              new DivisionDefinition[] { 
                new DivisionDefinition("Div_1", new String[] { "Aces", "Leafs", "Pucks", "Meteors", "Rockets", "Jets", "Tigers", "Barons" }, 12), 
                new DivisionDefinition("Div_2", new String[] { "Slapshots", "Dragons", "Fury", "Cougars", "Flyers", "Lizards" }, 10), },
              new ExtensionDefinition[] {
                new ExtensionDefinition("position", String.class, new String[] { "R", "L", "C", "D" }),
                new ExtensionDefinition("penaltyMinutes", Integer.class, 100)}));
    }

}
