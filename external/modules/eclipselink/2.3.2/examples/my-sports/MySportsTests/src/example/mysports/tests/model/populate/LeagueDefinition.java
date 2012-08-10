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

import java.util.Random;

import example.mysports.model.Player;

class LeagueDefinition {

    String leagueId;

    DivisionDefinition[] divisions;

    ExtensionDefinition[] extensions;

    LeagueDefinition(String leagueId, DivisionDefinition[] divisions, ExtensionDefinition[] extensions) {
        this.leagueId = leagueId;
        this.divisions = divisions;
        this.extensions = extensions;
    }

    String getLeagueId() {
        return leagueId;
    }

    DivisionDefinition[] getDivisions() {
        return divisions;
    }

    int getTotalPlayers() {
        int total = 0;

        for (DivisionDefinition divDef : getDivisions()) {
            total += divDef.getTotalPlayers();
        }
        return total;
    }

    ExtensionDefinition[] getExtensions() {
        return extensions;
    }

    static class ExtensionDefinition {
        String name;
        Class<?> type;
        Object[] values;
        int maxValue;

        ExtensionDefinition(String name, Class<?> type, Object[] values) {
            this.name = name;
            this.type = type;
            this.values = values;
        }

        ExtensionDefinition(String name, Class<?> type, int max) {
            this.name = name;
            this.type = type;
            this.values = null;
            this.maxValue = max;
        }

        String getName() {
            return name;
        }

        Class<?> getType() {
            return type;
        }

        Object[] getValues() {
            return values;
        }

        int getMaxValue() {
            return this.maxValue;
        }

        void populate(Player p, Random random) {
            if (getValues() == null) {
                p.set(getName(), random.nextInt(getMaxValue()));
            } else {
                p.set(getName(), getValues()[random.nextInt(getValues().length)]);
            }
        }
    }
}
