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
package example.mysports.tests.model.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import example.mysports.model.Player;


/**
 * Simple utility for generating players with random names.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class PlayerFactory {

    private static String[] FIRST_NAMES = new String[] { "John", "Ted", "Phil", "Jason", "Bill", "Jim", "Martin", "Sam", "Tim", "Rob" };

    private static String[] LAST_NAMES = new String[] { "Orr", "Smith", "Johnson", "Williams", "Jones", "Brown", "Davis", "Miller", "Wilson", "Moore", "Taylor" };

    public static Player createPlayer() {
        Random random = new Random();

        Player p = new Player();
        p.setNumber(random.nextInt(99) + 1);
        p.setFirstName(FIRST_NAMES[random.nextInt(FIRST_NAMES.length)]);
        p.setLastName(LAST_NAMES[random.nextInt(LAST_NAMES.length)]);
        return p;
    }

    public static List<Player> createPlayers(int num) {
        List<Player> players = new ArrayList<Player>();

        for (int index = 0; index < num; index++) {
            players.add(createPlayer());
        }

        return players;
    }

}
