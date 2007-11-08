/*
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */

package com.sun.s1peqe.ejb.cmp.roster.client;

import java.util.Iterator;
import java.util.ArrayList;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.rmi.PortableRemoteObject;
import com.sun.s1peqe.ejb.cmp.roster.ejb.*;
import com.sun.s1peqe.ejb.cmp.roster.util.*;
import com.sun.ejte.ccl.reporter.SimpleReporterAdapter;


public class RosterClient {
    
    private static SimpleReporterAdapter status =
        new SimpleReporterAdapter("appserv-tests");

    public static void main(String[] args) {
        try {
            status.addDescription("Testing cmp roster app.");
            Context initial = new InitialContext();
            Object objref = initial.lookup("java:comp/env/ejb/SimpleRoster");

            RosterHome home = 
                (RosterHome)PortableRemoteObject.narrow(objref, 
                                                        RosterHome.class);

            Roster myRoster = home.create();

            insertInfo(myRoster);
            getSomeInfo(myRoster);
            getMoreInfo(myRoster);

            status.printSummary("rosterAppID");
            System.exit(0);
        } catch (Exception ex) {
            System.err.println("Caught an exception in main: " + ex.toString());
            ex.printStackTrace();
        }
    }

    private static void getSomeInfo(Roster myRoster) {
        try {
            ArrayList playerList;
            ArrayList teamList;
            ArrayList leagueList;

            playerList = myRoster.getPlayersOfTeam("T2");
            printDetailsList(playerList);

            teamList = myRoster.getTeamsOfLeague("L1");
            printDetailsList(teamList);

            playerList = myRoster.getPlayersByPosition("defender");
            printDetailsList(playerList);

            leagueList = myRoster.getLeaguesOfPlayer("P28");
            printDetailsList(leagueList);
            status.addStatus("cmp roster:getSomeInfo", status.PASS);
        } catch (Exception ex) {
            status.addStatus("cmp roster:getSomeInfo", status.FAIL);
            System.err.println("Caught an exception in getSomeInfo: " +
                               ex.toString());
            ex.printStackTrace();
        }
    }

    private static void getMoreInfo(Roster myRoster) {
        try {
            LeagueDetails leagueDetails;
            TeamDetails teamDetails;
            PlayerDetails playerDetails;
            ArrayList playerList;
            ArrayList teamList;
            ArrayList leagueList;
            ArrayList sportList;

            leagueDetails = myRoster.getLeague("L1");
            System.out.println(leagueDetails.toString());
            System.out.println();

            teamDetails = myRoster.getTeam("T3");
            System.out.println(teamDetails.toString());
            System.out.println();

            playerDetails = myRoster.getPlayer("P20");
            System.out.println(playerDetails.toString());
            System.out.println();

            playerList = myRoster.getPlayersOfTeam("T2");
            printDetailsList(playerList);

            teamList = myRoster.getTeamsOfLeague("L1");
            printDetailsList(teamList);

            playerList = myRoster.getPlayersByPosition("defender");
            playerList = myRoster.getAllPlayers();
            playerList = myRoster.getPlayersNotOnTeam();
            playerList = myRoster.getPlayersByPositionAndName("power forward", 
                                                              "Jack Patterson");
            playerList = myRoster.getPlayersByCity("Truckee");
            playerList = myRoster.getPlayersBySport("Soccer");
            playerList = myRoster.getPlayersByLeagueId("L1");
            playerList = myRoster.getPlayersByHigherSalary("Ian Carlyle");
            playerList = myRoster.getPlayersBySalaryRange(500.00, 800.00);
            playerList = myRoster.getPlayersOfTeamCopy("T5");

            leagueList = myRoster.getLeaguesOfPlayer("P28");
            printDetailsList(leagueList);

            sportList = myRoster.getSportsOfPlayer("P28");
            printDetailsList(sportList);
            status.addStatus("cmp roster:getMoreInfo", status.PASS);
        } catch (Exception ex) {
            status.addStatus("cmp roster:getMoreInfo", status.FAIL);
            System.err.println("Caught an exception in getMoreInfo: " +
                               ex.toString());
            ex.printStackTrace();
        }
    }

    private static void printDetailsList(ArrayList list) {
        Iterator i = list.iterator();
        while (i.hasNext()) {
            Object details = (Object)i.next();
            System.out.println(details.toString());
        }
        System.out.println();
    }

    private static void insertInfo(Roster myRoster) {
        try {
            // Leagues
            myRoster.createLeague(new LeagueDetails(
                "L1", "Mountain", "Soccer"));

            myRoster.createLeague(new LeagueDetails(
                "L2", "Valley", "Basketball"));

            // Teams
            myRoster.createTeamInLeague(new TeamDetails(
                "T1", "Honey Bees", "Visalia"), "L1");

            myRoster.createTeamInLeague(new TeamDetails(
                "T2", "Gophers", "Manteca"), "L1");
           
            myRoster.createTeamInLeague(new TeamDetails(
                "T3", "Deer", "Bodie"), "L2");

            myRoster.createTeamInLeague(new TeamDetails(
                "T4", "Trout", "Truckee"), "L2");

            myRoster.createTeamInLeague(new TeamDetails(
                "T5", "Crows", "Orland"), "L1");

            // Players, Team T1
            myRoster.createPlayer(new PlayerDetails(
                "P1", "Phil Jones", "goalkeeper", 100.00));
            myRoster.addPlayer("P1", "T1");

            myRoster.createPlayer(new PlayerDetails(
                "P2", "Alice Smith", "defender", 505.00));
            myRoster.addPlayer("P2", "T1");

            myRoster.createPlayer(new PlayerDetails(
                "P3", "Bob Roberts", "midfielder", 65.00));
            myRoster.addPlayer("P3", "T1");

            myRoster.createPlayer(new PlayerDetails(
                "P4", "Grace Phillips", "forward", 100.00));
            myRoster.addPlayer("P4", "T1");

            myRoster.createPlayer(new PlayerDetails(
                "P5", "Barney Bold", "defender", 100.00));
            myRoster.addPlayer("P5", "T1");

            // Players, Team T2
            myRoster.createPlayer(new PlayerDetails(
                "P6", "Ian Carlyle", "goalkeeper", 555.00));
            myRoster.addPlayer("P6", "T2");

            myRoster.createPlayer(new PlayerDetails(
                "P7", "Rebecca Struthers", "midfielder", 777.00));
            myRoster.addPlayer("P7", "T2");

            myRoster.createPlayer(new PlayerDetails(
                "P8", "Anne Anderson", "forward", 65.00));
            myRoster.addPlayer("P8", "T2");

            myRoster.createPlayer(new PlayerDetails(
                "P9", "Jan Wesley", "defender", 100.00));
            myRoster.addPlayer("P9", "T2");

            myRoster.createPlayer(new PlayerDetails(
                "P10", "Terry Smithson", "midfielder", 100.00));
            myRoster.addPlayer("P10", "T2");

            // Players, Team T3
            myRoster.createPlayer(new PlayerDetails(
                "P11", "Ben Shore", "point guard", 188.00));
            myRoster.addPlayer("P11", "T3");

            myRoster.createPlayer(new PlayerDetails(
                "P12", "Chris Farley", "shooting guard", 577.00));
            myRoster.addPlayer("P12", "T3");

            myRoster.createPlayer(new PlayerDetails(
                "P13", "Audrey Brown", "small forward", 995.00));
            myRoster.addPlayer("P13", "T3");

            myRoster.createPlayer(new PlayerDetails(
                "P14", "Jack Patterson", "power forward", 100.00));
            myRoster.addPlayer("P14", "T3");

            myRoster.createPlayer(new PlayerDetails(
                "P15", "Candace Lewis", "point guard", 100.00));
            myRoster.addPlayer("P15", "T3");

            // Players, Team T4
            myRoster.createPlayer(new PlayerDetails(
                "P16", "Linda Berringer", "point guard", 844.00));
            myRoster.addPlayer("P16", "T4");

            myRoster.createPlayer(new PlayerDetails(
                "P17", "Bertrand Morris", "shooting guard", 452.00));
            myRoster.addPlayer("P17", "T4");

            myRoster.createPlayer(new PlayerDetails(
                "P18", "Nancy White", "small forward", 833.00));
            myRoster.addPlayer("P18", "T4");

            myRoster.createPlayer(new PlayerDetails(
                "P19", "Billy Black", "power forward", 444.00));
            myRoster.addPlayer("P19", "T4");

            myRoster.createPlayer(new PlayerDetails(
                "P20", "Jodie James", "point guard", 100.00));
            myRoster.addPlayer("P20", "T4");

            // Players, Team T5
            myRoster.createPlayer(new PlayerDetails(
                "P21", "Henry Shute", "goalkeeper", 205.00));
            myRoster.addPlayer("P21", "T5");

            myRoster.createPlayer(new PlayerDetails(
                "P22", "Janice Walker", "defender", 857.00));
            myRoster.addPlayer("P22", "T5");

            myRoster.createPlayer(new PlayerDetails(
                "P23", "Wally Hendricks", "midfielder", 748.00));
            myRoster.addPlayer("P23", "T5");

            myRoster.createPlayer(new PlayerDetails(
                "P24", "Gloria Garber", "forward", 777.00));
            myRoster.addPlayer("P24", "T5");

            myRoster.createPlayer(new PlayerDetails(
                "P25", "Frank Fletcher", "defender", 399.00));
            myRoster.addPlayer("P25", "T5");

            // Players, no team
            myRoster.createPlayer(new PlayerDetails(
                "P26", "Hobie Jackson", "pitcher", 582.00));
          
            myRoster.createPlayer(new PlayerDetails(
                "P27", "Melinda Kendall", "catcher", 677.00));

            // Players, multiple teams
            myRoster.createPlayer(new PlayerDetails(
                "P28", "Constance Adams", "substitue", 966.00));
            myRoster.addPlayer("P28", "T1");
            myRoster.addPlayer("P28", "T3");
            status.addStatus("cmp roster:insertInfo", status.PASS);
        } catch (Exception ex) {
            status.addStatus("cmp roster:insertInfo", status.FAIL);
            System.err.println("Caught an exception in insertInfo: " +
                               ex.toString());
            ex.printStackTrace();
        }
    }
}
