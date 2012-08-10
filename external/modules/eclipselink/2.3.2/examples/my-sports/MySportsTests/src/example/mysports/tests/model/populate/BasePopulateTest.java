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

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javax.persistence.EntityManager;
import javax.persistence.metamodel.Attribute;
import javax.persistence.metamodel.ManagedType;
import javax.persistence.metamodel.Metamodel;

import junit.framework.Assert;

import org.junit.Test;

import example.mysports.model.Division;
import example.mysports.model.Player;
import example.mysports.model.Team;
import example.mysports.tests.model.populate.LeagueDefinition.ExtensionDefinition;
import example.mysports.tests.model.util.BaseLeagueTest;
import example.mysports.tests.model.util.PlayerFactory;

public abstract class BasePopulateTest extends BaseLeagueTest {

    private LeagueDefinition leagueDefinition;

    public BasePopulateTest(LeagueDefinition leagueDefinition) {
        this.leagueDefinition = leagueDefinition;
    }

    public LeagueDefinition getLeagueDefinition() {
        return leagueDefinition;
    }

    @Override
    protected String getLeagueId() {
        return getLeagueDefinition().getLeagueId();
    }

    @Test
    public void populate() {
        EntityManager em = getEMF().createEntityManager();

        try {
            em.getTransaction().begin();
            em.createQuery("DELETE FROM Player").executeUpdate();
            em.createQuery("DELETE FROM Team").executeUpdate();
            em.createQuery("DELETE FROM Division").executeUpdate();
            em.getTransaction().commit();

            Assert.assertEquals(0, em.createQuery("SELECT COUNT(p) FROM Player p", Number.class).getSingleResult().intValue());
            Assert.assertEquals(0, em.createQuery("SELECT COUNT(t) FROM Team t", Number.class).getSingleResult().intValue());
            Assert.assertEquals(0, em.createQuery("SELECT COUNT(d) FROM Division d", Number.class).getSingleResult().intValue());
        } finally {
            em.close();
        }

        for (DivisionDefinition divDef : getLeagueDefinition().getDivisions()) {
            Division div = getRepository().addDivision(divDef.getName());

            Assert.assertNotNull(div);
            for (int index = 0; index < divDef.getTeamNames().length; index++) {
                getRepository().addTeam(divDef.getTeamNames()[index], createPlayers(divDef.getNumPlayers()), div);
            }
        }

        getEMF().getCache().evictAll();
    }

    private List<Player> createPlayers(int num) {
        List<Player> players = new ArrayList<Player>();
        Random random = new Random();

        for (int index = 0; index < num; index++) {
            Player p = PlayerFactory.createPlayer();
            for (ExtensionDefinition extDef : getLeagueDefinition().getExtensions()) {
                extDef.populate(p, random);
            }
            players.add(p);
        }

        return players;
    }

    /**
     * Validate extension definitions using JPA2 metamodel
     */
    @Test
    public void validateExtensions() {
        Metamodel metamodel = getEMF().getMetamodel();

        ManagedType<Player> playerType = metamodel.managedType(Player.class);
        Assert.assertNotNull(playerType);

        for (ExtensionDefinition extDef : getLeagueDefinition().getExtensions()) {
            Attribute<Player, ?> attr = playerType.getDeclaredAttribute(extDef.getName());

            Assert.assertNotNull(attr);
            Assert.assertEquals(extDef.getType(), attr.getJavaType());
        }
    }

    @Test
    public void validateGetDivisions() {
        List<Division> divisions = getRepository().getDivisions();

        Assert.assertEquals(getLeagueDefinition().getDivisions().length, divisions.size());
    }

    @Test
    public void validateQueryingTeams() {
        EntityManager em = getEMF().createEntityManager();

        try {
            for (DivisionDefinition divDef : getLeagueDefinition().getDivisions()) {
                List<Team> teams = em.createQuery("SELECT t FROM Team t WHERE t.division.name = :DIV", Team.class).setParameter("DIV", divDef.getName()).getResultList();
                Assert.assertNotNull(teams);
                Assert.assertEquals(divDef.getTeamNames().length, teams.size());
            }
        } finally {
            em.close();
        }
    }

    @Test
    public void validateAllPlayers() {
        EntityManager em = getEMF().createEntityManager();

        try {
            List<Player> allPlayers = em.createQuery("SELECT p FROM Player p", Player.class).getResultList();
            Assert.assertNotNull(allPlayers);
            Assert.assertEquals(getLeagueDefinition().getTotalPlayers(), allPlayers.size());
        } finally {
            em.close();
        }
    }

    @Test
    public void validatePlayersByDivision() {
        EntityManager em = getEMF().createEntityManager();

        try {
            for (DivisionDefinition divDef : getLeagueDefinition().getDivisions()) {
                List<Player> players = em.createQuery("SELECT p FROM Player p WHERE p.team.division.name = :DIV", Player.class).setParameter("DIV", divDef.getName()).getResultList();
                Assert.assertNotNull(players);
                Assert.assertEquals(divDef.getTotalPlayers(), players.size());
            }
        } finally {
            em.close();
        }
    }

    @Test
    public void validatePlayersByTeam() {
        EntityManager em = getEMF().createEntityManager();

        try {
            for (DivisionDefinition divDef : getLeagueDefinition().getDivisions()) {
                for (String teamName : divDef.getTeamNames()) {
                    List<Player> players = em.createQuery("SELECT p FROM Player p WHERE p.team.name = :TEAM", Player.class).setParameter("TEAM", teamName).getResultList();
                    Assert.assertNotNull(players);
                    Assert.assertEquals(divDef.getNumPlayers(), players.size());
                }
            }
        } finally {
            em.close();
        }
    }
}
