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
package example.mysports.service;

import java.util.ArrayList;
import java.util.List;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.Attribute;
import javax.persistence.metamodel.ManagedType;
import javax.persistence.metamodel.Metamodel;

import org.eclipse.persistence.internal.jpa.metamodel.AttributeImpl;
import org.eclipse.persistence.mappings.AttributeAccessor;

import example.mysports.model.Division;
import example.mysports.model.Division_;
import example.mysports.model.Player;
import example.mysports.model.Player_;
import example.mysports.model.Team;
import example.mysports.model.Team_;
import example.mysports.persistence.TenantContext;

/**
 * MySports Domain Model Repository providing simplified persistence facade for
 * managing the application bootstrapped {@link EntityManagerFactory} that are
 * tenant aware.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Stateless
@TransactionAttribute(TransactionAttributeType.NOT_SUPPORTED)
public class LeagueRepository {

    private TenantContext context;

    /**
     * Cache the current division to simplify operations within the division.
     */
    private transient Division currentDivision;

    public TenantContext getContext() {
        return context;
    }

    @Inject
    public void setContext(TenantContext context) {
        this.context = context;
    }

    public void setLeagueId(String leagueId) {
        getContext().setLeagueId(leagueId);
    }

    /**
     * Retrieve all of the divisions in this league and optionally force the
     * lazily loaded teams to be returned within the {@link Division}s.
     */
    public List<Division> getDivisions() {
        EntityManager em = getContext().createEntityManager();

        try {
            TypedQuery<Division> q = em.createNamedQuery("Division.findAll", Division.class);
            return q.getResultList();
        } finally {
            em.close();
        }
    }

    public Division getCurrentDivision() {
        return currentDivision;
    }

    public void setCurrentDivision(Division currentDivision) {
        this.currentDivision = currentDivision;
    }

    public <T> T find(Class<T> entityClass, int id) {
        EntityManager em = getContext().createEntityManager();

        try {
            return em.find(entityClass, id);
        } finally {
            em.close();
        }
    }

    public Division addDivision(String name) {
        EntityManager em = getContext().createEntityManager();

        try {

            em.getTransaction().begin();
            Division div = new Division(name);
            em.persist(div);
            em.getTransaction().commit();
            return div;
        } finally {
            em.close();
        }
    }

    public Team addTeam(String name, List<Player> players, Division division) {
        Division div = division == null ? getCurrentDivision() : division;

        EntityManager em = getContext().createEntityManager();

        try {

            em.getTransaction().begin();

            Division managedDiv = em.merge(div);
            Team team = new Team(name);
            if (players != null) {
                for (Player player : players) {
                    team.addPlayer(player);
                }
            }
            em.persist(team);
            managedDiv.addTeam(team);

            em.getTransaction().commit();

            this.currentDivision = managedDiv;
            return team;
        } finally {
            em.close();
        }
    }

    public Team mergeTeam(Team team) {
        EntityManager em = getContext().createEntityManager();

        try {

            em.getTransaction().begin();

            Team managedTeam = em.merge(team);

            em.getTransaction().commit();

            this.currentDivision = managedTeam.getDivision();
            return managedTeam;
        } finally {
            em.close();
        }
    }

    public Division getDivision(String name) {
        EntityManager em = getContext().createEntityManager();

        try {
            TypedQuery<Division> q = em.createNamedQuery("Division.findByName", Division.class);
            q.setParameter("NAME", name);
            return q.getSingleResult();
        } finally {
            em.close();
        }
    }

    public Team getTeam(String division, String name) {
        EntityManager em = getContext().createEntityManager();

        try {
            TypedQuery<Team> q = em.createNamedQuery("Team.findByDivisionAndName", Team.class);
            q.setParameter("DIV", division);
            q.setParameter("NAME", name);
            return q.getSingleResult();
        } finally {
            em.close();
        }
    }

    /**
     * Retrieve a player using its division, team name, and jersey number using
     * JPA 2.0 criteria.
     */
    public Player getPlayerByNumber(String division, String teamId, int number) {
        EntityManager em = getContext().createEntityManager();

        try {
            CriteriaBuilder qb = em.getCriteriaBuilder();

            CriteriaQuery<Player> query = qb.createQuery(Player.class);
            Root<Player> player = query.from(Player.class);
            Predicate numEqual = qb.equal(player.get(Player_.number), number);
            Predicate teamEqual = qb.equal(player.get(Player_.team).get(Team_.name), teamId);
            Predicate divEqual = qb.equal(player.get(Player_.team).get(Team_.division).get(Division_.name), division);
            query.where(qb.and(numEqual, teamEqual, divEqual));

            return em.createQuery(query).getSingleResult();
        } finally {
            em.close();
        }
    }

    public void remove(Object entity) {
        EntityManager em = getContext().createEntityManager();

        try {
            Object managedEntity = em.merge(entity);

            em.getTransaction().begin();
            em.remove(managedEntity);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    /*
     * JPA API
     */

    /**
     * Determine which mapped attributes are virtual/extended attributes. Used
     * to provide dynamic access to these attributes in presentation layer.
     * 
     * @return list of extended (virtual) JPA meta-model attributes
     * @throws IllegalArgumentException
     *             if entityType is not a managed type
     */
    public List<Attribute<?, ?>> getAdditionalAttributes(Class<?> entityType) {
        EntityManager em = getContext().createEntityManager();

        try {
            Metamodel metamodel = em.getMetamodel();
            ManagedType<?> type = metamodel.managedType(entityType);
            List<Attribute<?, ?>> addnAttrs = new ArrayList<Attribute<?, ?>>();

            for (Attribute<?, ?> attr : type.getAttributes()) {
                AttributeImpl<?, ?> attrImpl = (AttributeImpl<?, ?>) attr;
                AttributeAccessor accessor = attrImpl.getMapping().getAttributeAccessor();
                if (accessor.isVirtualAttributeAccessor()) {
                    addnAttrs.add(attr);
                }
            }

            return addnAttrs;
        } finally {
            em.close();
        }
    }

}
