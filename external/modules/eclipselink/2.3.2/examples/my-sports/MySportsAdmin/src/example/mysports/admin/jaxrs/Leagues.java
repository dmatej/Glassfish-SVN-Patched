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
package example.mysports.admin.jaxrs;

import java.util.List;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import example.mysports.admin.model.League;

/**
 * TODO
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Stateless
@LocalBean
@Path("leagues")
public class Leagues {

    @PersistenceUnit(unitName="mysports-admin")
    private EntityManagerFactory emf;

    @GET
    @Produces(MediaType.APPLICATION_XML)
    public List<League> allLeagues() {
        System.out.println("MySportsAdmin> Retrieving Leagues.");

        EntityManager em = getEmf().createEntityManager();
        try {
            return em.createQuery("SELECT l FROM League l", League.class).getResultList();
        } finally {
            em.close();
        }
    }

    @POST
    @Produces(MediaType.APPLICATION_XML)
    @Path("create")
    public League create(@FormParam("league-id") String id, @FormParam("league-name") String name, @FormParam("league-logo") String logoUrl) {
        System.out.println("MySportsAdmin> Retrieving Leagues.");

        EntityManager em = getEmf().createEntityManager();
        try {
            em.getTransaction().begin();
            League league = new League(id, name);
            em.persist(league);
            em.getTransaction().commit();
            return league;
        } finally {
            em.close();
        }
    }

    public EntityManagerFactory getEmf() {
        return emf;
    }

    public void setEmf(EntityManagerFactory emf) {
        this.emf = emf;
    }

}