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

import java.io.InputStream;
import java.util.List;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.eclipse.persistence.jaxb.xmlmodel.XmlBindings;

import example.mysports.admin.model.HostedLeague;
import example.mysports.admin.model.Style;

/**
 * Session bean exposing JAX-RS methods for defining and usage of league
 * specific metadata.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Stateless
@LocalBean
@Path("league")
public class HostedLeagues {

    @Context
    private UriInfo context;

    @PersistenceUnit(unitName = "mysports-admin")
    private EntityManagerFactory emf;

    @GET
    @Produces(MediaType.APPLICATION_XML)
    public List<HostedLeague> allLeagues() {
        EntityManager em = getEmf().createEntityManager();
        try {
            List<HostedLeague> leagues = em.createNamedQuery("HostedLeague.findAll", HostedLeague.class).getResultList();

            for (HostedLeague hl : leagues) {
                hl.setUri(context.getAbsolutePath() + "/" + hl.getId());
            }

            return leagues;
        } finally {
            em.close();
        }
    }

    @GET
    @Path("{league}")
    @Produces(MediaType.APPLICATION_XML)
    public HostedLeague getLeague(@PathParam("league") String leagueId) {
        System.out.println("MySportsAdmin> Retrieving Leagues.");

        EntityManager em = getEmf().createEntityManager();
        try {
            return em.find(HostedLeague.class, leagueId);
        } finally {
            em.close();
        }
    }

    @DELETE
    @Path("{league}")
    public Response deleteLeague(@PathParam("league") String leagueId) {
        System.out.println("MySportsAdmin> Retrieving Leagues.");

        EntityManager em = getEmf().createEntityManager();
        try {
            HostedLeague league = em.find(HostedLeague.class, leagueId);

            if (league == null) {
                return Response.status(Response.Status.NOT_FOUND).build();
            }
            em.getTransaction().begin();
            em.remove(league);
            em.getTransaction().commit();

            return Response.status(Response.Status.ACCEPTED).build();
        } finally {
            em.close();
        }
    }

    @POST
    public Response create(@FormParam("league-id") String id, @FormParam("league-name") String name, @FormParam("colour-scheme") String scheme) {
        EntityManager em = getEmf().createEntityManager();

        try {
            em.getTransaction().begin();
            HostedLeague league = new HostedLeague(id, name, scheme);
            em.persist(league);
            em.getTransaction().commit();

            return Response.status(Response.Status.CREATED).build();
        } finally {
            em.close();
        }
    }

    @GET
    @Path("{league}/orm")
    @Produces(MediaType.APPLICATION_XML)
    public String getORM(@PathParam("league") String leagueId) {
        EntityManager em = getEmf().createEntityManager();

        try {
            return MappingsLoader.getORMapping(em, leagueId);
        } finally {
            em.close();
        }
    }

    @GET
    @Path("{league}/oxm")
    @Produces(MediaType.APPLICATION_XML)
    public XmlBindings getOXM(@PathParam("league") String leagueId) {
        EntityManager em = getEmf().createEntityManager();

        try {
            return MappingsLoader.getXMLBindings(em, leagueId);
        } finally {
            em.close();
        }
    }

    @GET
    @Path("{league}.css")
    @Produces("text/css")
    public String getCSS(@PathParam("league") String leagueId) {
        EntityManager em = getEmf().createEntityManager();

        try {
            HostedLeague league = em.find(HostedLeague.class, leagueId);
            Style style = null;

            if (league != null && league.getColourScheme() != null) {
                style = em.find(Style.class, league.getColourScheme());
            }
            if (style == null) {
                em.find(Style.class, "default");
            }

            return style.getCss();
        } finally {
            em.close();
        }
    }

    @GET
    @Path("{league}.png")
    @Produces("image/jpeg")
    public InputStream getLogo(@PathParam("league") String leagueId) {
        return Thread.currentThread().getContextClassLoader().getResourceAsStream(leagueId.toLowerCase() + ".png");
    }

    public EntityManagerFactory getEmf() {
        return emf;
    }

    public void setEmf(EntityManagerFactory emf) {
        this.emf = emf;
    }

    public UriInfo getContext() {
        return context;
    }

    public void setContext(UriInfo context) {
        this.context = context;
    }

}