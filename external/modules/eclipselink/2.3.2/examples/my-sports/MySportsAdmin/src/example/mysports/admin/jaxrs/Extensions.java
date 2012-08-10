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

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.LockModeType;
import javax.persistence.PersistenceUnit;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Response;

import example.mysports.admin.model.Extension;
import example.mysports.admin.model.HostedLeague;

/**
 * Persistent entity representing an extension to any domain object in the
 * MySports application.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Stateless
@LocalBean
@Path("extensions")
public class Extensions {

    @PersistenceUnit(unitName = "mysports-admin")
    private EntityManagerFactory emf;

    @POST
    @Path("{entity}")
    public Response create(@PathParam("entity") String entityType, @FormParam("league-id") String leagueId, @FormParam("attribute-name") String name, @FormParam("attribute-type") String javaType, @FormParam("column-name") String columnName, @FormParam("xml-path") String xmlPath) {
        EntityManager em = getEmf().createEntityManager();

        try {
            if (xmlPath == null || xmlPath.isEmpty()) {
                xmlPath = name;
            }
            if (!xmlPath.endsWith("/text()")) {
                xmlPath = xmlPath + "/text()";
            }

            HostedLeague league = em.find(HostedLeague.class, leagueId);

            em.getTransaction().begin();
            em.lock(league, LockModeType.OPTIMISTIC_FORCE_INCREMENT);

            em.persist(new Extension(league, "Player", name, javaType, columnName, xmlPath));
            em.getTransaction().commit();

            return Response.status(Response.Status.CREATED).build();
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