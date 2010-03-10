/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - initial GeoNames EclipseLink JPA example
 ******************************************************************************/
package example.where.utils;

import static org.eclipse.persistence.config.PersistenceUnitProperties.*;

import java.util.*;

import javax.persistence.*;

import org.eclipse.persistence.config.TargetDatabase;
import org.eclipse.persistence.descriptors.RelationalDescriptor;
import org.eclipse.persistence.exceptions.DatabaseException;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.ReportQuery;
import org.eclipse.persistence.sessions.server.Server;

import example.where.service.impl.EclipseLinkServiceFactory;
import example.where.service.impl.PersistenceService;

/**
 * Utility to drop and create the schema
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class CreateSchema {

    /**
     * @param args
     */
    public static void main(String[] args) {
        EntityManagerFactory emf = null;
        EntityManager em = null;

        Map<String, String> properties = new HashMap<String, String>();

        properties.put(DDL_GENERATION, DROP_AND_CREATE);
        properties.put(DDL_GENERATION_MODE, DDL_BOTH_GENERATION);
        properties.put(TARGET_DATABASE, TargetDatabase.Oracle);

        emf = PersistenceService.createEMF(properties, true, new EclipseLinkServiceFactory(), true);
        em = emf.createEntityManager();
        em.close();
        emf.close();
    }

    /**
     * Walk through all mapped entity's descriptors verifying that no data
     * exists on the database
     */
    private static void verifyDatabaseEmpty(EntityManager em) {
        Server server = JpaHelper.getEntityManager(em).getServerSession();

        for (Iterator i = server.getDescriptors().values().iterator(); i.hasNext();) {
            RelationalDescriptor descriptor = (RelationalDescriptor) i.next();

            if (!descriptor.isAggregateDescriptor()) {
                ReportQuery rq = new ReportQuery(descriptor.getJavaClass(), new ExpressionBuilder());
                rq.addCount();
                rq.setShouldReturnSingleValue(true);

                int count = -1;
                try {
                    count = ((Number) server.executeQuery(rq)).intValue();
                } catch (DatabaseException dbe) {

                }

                System.out.println("COUNT(" + descriptor.getAlias() + "): " + count);
            }
        }
    }

    /**
     * 
     * @param entityName
     * @param em
     * @return number of rows deleted or -1 if delete failed
     */
    private static int deleteAll(String entityName, EntityManager em) {
        try {
            return em.createQuery("DELETE FROM " + entityName).executeUpdate();
        } catch (PersistenceException pe) {
            return -1;
        }
    }

    public static void deleteAll(EntityManager em) {
        boolean begunTX = false;

        if (!em.getTransaction().isActive()) {
            em.getTransaction().begin();
            begunTX = true;
        }

        deleteAll("Point", em);
        deleteAll("CountryLanguage", em);
        deleteAll("Language", em);
        deleteAll("Feature", em);
        deleteAll("TimeZone", em);
        deleteAll("AdminDivision", em);
        deleteAll("Country", em);
        deleteAll("Continent", em);

        em.flush();

        if (begunTX) {
            em.getTransaction().commit();
        }
        verifyDatabaseEmpty(em);
    }
}
