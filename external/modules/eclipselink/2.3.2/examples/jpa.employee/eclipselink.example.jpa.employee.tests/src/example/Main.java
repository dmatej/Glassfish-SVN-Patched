/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - EclipseLink Employee JSF-EJB-JPA Example
 ******************************************************************************/
package example;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.config.PersistenceUnitProperties;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;

import eclipselink.example.jpa.employee.model.util.Sample;
import example.util.ExamplePropertiesLoader;

public class Main {

    public static void main(String[] args) {
        EntityManagerFactory emf = createEMF(null);

        try {
            createSchemaAndPopulateSamples(emf);

            new Queries().runExamples(emf);
            new Transactions().runExamples(emf);

        } finally {
            emf.close();
        }
    }

    /**
     * Create an {@link EntityManagerFactory} overriding the persistence.xml
     * config to use RESOURCE_LOCAL transactions and an EclipseLink internal
     * connection pool for JavaSE example/testing purposes. Additionally the
     * {@link ExamplePropertiesLoader} overrides the configuration with any
     * local or user home provided example customization properties.
     * 
     * @param additionalProperties
     *            allows a specific test case to provide some final property
     *            overrides that are applied after all others.
     */
    public static EntityManagerFactory createEMF(Map<Object, Object> additionalProperties) {
        Map<Object, Object> properties = new HashMap<Object, Object>();

        // These properties required since the persistence unit is configured
        // for JTA usage. These will put it in RESOURCE_LOCAL transactions with
        // EclipseLink's internal connection pool.
        properties.put(PersistenceUnitProperties.TRANSACTION_TYPE, "RESOURCE_LOCAL");
        properties.put(PersistenceUnitProperties.JTA_DATASOURCE, null);

        // Load properties from eclipselink-example.properties file to override
        // what is in the persistence.xml.
        ExamplePropertiesLoader.loadProperties(properties);
        
        if (additionalProperties != null) {
            for (Map.Entry<Object, Object> entry: additionalProperties.entrySet()) {
                properties.put(entry.getKey(), entry.getValue());
            }
        }

        return Persistence.createEntityManagerFactory("employee", properties);
    }

    /**
     * Create the schema using the {@link SchemaManager} instead of using the
     * {@link PersistenceUnitProperties#DDL_GENERATION}.
     */
    public static void createSchemaAndPopulateSamples(EntityManagerFactory emf) {
        SchemaManager manager = new SchemaManager(JpaHelper.getServerSession(emf));

        manager.replaceDefaultTables();
        manager.replaceSequences();

        EntityManager em = emf.createEntityManager();

        try {
            em.getTransaction().begin();
            new Sample(null).persistAll(em, true);
            em.getTransaction().commit();
        } finally {
            if (em.getTransaction().isActive()) {
                em.getTransaction().rollback();
            }
            em.close();
        }
    }
}
