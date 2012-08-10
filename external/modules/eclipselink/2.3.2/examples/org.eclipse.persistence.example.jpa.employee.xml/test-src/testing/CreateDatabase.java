/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 * 		dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package testing;

import static org.eclipse.persistence.config.PersistenceUnitProperties.DDL_GENERATION;
import static org.eclipse.persistence.config.PersistenceUnitProperties.DROP_AND_CREATE;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;
import org.junit.Test;

import example.Sample;
import example.util.ExamplePropertiesLoader;

/**
 * Utility class to create the database schema and populate it for the Employee
 * JPA example using XML configuration. This
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class CreateDatabase {

    @Test
    public void createSchemaAndPopulate() {
        populate(new HashMap());
    }

    public static void populate(Map properties) {
        properties.put(DDL_GENERATION, DROP_AND_CREATE);

        ExamplePropertiesLoader.loadProperties(properties);
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee-xml", properties);

        new SchemaManager(JpaHelper.getServerSession(emf)).createSequences();

        EntityManager em = null;

        try {
            em = emf.createEntityManager();

            em.getTransaction().begin();
            Sample.population.persistAll(em);
            em.getTransaction().commit();
        } finally {
            if (em != null && em.isOpen()) {
                if (em.getTransaction().isActive()) {
                    em.getTransaction().rollback();
                }
                em.close();
                emf.close();
            }
        }
    }

    public static void main(String[] args) {
        new CreateDatabase().createSchemaAndPopulate();
    }

}
