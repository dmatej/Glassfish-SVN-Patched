/*******************************************************************************
 * Copyright (c) 1998, 2010 Oracle. All rights reserved.
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
package testing.model;

import static junit.framework.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.config.PersistenceUnitProperties;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.server.Server;
import org.eclipse.persistence.sessions.server.ServerSession;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;
import org.junit.Test;

import eclipselink.example.jpa.employee.model.Company;
import eclipselink.example.jpa.employee.model.User;
import eclipselink.example.jpa.employee.model.util.Sample;
import example.util.ExamplePropertiesLoader;

/**
 * Utility class to create the database schema and populate it for the Employee
 * JPA example using XML configuration. This
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class CreateDatabase {

    private void createTables(EntityManagerFactory emf) {
        assertTrue(emf.isOpen());

        EntityManager em = emf.createEntityManager();
        assertTrue(em.isOpen());
        try {
            ServerSession session = em.unwrap(ServerSession.class);

            SchemaManager sm = new SchemaManager(session);
            sm.replaceDefaultTables();
            sm.createSequences();
        } finally {
            em.close();
        }
    }


    @Test
    public void populate() {
        Map<Object, Object> properties = new HashMap<Object, Object>();
        ExamplePropertiesLoader.loadProperties(properties);

        properties.put(PersistenceUnitProperties.TRANSACTION_TYPE, "RESOURCE_LOCAL");
        properties.put(PersistenceUnitProperties.JTA_DATASOURCE, null);

        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee", properties);
        Server session = JpaHelper.getServerSession(emf);

        EntityManager em = null;

        try {
            createTables(emf);
            // Create Admin user using SQL
            session.executeNonSelectingSQL("INSERT INTO EMP_USER(U_ID, L_NAME, SYS_ADMIN, C_ADMIN, F_NAME, PWD, CC) VALUES('admin', null, 1, 0, 'Admin', 'password', null)");

            em = emf.createEntityManager();

            em.getTransaction().begin();

            Company c1 = createCompany(em, "C1", "Company 1");
            //Company c2 = createCompany(em, "C2", "Company 2");
            //Company c3 = createCompany(em, "C3", "Company 3");

            // Create Users
            User u1 = new User("u1", "User", "One");
            u1.setPassword("password");
            u1.setCompany(c1);
            em.persist(u1);

            User u2 = new User("u2", "User", "Two");
            u2.setPassword("password");
            //u2.setCompany(c2);
            //em.persist(u2);

            User u3 = new User("u3", "User", "Three");
            u3.setPassword("password");
            //u3.setCompany(c3);
            //em.persist(u3);

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

    /**
     * TODO
     * 
     * @param em
     * @param code
     * @param name
     */
    protected static Company createCompany(EntityManager em, String code, String name) {
        em.setProperty("eclipselink.tenant-id", code);

        Company company = new Company();
        company.setCode(code);
        company.setName(name);

        em.persist(company);
        // Flush to ensure that the company can be created
        em.flush();

        new Sample(company).persistAll(em, false);

        em.flush();
        em.clear();

        return company;
    }

    public static void main(String[] args) {
        new CreateDatabase().populate();
    }

}
