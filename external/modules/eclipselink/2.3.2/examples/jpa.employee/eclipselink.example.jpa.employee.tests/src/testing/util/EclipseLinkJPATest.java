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
 * 		dclarke - initial JPA Employee example using XML (bug 217884)
 * 				- ported from earlier Oracle Toplink examples
 ******************************************************************************/
package testing.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.ReadObjectQuery;
import org.eclipse.persistence.sessions.server.Server;
import org.junit.After;
import org.junit.AfterClass;

import eclipselink.example.jpa.employee.model.User;

/**
 * Base test case for testing a JPA persistence unit in JavaSE using JUnit4.
 * 
 * Through the usage
 * 
 * @PersistenceContext on subclasses a developer can indicate the persistence
 *                     unit name that the
 * @BeforeClass method should use to access the entityManager.
 * 
 * @author dclarke
 * @since EclipseLink 1.1.2
 */
public abstract class EclipseLinkJPATest {

    private static EntityManagerFactory emf;

    private EntityManager entityManager;

    private User currentUser;

    public static EntityManagerFactory getEMF() {
        if (emf == null) {
            emf = Persistence.createEntityManagerFactory("employee");
            QuerySQLTracker.install(JpaHelper.getServerSession(emf));
        }
        return emf;
    }

    protected EntityManager getEntityManager(String userid, String password) {
        if (userid != null) {
            User candidate = findCachedUser(userid);

            if (candidate == null) {
                throw new RuntimeException("Invalid user id");
            }

            if (!candidate.validate(password)) {
                throw new RuntimeException("Invalid password");
            }

            this.currentUser = candidate;
        }
        if (this.entityManager == null || !this.entityManager.isOpen()) {
            this.entityManager = getEMF().createEntityManager();
        }

        if (this.entityManager != null && this.currentUser != null && this.currentUser.getCompany() != null) {
            this.entityManager.setProperty("eclipselink.tenant-id", getCurrentUser().getCompany().getCode());
            verifyConfig(emf, this.entityManager, userid);
        }

        return this.entityManager;
    }

    protected User findCachedUser(String id) {
        Server session = JpaHelper.getServerSession(getEMF());
        ReadObjectQuery roq = new ReadObjectQuery(User.class);
        roq.setSelectionCriteria(roq.getExpressionBuilder().get("id").equal(id));

        return (User) session.executeQuery(roq);
    }

    /**
     * 
     * @param properties
     * @return
     * @throws Exception
     */
    protected EntityManagerFactory createEMF(String unitName, Map<String, Object> properties) {
        try {
            Map<Object, Object> emfProps = new HashMap<Object, Object>();

            if (properties != null) {
                emfProps.putAll(properties);
            }

            EntityManagerFactory emf = Persistence.createEntityManagerFactory(unitName, emfProps);
            QuerySQLTracker.install(JpaHelper.getServerSession(emf));
            return emf;
        } catch (Exception e) {
            System.out.println("Persistence.createEMF FAILED: " + e.getMessage());
            e.printStackTrace();
            throw new RuntimeException("EclipseLinkJPATest.createEMF(" + unitName + ", properties) - failed", e);
        }
    }

    public User getCurrentUser() {
        return this.currentUser;
    }

    /**
     * This method is invoked prior to the return of an EntityManager from
     * {@link #getEntityManager()}. The intent is that subclasses of this test
     * class can override this method to verify configuration information that
     * must be true for all test cases. This of course assumes
     * {@link #getEntityManager()} is called in each test case.
     */
    protected void verifyConfig(EntityManagerFactory emf, EntityManager em, String userid) {
        assertNotNull("EntityManagerFactory is null", emf);
        assertNotNull("EntityManager is null", em);
        assertNotNull("No user in EntityManager. Expected user with id: " + userid, getCurrentUser());
        assertEquals("Current user's id does not match requested", userid, getCurrentUser().getId());
    }

    protected QuerySQLTracker getQuerySQLTracker(EntityManager em) {
        return QuerySQLTracker.getTracker(JpaHelper.getEntityManager(em).getActiveSession());
    }

    protected QuerySQLTracker getQuerySQLTracker(EntityManagerFactory emf) {
        return QuerySQLTracker.getTracker(JpaHelper.getServerSession(emf));
    }

    @After
    public void cleanupClosedEMF() {
        if (this.entityManager != null) {

            if (this.entityManager.getTransaction().isActive()) {
                this.entityManager.getTransaction().rollback();
            }
            if (this.entityManager.isOpen()) {
                this.entityManager.close();
            }
        }
        this.entityManager = null;

        if (emf != null) {
            if (!emf.isOpen()) {
                emf = null;
            } else {
                // Clear query-SQL tracker
                QuerySQLTracker.getTracker(JpaHelper.getServerSession(emf)).reset();
                // Clear Shared Cache
                JpaHelper.getServerSession(emf).getIdentityMapAccessor().initializeAllIdentityMaps();
                // Initialize sequences
                JpaHelper.getServerSession(emf).getSequencingControl().initializePreallocated();
            }
        }
    }

    @AfterClass
    public static void closeEMF() throws Exception {
        if (emf != null && emf.isOpen()) {
            emf.close();
            emf = null;
        }
    }

    protected ClassDescriptor getDescriptor(Object entity) {
        return JpaHelper.getServerSession(getEMF()).getClassDescriptor(entity);
    }

    protected ClassDescriptor getDescriptor(String alias) {
        return JpaHelper.getServerSession(getEMF()).getClassDescriptorForAlias(alias);
    }
}
