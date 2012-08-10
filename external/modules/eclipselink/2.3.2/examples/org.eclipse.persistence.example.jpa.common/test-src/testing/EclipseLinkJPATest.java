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
package testing;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.PersistenceContext;

import junit.framework.Assert;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.server.Server;
import org.junit.After;
import org.junit.AfterClass;

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

    /**
     * This is he current EMF in use
     */
    private static EntityManagerFactory emf;

    private EntityManager entityManager;
    
    private static EclipseLinkJPATest last;

    protected EntityManagerFactory getEMF() {
        if (emf == null) {
            emf = createEMF(getUnitName());
        }

        return emf;
    }

    protected EntityManager getEntityManager() {
        if (this.entityManager == null) {
            this.entityManager = getEMF().createEntityManager();
        }

        verifyConfig(getEMF(), this.entityManager);

        return this.entityManager;
    }

    protected EntityManagerFactory createEMF(String unitName) {
        if (emf != null) {
            if (emf.isOpen()) {
                emf.close();
            }
        }

        Assert.assertNotNull("EclipseLinkJPATest.createEMF:: Null unit name", unitName);

        try {
            return createEMF(unitName, null);
        } catch (RuntimeException e) {
            System.out.println("Persistence.createEMF FAILED: " + e.getMessage());
            e.printStackTrace();
            throw e;
        }
    }

    protected String getUnitName() {
        PersistenceContext context = null;
        Class<?> javaClass = getClass();

        while (context == null && javaClass != Object.class) {
            context = (PersistenceContext) javaClass.getAnnotation(PersistenceContext.class);
            javaClass = javaClass.getSuperclass();
        }
        Assert.assertNotNull("No @PersistenceContext found", context);

        return context.unitName();
    }

    /**
     * Create the {@link EntityManagerFactory}
     */
    protected EntityManagerFactory createEMF(String unitName, Map<Object, Object> properties) {
        try {
            Map<Object, Object> emfProps = getEMFProperties();

            if (properties != null) {
                emfProps.putAll(properties);
            }

            EntityManagerFactory emf = Persistence.createEntityManagerFactory(unitName, emfProps);
            QuerySQLTracker.install(JpaHelper.getServerSession(emf));
            
            postCreate(emf);
            
            return emf;
        } catch (Exception e) {
            System.out.println("Persistence.createEMF FAILED: " + e.getMessage());
            e.printStackTrace();
            throw new RuntimeException("EclipseLinkJPATest.createEMF(" + unitName + ", properties) - failed", e);
        }
    }
    
    /**
     * 
     */
    protected void postCreate(EntityManagerFactory emf) {
        
    }

    /**
     * 
     * @return
     */
    protected Map<Object, Object> getEMFProperties() {
        Map<Object, Object> properties = new HashMap<Object, Object>();

        ExamplePropertiesLoader.loadProperties(properties);

        return properties;
    }

    /**
     * This method is invoked prior to the return of an EntityManager from
     * {@link #getEntityManager()}. The intent is that subclasses of this test
     * class can override this method to verify configuration information that
     * must be true for all test cases. This of course assumes
     * {@link #getEntityManager()} is called in each test case.
     */
    protected void verifyConfig(EntityManagerFactory emf, EntityManager em) {
        assertNotNull("EntityManagerFactory is null", emf);
        assertNotNull("EntityManager is null", em);
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
                QuerySQLTracker.getTracker(JpaHelper.getServerSession(emf)).reset();
                // Reset the cache each time
                getEMF().getCache().evictAll();
            }
        }
        
        last = this;
    }

    @AfterClass
    public static void closeEMF() throws Exception {
        if (emf != null && emf.isOpen()) {
            if (last != null) {
                last.preClose(emf);
            }
            
            emf.close();
            emf = null;
        }
    }
    
    /**
     * TODO
     */
    protected void preClose(EntityManagerFactory emf) {
        
    }

    protected ClassDescriptor getDescriptor(Object entity) {
        return JpaHelper.getServerSession(getEMF()).getClassDescriptor(entity);
    }

    protected ClassDescriptor getDescriptor(String alias) {
        Server session = JpaHelper.getServerSession(getEMF());
        assertTrue("No descriptor for alias: " + alias, session.getProject().getAliasDescriptors().containsKey(alias));
        return session.getClassDescriptorForAlias(alias);
    }

    protected void log(int level, String message) {
        JpaHelper.getServerSession(getEMF()).getSessionLog().log(level, message);
    }
}
