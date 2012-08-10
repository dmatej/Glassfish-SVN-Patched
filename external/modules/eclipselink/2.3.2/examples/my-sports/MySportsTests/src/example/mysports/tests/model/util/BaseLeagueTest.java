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
package example.mysports.tests.model.util;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import junit.framework.Assert;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.jpa.JpaHelper;
import org.junit.AfterClass;

import example.mysports.MySportsConfig;
import example.mysports.persistence.TenantContext;
import example.mysports.persistence.TenantPersistence;
import example.mysports.service.LeagueRepository;
import example.mysports.tests.TestingProperties;
import example.mysports.tests.admin.MockAdminServerConnector;

/**
 * 
 * @author dclarke
 * @since EclipseLink 2.3.1
 */
public abstract class BaseLeagueTest {

    private static LeagueRepository repository;

    protected static TenantContext context = new TenantContext();

    protected LeagueRepository getRepository() {
        return getRepository(getLeagueId());
    }

    protected abstract String getLeagueId();

    protected static MySportsConfig getConfig() {
        return context.getConfig();
    }

    protected static LeagueRepository getRepository(String league) {
        if (context.getConfig() == null) {
            context.setConfig(new MySportsConfig());
        }
        EntityManagerFactory adminEMF = Persistence.createEntityManagerFactory("mysports-admin", TestingProperties.get());
        ((MockAdminServerConnector) getConfig().getAdminConnector()).setEMF(adminEMF);

        context.setLeagueId(league);

        repository = new LeagueRepository();
        repository.setContext(context);

        return repository;
    }

    @AfterClass
    public static void closeEMF() {
        if (repository != null) {
            repository.setContext(null);
        }

        if (context != null) {
            ((MockAdminServerConnector) context.getConfig().getAdminConnector()).getEMF().close();
        }

    }

    protected EntityManagerFactory getEMF() {
        return TenantPersistence.createEntityManagerFactory(MySportsConfig.PU_NAME, getLeagueId(), TestingProperties.get(getLeagueId()));
    }

    /**
     * Verify the provided entity is from this league by doing a native query
     * against the database.
     */
    protected void assertLeague(Object entity) {
        Assert.assertNotNull(entity);
        EntityManager em = getEMF().createEntityManager();

        try {
            ClassDescriptor descriptor = JpaHelper.getServerSession(getEMF()).getClassDescriptor(entity);

            Assert.assertNotNull("No descriptor found for: " + entity, descriptor);
        } finally {
            em.close();
        }
    }
}
