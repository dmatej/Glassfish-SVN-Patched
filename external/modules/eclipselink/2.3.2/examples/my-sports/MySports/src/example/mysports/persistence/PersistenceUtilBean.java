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
package example.mysports.persistence;

import javax.annotation.PreDestroy;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;

import org.eclipse.persistence.sessions.factories.SessionManager;

import example.mysports.MySportsConfig;
import example.mysports.service.LeagueRepository;

/**
 * This utility class will cause the container to weave the persistence unit.
 * Since the {@link LeagueRepository} uses the persistence unit through the
 * application bootstrap API the container will not instrument/weave the entity
 * classes. This class is ONLY required in the application to force the weaving
 * to occur.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Startup
@Singleton
public class PersistenceUtilBean {

    @SuppressWarnings("unused")
    @PersistenceUnit(unitName = MySportsConfig.PU_NAME)
    private EntityManagerFactory emf;

    /**
     * Ensure all sessions backing tenant specific {@link EntityManagerFactory}
     * are destroyed so that no state exists in the container across
     * deployments.
     */
    @PreDestroy
    public void shutdown() {
        SessionManager.getManager().destroyAllSessions();
    }
}
