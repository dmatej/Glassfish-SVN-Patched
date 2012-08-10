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

import static example.mysports.MySportsConfig.PU_NAME;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import example.mysports.MySportsConfig;
import example.mysports.view.LeagueRepositoryBean;

/**
 * This {@link SessionScoped} bean manages the current league (tenant
 * identifier) for the application. It uses this identifier to acquire a tenant
 * specific {@link EntityManager} (persistence context) or a {@link JAXBContext}
 * 
 * @author dclarke
 * @since EclipseLink 2.3.1
 */
@SessionScoped
public class TenantContext implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * Current tenant identifier. Populated by either the user from a JSF page
     * invoking {@link LeagueRepositoryBean#setLeague()} or through the
     * {@link MySportsConfig}'s backed properties file having a dedicated tenant
     * configured in it.
     */
    private String leagueId;

    private MySportsConfig config;

    private Map<String, Object> properties = new HashMap<String, Object>();

    public String getLeagueId() {
        return leagueId;
    }

    public void setLeagueId(String leagueId) {
        if (isMultitenant()) {
            this.leagueId = leagueId;
        }
    }

    public MySportsConfig getConfig() {
        return config;
    }

    @Inject
    public void setConfig(MySportsConfig config) {
        this.config = config;
    }

    public boolean hasLeague() {
        return this.leagueId != null;
    }

    public boolean isMultitenant() {
        return true;
    }

    public Map<String, Object> getProperties() {
        return properties;
    }

    public EntityManager createEntityManager() {
        if (!hasLeague()) {
            throw new IllegalStateException("No league set on " + this);
        }

        EntityManagerFactory emf = TenantPersistence.createEntityManagerFactory(PU_NAME, leagueId, getProperties());
        return emf.createEntityManager();
    }

    public JAXBContext getJAXBContext() throws JAXBException {
        if (!hasLeague()) {
            throw new IllegalStateException("No league set on " + this);
        }
        return TenantPersistence.getJAXBContext(new MySportsConfig(), getLeagueId());
    }

}
