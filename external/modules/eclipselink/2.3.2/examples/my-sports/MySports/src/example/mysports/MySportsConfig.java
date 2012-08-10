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
package example.mysports;

import static org.eclipse.persistence.config.PersistenceUnitProperties.MULTITENANT_PROPERTY_DEFAULT;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;

import javax.enterprise.context.ApplicationScoped;
import javax.faces.context.ExternalContext;

import org.eclipse.persistence.internal.helper.ConversionManager;

import example.mysports.admin.AdminServerConnector;
import example.mysports.admin.League;
import example.mysports.service.LeagueRepository;

/**
 * Responsible for managing application context configuration. For now this is
 * hard coded for local deployment but will be made more flexible in the future.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@ApplicationScoped
public class MySportsConfig implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * Application specific configuration file which controls various deployment
     * and testing specific options.
     * 
     * @see #loadProperties() for processing information
     */
    public static final String PROPERTIES_FILE = "mysports.properties";

    /**
     * TODO
     */
    public static final String ADMIN_SERVER_URL_PROPERTY = "mysports.admin.url";

    /**
     * TODO
     */
    public static final String ADMIN_SERVER_CONTEXT_PROPERTY = "mysports.admin.context";

    /**
     * Persistence unit name. This is the base PU which is used as a template
     * for each tenant's persistence unit.
     * 
     * @see LeagueRepository#setLeagueId(String, java.util.Map) for details of
     *      how the PUs are created from template.
     */
    public static final String PU_NAME = "mysports";

    public static final String ADMIN_CONNECTOR_PROPERTY = "mysports.admin-connector";

    /**
     * configuration properties loaded from available {@value #PROPERTIES_FILE}
     * files.
     */
    private Properties properties;

    private AdminServerConnector adminConnector;

    private League league;

    /**
     * Create a new {@link MySportsConfig} loading available properties files.
     */
    public MySportsConfig() {
        try {
            this.properties = loadProperties();
        } catch (IOException e) {
            throw new RuntimeException("MySportsConfig failed to load " + PROPERTIES_FILE + " files", e);
        }
    }

    protected String getProperty(String name) {
        return (String) this.properties.get(name);
    }

    public AdminServerConnector getAdminConnector() {
        if (this.adminConnector == null) {
            Object value = getProperty(ADMIN_CONNECTOR_PROPERTY);
            if (value != null && value instanceof String) {
                @SuppressWarnings("unchecked")
                Class<AdminServerConnector> connectorClass = ConversionManager.getDefaultManager().convertClassNameToClass((String) value);
                try {
                    this.adminConnector = connectorClass.newInstance();
                    this.adminConnector.setConfig(this);
                } catch (InstantiationException e) {
                    throw new RuntimeException("Could not create AdminServerConnector", e);
                } catch (IllegalAccessException e) {
                    throw new RuntimeException("Could not create AdminServerConnector", e);
                }
            }
        }
        return this.adminConnector;
    }

    public boolean isMultitenant() {
        return !this.properties.containsKey(MULTITENANT_PROPERTY_DEFAULT);
    }

    public String getAdminContext() {
        return getProperty(ADMIN_SERVER_CONTEXT_PROPERTY);
    }

    public String getAdminURL(ExternalContext externalContext) {
        return externalContext.getRequestScheme() + "://" + externalContext.getRequestServerName() + ":" + externalContext.getRequestServerPort();
    }

    public String getLeagueContext() {
        return (String) this.properties.get(MULTITENANT_PROPERTY_DEFAULT);
    }

    public League getLeague() {
        if (this.league == null) {
            if (isMultitenant()) {
                throw new RuntimeException("No league context provided in " + PROPERTIES_FILE);
            }
            this.league = getLeague(getLeagueContext());
        }
        return this.league;
    }

    public League getLeague(String leagueId) {
        if (isMultitenant()) {
            return getAdminConnector().getLeague(leagueId);
        } else {
            return getLeague();
        }

    }

    /**
     * Load all available {@value #PROPERTIES_FILE} files merging them together
     * in the reverse order they are found. This will mean that the properties
     * files available first on the classpath will override others.
     */
    private Properties loadProperties() throws IOException {
        Enumeration<URL> urls = null;
        urls = Thread.currentThread().getContextClassLoader().getResources(PROPERTIES_FILE);

        List<Properties> props = new ArrayList<Properties>();
        while (urls.hasMoreElements()) {
            URL url = urls.nextElement();
            InputStream in = null;
            try {
                in = url.openStream();
                Properties newProps = new Properties();
                newProps.load(in);
                props.add(0, newProps);
            } finally {
                in.close();
            }
        }

        Properties mergedProperties = new Properties();
        for (Properties prop : props) {
            mergedProperties.putAll(prop);
        }
        return mergedProperties;
    }

}
