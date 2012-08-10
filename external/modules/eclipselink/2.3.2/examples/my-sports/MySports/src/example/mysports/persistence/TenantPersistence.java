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
import static org.eclipse.persistence.config.PersistenceUnitProperties.MULTITENANT_PROPERTY_DEFAULT;
import static org.eclipse.persistence.config.PersistenceUnitProperties.SESSION_NAME;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.eclipse.persistence.internal.jpa.EntityManagerFactoryImpl;
import org.eclipse.persistence.jaxb.JAXBContextFactory;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.factories.SessionManager;
import org.eclipse.persistence.sessions.server.Server;

import example.mysports.MySportsConfig;
import example.mysports.model.Divisions;

/**
 * Helper class used to build and access tenant specific persistence contexts.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.1
 */
public class TenantPersistence {

    /**
     * Lookup and create if needed an EMF for the specified tenant.
     */
    public static EntityManagerFactory createEntityManagerFactory(String persistenceUnitName, String leagueId, Map<String, Object> properties) {
        EntityManagerFactory emf = getEntityManagerFactory(persistenceUnitName, leagueId);

        if (emf == null) {
            Map<String, Object> emfProps = buildProperties(persistenceUnitName, leagueId, properties);
            emf = Persistence.createEntityManagerFactory(PU_NAME, emfProps);
            Server session = JpaHelper.getServerSession(emf);
            session.setProperty(EntityManagerFactory.class.getName(), emf);
        }
        return emf;
    }

    /**
     * Lookup and create if needed an EMF for the specified tenant.
     */
    private static EntityManagerFactory getEntityManagerFactory(String persistenceUnitName, String leagueId) {
        EntityManagerFactory emf = null;
        String sessionName = persistenceUnitName + leagueId;

        // If a shared session exists check for a cached EntityMnagerFactory
        Server session = (Server) SessionManager.getManager().getSessions().get(sessionName);
        if (session != null) {
            emf = (EntityManagerFactory) session.getProperty(EntityManagerFactory.class.getName());
        }
        return emf;
    }

    public static void refreshEntityManagerFactory(String persistenceUnitName, String leagueId, Long version, Map<String, Object> properties) {
        EntityManagerFactory emf = getEntityManagerFactory(persistenceUnitName, leagueId);

        if (emf != null && emf.getProperties().containsKey("VERSION")) {
            Long currentVersion = (Long) emf.getProperties().get("VERSION");
            if (version > currentVersion) {
                ((EntityManagerFactoryImpl) emf).refreshMetadata(properties);
            }
        }
    }

    /**
     * Create the properties
     */
    public static Map<String, Object> buildProperties(String persistenceUnitName, String tenantIdentifier, Map<String, Object> properties) {
        Map<String, Object> emfProps = new HashMap<String, Object>();

        if (properties != null) {
            emfProps.putAll(properties);
        }

        emfProps.put(SESSION_NAME, persistenceUnitName + tenantIdentifier);
        emfProps.put(MULTITENANT_PROPERTY_DEFAULT, tenantIdentifier);
        emfProps.put(MySportsConfig.class.getName(), new MySportsConfig());

        return emfProps;
    }

    /**
     * Property name for caching the {@link JAXBContext} for the league within
     * its {@link EntityManagerFactory}
     */
    private static final String JAXB_CONTEXT = "jaxb-context";
    private static final String MAPPING_FILE = "META-INF/eclipselink-oxm.xml";

    /**
     * Create an EclipseLink {@link JAXBContext} which is built from the
     * {@value #MAPPING_FILE} combined with the virtual attribute extended
     * mappings returned from the call to the Admin server.
     * 
     * @throws JAXBException
     */
    public static JAXBContext getJAXBContext(MySportsConfig config, String leagueId) throws JAXBException {
        EntityManagerFactory emf = createEntityManagerFactory(PU_NAME, leagueId, null);
        Server session = JpaHelper.getServerSession(emf);
        JAXBContext context = (JAXBContext) session.getProperty(JAXB_CONTEXT);

        if (context == null) {
            List<String> xmlBindings = new ArrayList<String>();
            xmlBindings.add(MAPPING_FILE);
            xmlBindings.add(config.getAdminConnector().getOxmURL(leagueId));

            Map<String, Object> props = new HashMap<String, Object>();
            props.put(JAXBContextFactory.ECLIPSELINK_OXM_XML_KEY, xmlBindings);
            context = JAXBContextFactory.createContext(new Class[] { Divisions.class }, props);

            // Cache the JAXB context in the shared session's properties
            session.setProperty(JAXB_CONTEXT, context);
        }
        return context;
    }

}
