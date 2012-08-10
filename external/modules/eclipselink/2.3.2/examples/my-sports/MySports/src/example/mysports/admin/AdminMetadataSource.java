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
package example.mysports.admin;

import static org.eclipse.persistence.config.PersistenceUnitProperties.MULTITENANT_PROPERTY_DEFAULT;

import java.util.Map;

import org.eclipse.persistence.config.PersistenceUnitProperties;
import org.eclipse.persistence.internal.jpa.metadata.xml.XMLEntityMappings;
import org.eclipse.persistence.jpa.metadata.MetadataSource;
import org.eclipse.persistence.jpa.metadata.XMLMetadataSource;
import org.eclipse.persistence.logging.SessionLog;

import example.mysports.MySportsConfig;

/**
 * {@link MetadataSource} which using REST calls to retrieve the league (tenant)
 * specific mappings from the MySports Admin application.
 * <p>
 * If the Admin application can not be reached a warning message is logged and
 * the contained 'local-eclipselink-orm.xml' is returned.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class AdminMetadataSource extends XMLMetadataSource {

    /**
     * Default local resource. Only used when admin service cannot be accessed.
     */
    public static final String LOCAL_ECLIPSELINK_ORM = "META-INF/local-eclipselink-orm.xml";

    @Override
    public XMLEntityMappings getEntityMappings(Map<String, Object> properties, ClassLoader classLoader, SessionLog log) {
        String leagueId = (String) properties.get(MULTITENANT_PROPERTY_DEFAULT);
        MySportsConfig config = (MySportsConfig) properties.get(MySportsConfig.class.getName());

        if (config != null) {
            properties.put(PersistenceUnitProperties.METADATA_SOURCE_XML_URL, config.getAdminConnector().getOrmURL(leagueId));
            try {
                return super.getEntityMappings(properties, classLoader, log);
            } catch (Exception e) {
                System.err.println("AdminMetadataSource.getEntityMappings failed. League: " + leagueId + " :: " + e);
            }
        }

        properties.remove(PersistenceUnitProperties.METADATA_SOURCE_XML_URL);
        properties.put(PersistenceUnitProperties.METADATA_SOURCE_XML_FILE, LOCAL_ECLIPSELINK_ORM);
        
        return super.getEntityMappings(properties, classLoader, log);
    }
}
