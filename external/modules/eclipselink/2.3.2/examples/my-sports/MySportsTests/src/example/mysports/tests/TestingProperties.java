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
package example.mysports.tests;

import static org.eclipse.persistence.config.PersistenceUnitProperties.CONNECTION_POOL;
import static org.eclipse.persistence.config.PersistenceUnitProperties.CONNECTION_POOL_INITIAL;
import static org.eclipse.persistence.config.PersistenceUnitProperties.CONNECTION_POOL_MIN;
import static org.eclipse.persistence.config.PersistenceUnitProperties.CONNECTION_POOL_READ;
import static org.eclipse.persistence.config.PersistenceUnitProperties.JTA_DATASOURCE;
import static org.eclipse.persistence.config.PersistenceUnitProperties.METADATA_SOURCE;
import static org.eclipse.persistence.config.PersistenceUnitProperties.METADATA_SOURCE_XML_FILE;
import static org.eclipse.persistence.config.PersistenceUnitProperties.NON_JTA_DATASOURCE;
import static org.eclipse.persistence.config.PersistenceUnitProperties.TRANSACTION_TYPE;

import java.util.HashMap;
import java.util.Map;

import example.mysports.tests.util.ExamplePropertiesLoader;

/**
 * Add testing specific properties to override the non-JTA data-source
 * configured in the persistence.xml.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class TestingProperties  {


    public static Map<String, Object> add(String leagueId, Map<String, Object> properties) {
        Map<String, Object> props = properties;

        if (props == null) {
            props = new HashMap<String, Object>();
        }
        props.put(TRANSACTION_TYPE, "RESOURCE_LOCAL");
        props.put(NON_JTA_DATASOURCE, "");
        props.put(JTA_DATASOURCE, "");

        props.put(CONNECTION_POOL_READ + CONNECTION_POOL_INITIAL, "1");
        props.put(CONNECTION_POOL_READ + CONNECTION_POOL_MIN, "1");
        props.put(CONNECTION_POOL + CONNECTION_POOL_INITIAL, "1");
        props.put(CONNECTION_POOL + CONNECTION_POOL_MIN, "1");

        if (leagueId != null) {
            props.put(METADATA_SOURCE, "XML");
            props.put(METADATA_SOURCE_XML_FILE, leagueId.toLowerCase() + "-eclipselink-orm.xml");
        }

        // Load property overrides from
        // {user-home}/eclipselink-example.properties
        ExamplePropertiesLoader.loadProperties(props);
        return props;
    }

    public static Map<String, Object> get(String leagueId) {
        return add(leagueId, null);
    }

    public static Map<String, Object> get() {
        return add(null, null);
    }
}
