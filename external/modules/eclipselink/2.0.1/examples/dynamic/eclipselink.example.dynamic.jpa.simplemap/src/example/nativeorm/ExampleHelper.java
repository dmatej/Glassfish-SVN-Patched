/*******************************************************************************
 * Copyright (c) 1998, 2008 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Bug 277731: Simple Map Dynamic JPA Example
 *               http://wiki.eclipse.org/EclipseLink/Examples/JPA/Dynamic/SimpleDynamicMap
 ******************************************************************************/
package example.nativeorm;

import java.util.HashMap;
import java.util.Map;

import model.DynamicMapEntity;

import org.eclipse.persistence.config.PersistenceUnitProperties;
import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.RelationalDescriptor;
import org.eclipse.persistence.dynamic.DynamicClassLoader;
import org.eclipse.persistence.internal.sessions.PropertiesHandler;
import org.eclipse.persistence.logging.SessionLog;
import org.eclipse.persistence.mappings.DirectToFieldMapping;
import org.eclipse.persistence.sessions.DatabaseLogin;
import org.eclipse.persistence.sessions.DatabaseSession;
import org.eclipse.persistence.sessions.Project;
import org.eclipse.persistence.sessions.Session;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;

import example.util.ExamplePropertiesLoader;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.1.1
 */
public class ExampleHelper {

    /**
     * Create a database session with no descriptors. The property values used
     * to populate the DatabaseLogin object are loaded from a 'test.properties'
     * file stored in the user's home.
     * 
     * The eclipselink-example.properties file should contain your environement
     * specific configuration overrides as:
     * 
     * eclipselink.jdbc.driver=oracle.jdbc.OracleDriver
     * eclipselink.jdbc.url=jdbc:oracle:thin:@localhost:1521:ORCL
     * eclipselink.jdbc.user=scott eclipselink.jdbc.pwd=tiger
     * eclipselink.target-database=org.eclipse.persistence.platform.database.OraclePlatform
     * 
     * @return newly created and logged in Session
     */
    public static DatabaseSession createSession() {
        Map<String, Object> properties = new HashMap<String, Object>();
        ExamplePropertiesLoader.loadProperties(properties);

        DatabaseLogin login = new DatabaseLogin();
        login.setDriverClassName((String) properties.get(PersistenceUnitProperties.JDBC_DRIVER));
        login.setDatabaseURL((String) properties.get(PersistenceUnitProperties.JDBC_URL));
        login.setUserName((String) properties.get(PersistenceUnitProperties.JDBC_USER));
        login.setPassword((String) properties.get(PersistenceUnitProperties.JDBC_PASSWORD));

        String eclipselinkPlatform = PropertiesHandler.getPropertyValueLogDebug(PersistenceUnitProperties.TARGET_DATABASE, properties, null);
        if (eclipselinkPlatform != null) {
            login.setPlatformClassName(eclipselinkPlatform, Thread.currentThread().getContextClassLoader());
        }

        DatabaseSession session = new Project(login).createDatabaseSession();
        session.getSessionLog().setLevel(SessionLog.FINE);
        session.login();

        return session;
    }

    /**
     * Create a new dynamic type called 'SimpleType' with the class name of
     * 'model.SimpleType'. The generated class would look like: <code>
     * package model;
     * public class SimpleType extends example.dynamic.DynamicEntity {}
     * </code>
     * 
     * The attributes defined in the mapping give the class the apparent
     * structure of: <code>
     * package model;
     * public class SimpleType extends example.dynamic.DynamicEntity {
     *    Integer id;
     *    String value;
     * }
     * </code>
     */
    public static ClassDescriptor createDynamicType(DatabaseSession session) {
        RelationalDescriptor descriptor = new RelationalDescriptor();
        descriptor.setJavaClassName("model.SimpleType");

        DynamicClassLoader dcl = DynamicClassLoader.lookup(session);
        dcl.addClass("model.SimpleType", DynamicMapEntity.class);
        try {
            descriptor.setJavaClass(dcl.loadClass(descriptor.getJavaClassName()));
        } catch (ClassNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        descriptor.setTableName("DYNAMIC_SIMPLE");
        descriptor.setPrimaryKeyFieldName("ID");

        DirectToFieldMapping mapping = (DirectToFieldMapping) descriptor.addDirectMapping("id", "ID");
        mapping.setAttributeAccessor(new DynamicMapEntity.ValueAccessor(mapping, Integer.class));
        mapping = (DirectToFieldMapping) descriptor.addDirectMapping("value", "VALUE");
        mapping.setAttributeAccessor(new DynamicMapEntity.ValueAccessor(mapping, String.class));

        session.addDescriptor(descriptor);

        // Create the underlying table on the database. Drop it if it already
        // exists
        new SchemaManager(session).replaceDefaultTables();

        return descriptor;
    }

    /**
     * Factory for creating new dynamic instances
     */
    public static DynamicMapEntity newInstance(Session session, String typeName) {
        ClassDescriptor descriptor = session.getClassDescriptorForAlias(typeName);
        return (DynamicMapEntity) descriptor.getInstantiationPolicy().buildNewInstance();
    }

}
