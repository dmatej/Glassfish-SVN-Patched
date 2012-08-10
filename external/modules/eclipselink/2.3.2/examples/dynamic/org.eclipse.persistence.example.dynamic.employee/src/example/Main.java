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
 *     dclarke - Dynamic Persistence INCUBATION - Enhancement 200045
 *               http://wiki.eclipse.org/EclipseLink/Development/JPA/Dynamic
 *     
 * This code is being developed under INCUBATION and is not currently included 
 * in the automated EclipseLink build. The API in this code may change, or 
 * may never be included in the product. Please provide feedback through mailing 
 * lists or the bug database.
 ******************************************************************************/
package example;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.config.PersistenceUnitProperties;
import org.eclipse.persistence.dynamic.DynamicClassLoader;
import org.eclipse.persistence.dynamic.DynamicType;
import org.eclipse.persistence.jpa.dynamic.JPADynamicHelper;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;

import example.util.ExamplePropertiesLoader;

public class Main {

    public static void main(String[] args) throws Exception {
        // Test 1 - using dynamic API directly.
        runDynamicAPITest();
        
        // Test 2 - using only JPA.
        runDynamicJPATest();
    }
    
    public static void runDynamicAPITest() {
        // Create a dynamic class loader and create the types.
        DynamicClassLoader dcl = new DynamicClassLoader(Thread.currentThread().getContextClassLoader());
        List<DynamicType> types = EmployeeDynamicMappings.createTypes(dcl, "example.jpa.dynamic.model.employee");        
        
        // Create an entity manager factory.
        EntityManagerFactory emf = createEntityManagerFactory(dcl, "default");
        
        // Create JPA Dynamic Helper (with the emf above) and after the types
        // have been created and add the types through the helper.
        JPADynamicHelper helper = new JPADynamicHelper(emf);
        helper.addTypes(true, true, types);
        
        // Create database and populate
        new SchemaManager(helper.getSession()).replaceDefaultTables();

        EntityManager em = emf.createEntityManager();
        em.getTransaction().begin();
        new Samples(emf).persistAll(em);
        em.getTransaction().commit();
        em.clear();

        // Lookup types
        DynamicType empType = helper.getType("Employee");

        // Run Queries
        Queries queries = new Queries();

        int minEmpId = queries.minimumEmployeeId(em);
        queries.findEmployee(em, empType, minEmpId);
        queries.findEmployeesUsingGenderIn(em);

        // Example transactions
        Transactions txn = new Transactions();

        txn.createUsingPersist(em);

        em.close();
        emf.close();
    }
    
    public static void runDynamicJPATest() {
        // Create an entity manager factory with a dynamic class loader.
        EntityManagerFactory emf = createEntityManagerFactory(new DynamicClassLoader(Thread.currentThread().getContextClassLoader()), "jpa");

        EntityManager em = emf.createEntityManager();
        em.getTransaction().begin();
        new Samples(emf).persistAll(em);
        em.getTransaction().commit();
        em.clear();

        // Lookup types
        JPADynamicHelper helper = new JPADynamicHelper(emf);
        DynamicType empType = helper.getType("Employee");

        // Run Queries
        Queries queries = new Queries();

        int minEmpId = queries.minimumEmployeeId(em);
        queries.findEmployee(em, empType, minEmpId);
        queries.findEmployeesUsingGenderIn(em);

        // Example transactions
        Transactions txn = new Transactions();

        txn.createUsingPersist(em);

        em.close();
        emf.close();
    }
    
    public static EntityManagerFactory createEntityManagerFactory(DynamicClassLoader dcl, String persistenceUnit) {
        Map<String, Object> properties = new HashMap<String, Object>();
        ExamplePropertiesLoader.loadProperties(properties);
        properties.put(PersistenceUnitProperties.CLASSLOADER, dcl);
        properties.put(PersistenceUnitProperties.WEAVING, "static");
        return Persistence.createEntityManagerFactory(persistenceUnit, properties);
    }
}
