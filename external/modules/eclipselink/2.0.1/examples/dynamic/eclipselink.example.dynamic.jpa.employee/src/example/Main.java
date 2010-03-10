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

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.dynamic.DynamicHelper;
import org.eclipse.persistence.dynamic.DynamicType;
import org.eclipse.persistence.jpa.dynamic.JPADynamicHelper;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;

public class Main {

    /**
     * @param args
     */
    public static void main(String[] args) throws Exception {
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("empty");
        DynamicHelper helper = new JPADynamicHelper(emf);

        // Add dynamic types
        EmployeeDynamicMappings.createTypes(emf, "example.jpa.dynamic.model.employee", false);

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

}
