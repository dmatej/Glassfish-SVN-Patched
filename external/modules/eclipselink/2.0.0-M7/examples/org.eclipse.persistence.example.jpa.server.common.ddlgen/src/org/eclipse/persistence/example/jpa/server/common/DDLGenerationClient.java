/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:     
 *     10/02/2008-1.1M1 Michael O'Brien 
 *       - 250473: This DDL application managed DDL generation application is required before running the  
 *       tutorial submissions for WebLogic, JBoss, OC4J, GlassFish, Tomcat and WebSphere 
 *       - this project is specific to the Oracle database - customization via persistence.xml is possible for other databases.
 *       see
 *       http://wiki.eclipse.org/EclipseLink/Examples/JPA#JPA_Web_Application_Tutorials
 ******************************************************************************/

package org.eclipse.persistence.example.jpa.server.common;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.Persistence;
import javax.persistence.Query;

import org.eclipse.persistence.example.jpa.server.business.Cell;

/**
 * Purpose: Automatic DDL generation script
 * This test class assumes that we are working with a single Cell entity data model.
 * This class is used to invoke automatic DDL generation on the database specified by the persistence unit.
 * The DDL generation occurs on an application managed EM on a non-JTA datasource
 * 
 * The consumer of the DDL generated schema are all the Web/EJB container example applications in the
 * org.eclipse.persistence.jpa.server namespace.
 * 
 */
public class DDLGenerationClient {

    // Application managed EMF and EM
    public EntityManagerFactory emf  = null;
    public EntityManager entityManager = null;
    // Reference the database specific persistence unit in persistence.xml
    public static final String PU_NAME_CREATE = "dao.create.tables.oracle";    
    
    /**
     * Create the EMF and EM and start a transaction (out of container context)
     * @param puName
     */
    private void initialize(String puName) {
        try {
            // Initialize an application managed JPA emf and em via META-INF
            emf  = Persistence.createEntityManagerFactory(puName);
            entityManager = emf.createEntityManager();            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Close the application managed EM and EMF
     */
    public void finalize() {
        // close JPA
        try {
            if(null != getEntityManager()) {
                getEntityManager().close();
                getEmf().close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    
    /**
     * Insert some test data on the database
     * Assume that the entity manager is set to drop/create tables - or they exist
     */
    private void processInsert() {
        // Insert schema and classes into the database
        
        // create 2 cells and link them
        Cell cell1 = new Cell();
        Cell cell2 = new Cell();
        Set<Cell> peers1 = new HashSet<Cell>();
        Set<Cell> peers2 = new HashSet<Cell>();
        peers1.add(cell2);
        peers2.add(cell1);
        cell1.setPeers(peers1);
        cell2.setPeers(peers2);
        Set<Cell> refs1 = new HashSet<Cell>();
        Set<Cell> refs2 = new HashSet<Cell>();
        refs1.add(cell2);
        refs2.add(cell1);
        cell1.setReferences(refs1);
        cell2.setReferences(refs2);
        try {
            // Cache objects
            getEntityManager().getTransaction().begin();
            getEntityManager().persist(cell1);
            System.out.println("_createDatabase() Inserted: " + cell1);            
            getEntityManager().persist(cell2);
            System.out.println("_createDatabase() Inserted: " + cell2);            
            // Store objects        
            getEntityManager().getTransaction().commit();
        } catch (Exception e) {
            e.printStackTrace();
        }        
        //generateGlider(null);
    }

    /**
     * Attempt to create a "Complete Graph" of entities of numEntities size
     * where each Cell has a @ManyToMany relationship to all other Cell entities.
     * @param numEntities
     */
    private void persistCompleteGraphOfEntities(int numEntities) {        
        // Insert schema and classes into the database by using an EM with DDL generation   
        // create n cells of entities
        List<Cell> cells = new ArrayList<Cell>();
        for(int i=0;i<numEntities;i++ ) {
            Cell cell = new Cell();
            cells.add(cell);
        }

        // Add all (n-1) cells as peers except yourself
        Set<Cell> peers = new HashSet<Cell>();
        for(int current=0;current<numEntities;current++ ) {
            for(int i=0;i<current;i++ ) {
                peers.add(cells.get(i));
            }
            // skipped current cell 
            for(int i=current + 1;i<numEntities;i++ ) {
                peers.add(cells.get(i));
            }
            cells.get(current).setPeers(peers); 
            // clear peers set
            peers.clear();
        }

        // Add all(n-1) cells as bidirectional references except yourself
        Set<Cell> refs = new HashSet<Cell>();
        for(int current=0;current<numEntities;current++ ) {
            for(int i=0;i<current;i++ ) {
                refs.add(cells.get(i));
            }
            // skipped current cell 
            for(int i=current + 1;i<numEntities;i++ ) {
                refs.add(cells.get(i));
            }
            cells.get(current).setReferences(refs);            
            // clear references set
            refs.clear();
        }
        try {
            // Cache objects
            EntityManager em = getEntityManager();
            if(null == em) {
                System.out.println("EntityManager is null: Check your persistence.xml properties");
            } else {
                EntityTransaction transaction = em.getTransaction();
                if(null == transaction) {
                    System.out.println("Cannot get a transaction from entityManager: " + em);
                } else {                    
                    transaction.begin();
                    // all cells must be persisted together in a single transaction
                    for(int current=0;current<numEntities;current++ ) {
                        getEntityManager().persist(cells.get(current));
                        System.out.println("_persistCompleteGraphOfEntities() Inserted: " + cells.get(current));
                    }
                    // Store objects        
                    transaction.commit();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }        
        //generateGlider(null);
    }
    
    /**
     * Generate a W. H. Gosper Glider gun glider self-replicating pattern
     * @param out
     */
    private void generateGlider(PrintWriter out) {
        // Insert schema and classes into the database
        // Create glider
        // .  5  .
        // .  .  4
        // 1 2 3
        // Create the cells
        int numCells = 5;
        List<Cell> cells = new ArrayList<Cell>();
        for(int i=0;i<numCells;i++ ) {
            Cell cell = new Cell();
            cells.add(cell);
        }
        
        Cell cell1 = cells.get(0);
        Cell cell2 = cells.get(1);
        Cell cell3 = cells.get(2);
        Cell cell4 = cells.get(3);
        Cell cell5 = cells.get(4);        

        // Link objects in owner direction
        Set<Cell> peers1 = new HashSet<Cell>();
        Set<Cell> peers2 = new HashSet<Cell>();
        Set<Cell> peers3 = new HashSet<Cell>();
        Set<Cell> peers4 = new HashSet<Cell>();
        Set<Cell> peers5 = new HashSet<Cell>();
        peers1.add(cell2);
        
        peers2.add(cell1);
        peers2.add(cell3);
        peers2.add(cell4);
        
        peers3.add(cell2);
        peers3.add(cell4);
        
        peers4.add(cell2);
        peers4.add(cell3);
        peers4.add(cell5);        
        
        peers5.add(cell4);
        
        // Set peer cells
        cell1.setPeers(peers1);
        cell2.setPeers(peers2);
        cell3.setPeers(peers3);
        cell4.setPeers(peers4);
        cell5.setPeers(peers5);        

        // Link objects in reverse direction
        Set<Cell> refs1 = new HashSet<Cell>();
        Set<Cell> refs2 = new HashSet<Cell>();
        Set<Cell> refs3 = new HashSet<Cell>();
        Set<Cell> refs4 = new HashSet<Cell>();
        Set<Cell> refs5 = new HashSet<Cell>();
        refs1.add(cell2);
        
        refs2.add(cell1);
        refs2.add(cell3);
        refs2.add(cell4);
        
        refs3.add(cell2);
        refs3.add(cell4);
        
        refs4.add(cell2);
        refs4.add(cell3);
        refs4.add(cell5);        
        
        refs5.add(cell4);
        
        // Set peer cells
        cell1.setReferences(refs1);
        cell2.setReferences(refs2);
        cell3.setReferences(refs3);
        cell4.setReferences(refs4);
        cell5.setReferences(refs5);        

        // store first before creating relationships
        try {
            // Cache objects
            EntityManager em = getEntityManager();
            if(null == em) {
                System.out.println("EntityManager is null: Check your persistence.xml properties");
            } else {
                EntityTransaction transaction = em.getTransaction();
                if(null == transaction) {
                    System.out.println("Cannot get a transaction from entityManager: " + em);
                } else {                    
                    transaction.begin();
                    // all cells must be persisted together in a single transaction
                    for(int current=0;current<numCells;current++ ) {
                        em.persist(cells.get(current));
                        System.out.println("_persistCompleteGraphOfEntities() Inserted: " + cells.get(current));
                    }
                    // Store objects        
                    transaction.commit();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }        
    }
    
    /**
     * Get test data from the database
     * @param singleResult
     * @param jpqlQuery
     * @return always returns a list for a single/multiple result
     */
    public List<Cell> query( boolean singleResult, String jpqlQuery) {
        Query aQuery = null;
        Cell aResult = null;
        // send back an empty list if we have EM problems
        List<Cell> aResultsList = new ArrayList<Cell>();
        try {
            EntityManager em = getEntityManager();
            if(null == em) {
                System.out.println("EntityManager is null: Check your persistence.xml properties");
            } else {
                aQuery = em.createQuery(jpqlQuery);
                if(singleResult) {                
                    aResult = (Cell) aQuery.getSingleResult();
                    aResultsList = new ArrayList<Cell>();
                    if(null != aResult) {
                        aResultsList.add(aResult);
                    }
                } else {                
                    // Handle no results - create an empty list
                    aResultsList = aQuery.getResultList();
                }
            }            
        } catch (Exception e) {
            e.printStackTrace();
        }
        return aResultsList;
    }
    
    private void queryTest() {
        List<Cell> rowsList = (List<Cell>) query(false, "select object(e) from Cell e");
        Iterator<Cell> anIterator = rowsList.iterator();
        System.out.println(rowsList.size() + " Entities in storage");
        int rows = 0;
        while (anIterator.hasNext()) {
            Object anObject = anIterator.next();
            rows++;
            // Use an extended DTO wrapper around one of the entities
            System.out.println("Object: " + anObject);
        }
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        ////doCreateTables();
        doQuery();        
        //testMethod();
    }

    /**
     * doQuery() will invoke DDL generation via it's PU properties
     */
    public static void doQuery() {
        DDLGenerationClient aClient = new DDLGenerationClient();
        aClient.initialize(PU_NAME_CREATE);
        //aClient.processInsert();
        //aClient.persistCompleteGraphOfEntities(2);    // variable SOE down to 875     
        aClient.generateGlider(null);
        aClient.queryTest();
        aClient.finalize();
    }
    
    public EntityManagerFactory getEmf() {
        return emf;
    }

    public void setEmf(EntityManagerFactory emf) {
        this.emf = emf;
    }

    public EntityManager getEntityManager() {
        return entityManager;
    }

    public void setEntifyManager(EntityManager entityManager) {
        this.entityManager = entityManager;
    }
}
