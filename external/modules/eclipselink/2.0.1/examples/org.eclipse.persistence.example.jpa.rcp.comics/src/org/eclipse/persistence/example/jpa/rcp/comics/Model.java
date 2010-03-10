/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle., Eclipse Foundation All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     tware, ssmith - 1.0 - RCP Demo
 ******************************************************************************/  
package org.eclipse.persistence.example.jpa.rcp.comics;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.Query;

import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.persistence.config.PersistenceUnitProperties;
import org.eclipse.persistence.example.jpa.comics.model.annotated.Publisher;
import org.eclipse.persistence.jpa.osgi.PersistenceProvider;

public class Model extends PlatformObject {
    private static final String PU_NAME = "comics";
    private EntityManagerFactory emf;
    private EntityManager em;
    
    public Model() {
        begin();
    }
    
    public void begin() {
        EntityTransaction transaction = getEntityManager().getTransaction();
        if (transaction.isActive()) {
            throw new RuntimeException("TX already active");
        } else {
            transaction.begin();            
        }
    }
    
    public void commit() {
        EntityTransaction transaction = getEntityManager().getTransaction();
        if (transaction.isActive()) {
            transaction.commit();            
        } else {
            throw new RuntimeException("TX not active");
        }
        begin();
    }

    public Object getRoot() {
        return this;
    }

    public List<Publisher> getPublishers() {
        try {
            Query query = getEntityManager()
                .createQuery("Select p from Publisher p order by p.name asc");
            System.out.println(query);
            return query.getResultList();
        } catch (Throwable e) {
            e.printStackTrace();
            return new ArrayList<Publisher>();
        }
    }

    private EntityManager getEntityManager() {
        if (em == null) {
            em = getEntityManagerFactory().createEntityManager();
        }
        return em;
    }

    private EntityManagerFactory getEntityManagerFactory() {
        if (emf == null) {
            HashMap properties = new HashMap();
            properties.put(PersistenceUnitProperties.CLASSLOADER, this.getClass().getClassLoader());
            emf = new PersistenceProvider().createEntityManagerFactory(
                    PU_NAME, 
                    properties);
        }
        return emf;
    }

    public void dispose() {
        if (getEntityManager().getTransaction().isActive()) {
            getEntityManager().getTransaction().rollback();
        }
        getEntityManagerFactory().close();
    }
}
