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
 *     26/02/2009 - 2.0 Michael O'Brien 
 *       - 250475: Initial example tutorial submission for OC4J 10.1.3.5 EAR
 *       - all 3 Eclipse projects required EAR, EJB and Web
 *       http://wiki.eclipse.org/EclipseLink/Examples/JPA/OC4J_Web_Tutorial
 ******************************************************************************/ 

package org.eclipse.persistence.example.jpa.server.business;

import java.util.List;

import javax.ejb.Local;
import javax.ejb.Remove;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.Query;

@Local
@Stateless//(name="ApplicationService", mappedName="ApplicationService")
public class ApplicationService implements ApplicationServiceLocal  {

	@PersistenceContext(unitName="example", type=PersistenceContextType.TRANSACTION)	
	private EntityManager entityManager;

    public boolean insert(Object class1) {
        boolean errors = false;
        try {
            System.out.println("[EL Example]: enterprise: ApplicationService persisting: " + class1 + " on EM: " + entityManager);
            entityManager.persist(class1);
        } catch (Exception e) {
            e.printStackTrace();
            errors = true;
        }
        return errors;
    }    

    /**
     * Inserting multiple entities should be done in one transaction on the session bean
     * @param classes
     * @return
     */
    public boolean insertObjects(List<Cell> classes) {
        boolean errors = false;
        try {
            for(int i=0; i< classes.size(); i++) {
                System.out.println("[EL Example]: enterprise: ApplicationService persisting: " + classes.get(i) + " on EM: " + entityManager);
                entityManager.persist(classes.get(i));
            }
            
        } catch (Exception e) {
            errors = true;
            System.out.println("[EL Example]: enterprise: ApplicationService.insertObjects() exception: " + e.getMessage()); 
            e.printStackTrace();
        }
        return errors;
    }    

    public List<Cell> query(String jpqlQuery) {
		Query aQuery = null;
		List<Cell> aResultsList = null;
		try {
			aQuery = entityManager.createQuery(jpqlQuery);
			aResultsList = aQuery.getResultList();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return aResultsList;
	}


    @Remove
	public void finished() {
	}
    
    public String toString() {
        StringBuffer aString = new StringBuffer("ApplicationService@");
        aString.append(hashCode()).append("jpa provider: ").append(entityManager.toString());
        return aString.toString();
    }

    public EntityManager getEntityManager() {
        return entityManager;
    }
}
