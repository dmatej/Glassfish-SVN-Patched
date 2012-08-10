/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - Bug 324357 - Employee example using JSF-EJB-JPA for 2.1.2 
 ******************************************************************************/
package eclipselink.example.jpa.employee.services;

import java.sql.Timestamp;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.eclipse.persistence.internal.jpa.EntityManagerImpl;
import org.eclipse.persistence.jpa.JpaHelper;

import eclipselink.example.jpa.employee.model.User;
import eclipselink.example.jpa.employee.persistence.EmployeeCompanyCriteria;
import eclipselink.example.jpa.employee.persistence.ProjectAsOfCriteria;
import eclipselink.example.jpa.employee.services.diagnostics.Diagnostics;

/**
 * Base Service class for Employee Example Session Beans that require the
 * current user to apply additional criteria when querying the database.
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
public abstract class EmployeeBaseService {

    private EntityManager entityManager;

    private Diagnostics diagnostics;

    @PersistenceContext(unitName = "employee")
    public void setEntityManager(EntityManager em) {
        this.entityManager = em;
        this.diagnostics = new Diagnostics(this);
    }

    public EntityManager getEntityManager() {
        return this.entityManager;
    }

    public Diagnostics getDiagnostics() {
        return diagnostics;
    }

    protected EntityManager getEntityManager(User currentUser, Timestamp currentTime) {
        verifyUser(currentUser);
        
        EntityManagerImpl em = (EntityManagerImpl) JpaHelper.getEntityManager(getEntityManager());

        if (em == null) {
            // TODO
            throw new IllegalStateException();
        }

        em.setProperty(EmployeeCompanyCriteria.USER, currentUser);
        em.setProperty(ProjectAsOfCriteria.TS_PROPERTY, currentTime);

        return em;
    }
    
    /**
     * TODO
     * @param user
     */
    protected void verifyUser(User user) {
        
    }

}
