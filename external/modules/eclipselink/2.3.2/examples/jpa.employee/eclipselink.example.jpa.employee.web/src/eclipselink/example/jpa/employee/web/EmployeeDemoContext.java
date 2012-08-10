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
 *      dclarke - TODO 
 ******************************************************************************/
package eclipselink.example.jpa.employee.web;

import java.util.HashMap;
import java.util.Map;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import eclipselink.example.jpa.employee.model.User;
import eclipselink.example.jpa.employee.persistence.EmployeeCompanyCriteria;

@ManagedBean
@SessionScoped
public class EmployeeDemoContext {

    private User currentUser;

    private EntityManagerFactory emf;

    public EmployeeDemoContext() {
        this.emf = Persistence.createEntityManagerFactory("employee");
    }

    public User getCurrentUser() {
        return currentUser;
    }

    protected void setCurrentUser(User user) {
        this.currentUser = user;
    }

    public boolean isSignedIn() {
        return this.currentUser != null;
    }

    public boolean isAdmin() {
        return this.currentUser != null && this.currentUser.isAdministrator();
    }

    public boolean isUser() {
        return this.currentUser != null && !this.currentUser.isAdministrator();
    }

    public String signOut() {
        this.currentUser = null;
        return "login";
    }

    public String getCompanyName() {
        if (getCurrentUser() == null) {
            return "";
        }
        if (getCurrentUser().isAdministrator()) {
            return "System Administrator";
        }
        return getCurrentUser().getCompany().getName();
    }

    public EntityManagerFactory getEntityManagerFactory() {
        return emf;
    }

    public EntityManager createEntityManager() {
        Map<String, Object> properties = new HashMap<String, Object>();
        if (getCurrentUser() != null) {
            EmployeeCompanyCriteria.addProperties(properties, getCurrentUser().getId(), getCurrentUser().getPassword());
        }
        return getEntityManagerFactory().createEntityManager(properties);
    }
}
