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

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.bean.RequestScoped;
import javax.persistence.EntityManager;

import eclipselink.example.jpa.employee.model.User;

@ManagedBean
@RequestScoped
public class Login {

    @ManagedProperty(value = "#{employeeDemoContext}")
    private EmployeeDemoContext context;

    private String userid;

    private String password = "password";

    private String errorMessage;

    public String login() {
        System.out.println("LOGIN: " + getUserid() + "/" + getPassword());

        if (getUserid() == null || getUserid().isEmpty()) {
            setPassword(null);
            this.errorMessage = "Invalid user Id";
            return null;
        }

        EntityManager em = getContext().createEntityManager();

        User user = em.find(User.class, getUserid());

        if (user == null) {
            setPassword("");
            this.errorMessage = "Unknown User";
            return null;
        }

        if (user.getPassword() != null) {
            if (!user.getPassword().equals(getPassword())) {
                setPassword("");
                this.errorMessage = "Incorrect Password";
                return null;
            }
        }
        setPassword("");

        getContext().setCurrentUser(user);
        return user.isAdministrator() ? "admin_home" : "user_index?faces-redirect=true";
    }

    public String getUserid() {
        return userid;
    }

    public void setUserid(String userid) {
        this.userid = userid;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public EmployeeDemoContext getContext() {
        return context;
    }

    public void setContext(EmployeeDemoContext context) {
        this.context = context;
    }

}
