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
 *      dclarke - EclipseLink Employee JSF-EJB-JPA Example
 ******************************************************************************/
package eclipselink.example.jpa.employee.web.beans;

import eclipselink.example.jpa.employee.model.User;

/**
 * Primary context managed bean for the web tier.
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
public class EmployeeWebContextBean {
    
    private EmployeeAdminService service;

    private User currentUser;

    private String userid;

    private String password;

    private String loginError;

    public EmployeeAdminService getService() {
        return service;
    }

    public void setService(EmployeeAdminService service) {
        this.service = service;
    }

    public User getCurrentUser() {
        return currentUser;
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

    public String login() {
        if (getUserid() == null || getUserid().isEmpty() || getPassword() == null || getPassword().isEmpty()) {
            this.loginError = "Invalid userid or password";
            return null;
        }

        this.currentUser = getService().login(getUserid(), getPassword());
        if (this.currentUser != null) {
            // Clear login values
            this.userid = null;
            this.password = null;
            this.loginError = null;
        } else {
            this.loginError = "Incorrect userid or password";
        }
        return "home";
    }

    public String getLoginError() {
        return this.loginError;
    }

    public String logout() {
        this.userid = null;
        this.password = null;
        this.loginError = null;
        return "home";
    }
}
