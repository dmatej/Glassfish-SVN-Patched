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
package eclipselink.example.jpa.employee;

import eclipselink.example.jpa.employee.model.User;

public class UserView {

    private String userid;

    private String firstName;

    private String lastName;

    private String companyName;
    
    private boolean companyAdmin;
    
    private boolean systemAdmin;

    protected UserView(User user) {
        this.userid = user.getId();
        this.firstName = user.getFirstName();
        this.lastName = user.getLastName();
        this.companyAdmin = user.isCompanyAdmin();
        this.systemAdmin = user.isAdministrator();
        if (user.getCompany() != null) {
            this.companyName = user.getCompany().getName();
        }
    }

    public String getUserid() {
        return userid;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public String getCompanyName() {
        return companyName;
    }

    public boolean isCompanyAdmin() {
        return companyAdmin;
    }

    public boolean isSystemAdmin() {
        return systemAdmin;
    }
}
