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
 *      dclarke - TODO
 ******************************************************************************/
package eclipselink.example.jpa.employee.web.beans;

import eclipselink.example.jpa.employee.model.User;


/**
 * TODO
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
public abstract class BaseManagedBean {

    private EmployeeWebContextBean employeeContext;

    public EmployeeWebContextBean getEmployeeContext() {
        return employeeContext;
    }

    public void setEmployeeContext(EmployeeWebContextBean employeeContext) {
        this.employeeContext = employeeContext;
    }
    
    public User getCurrentUser() {
        return getEmployeeContext().getCurrentUser();
    }

}
