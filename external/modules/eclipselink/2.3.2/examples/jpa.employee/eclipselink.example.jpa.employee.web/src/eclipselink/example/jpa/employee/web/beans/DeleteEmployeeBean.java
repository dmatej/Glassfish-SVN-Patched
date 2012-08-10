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

import javax.faces.context.FacesContext;

import eclipselink.example.jpa.employee.model.Employee;

public class DeleteEmployeeBean extends BaseManagedBean {

    private Employee employee;

    private EmployeeService service;

    protected EmployeeService getService() {
        return this.service;
    }

    public Employee getEmployee() {
        return employee;
    }

    public String setSelectedEmployee() {
        String empId = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get("selected-emp-id");

        if (empId != null && !empId.isEmpty()) {
            this.employee = getService().findById(getCurrentUser(), Integer.valueOf(empId));
            if (this.employee != null) {
                return "delete";
            }
        }
        return null;
    }

    public String delete() {
        getService().deleteEmployee(getCurrentUser(), getEmployee());
        this.employee = null;
        return "";
    }
}
