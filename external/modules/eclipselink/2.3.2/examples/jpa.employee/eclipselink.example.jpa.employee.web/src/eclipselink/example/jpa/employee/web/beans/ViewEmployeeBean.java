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

public class ViewEmployeeBean extends BaseManagedBean {

    protected static final String NAV = "view";

    protected static final String EMP_ID_PARAM = "selected-emp-id";

    private Employee employee;

    private EmployeeService service;

    protected EmployeeService getService() {
        return this.service;
    }

    public Employee getEmployee() {
        if (this.employee == null) {
            this.employee = getSelectedEmployee();
        }
        return this.employee;
    }

    public void setEmployee(Employee employee) {
        this.employee = employee;
    }

    public Employee getSelectedEmployee() {
        String empId = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get(EMP_ID_PARAM);

        if (empId != null && !empId.isEmpty()) {
            Employee emp = getService().findById(getCurrentUser(), Integer.valueOf(empId));

            // TODO: Replace with LoadGroup in EclipseLink 2.1
            if (emp != null) {
                emp.getAddress();
                emp.getPhoneNumbers().size();
                emp.getManager();
                emp.getManagedEmployees().size();
                emp.getProjects().size();
            }
            return emp;
        }

        return null;
    }
}
