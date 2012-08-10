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
package eclipselink.example.jpa.employee.web.user;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.context.FacesContext;
import javax.persistence.EntityManager;
import javax.servlet.http.HttpServletRequest;

import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.web.EmployeeDemoContext;

@ManagedBean
public class EmployeeView {

    private Employee employee;

    @ManagedProperty(value = "#{employeeDemoContext}")
    private EmployeeDemoContext context;

    public EmployeeDemoContext getContext() {
        if (this.context == null) {
        }
        return context;
    }

    public void setContext(EmployeeDemoContext context) {
        this.context = context;
    }

    public Employee getEmployee() {
        if (this.employee == null) {
            Integer empId = getEmployeeId();

            if (empId != null) {
                EntityManager em = getContext().createEntityManager();

                try {
                    this.employee = em.find(Employee.class, empId);
                } finally {
                    em.close();
                }
            }
        }
        return employee;
    }

    /**
     * Retrieve the 'employeeId' parameter from the {@link FacesContext}
     * 
     * @return
     */
    protected Integer getEmployeeId() {
        FacesContext context = FacesContext.getCurrentInstance();
        HttpServletRequest myRequest = (HttpServletRequest) context.getExternalContext().getRequest();
        Object empId = myRequest.getParameter("employeeId");

        if (empId instanceof Number) {
            return ((Number) empId).intValue();
        }
        if (empId instanceof String) {
            return Integer.valueOf((String) empId);
        }

        return null;
    }
}
