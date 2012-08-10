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

import java.util.List;

import org.eclipse.persistence.internal.descriptors.PersistenceEntity;

import eclipselink.example.jpa.employee.model.Employee;

public class EmployeeController extends BaseManagedBean {

    private EmployeeService service;

    private String lastNameSearch = "%";

    private List<Employee> employeeList;

    protected EmployeeService getService() {
        return this.service;
    }

    public List<Employee> getEmployeeList() {
        return this.employeeList;
    }

    public String getLastNameSearch() {
        if (this.lastNameSearch == null || this.lastNameSearch.isEmpty()) {
            return "%";
        }
        return this.lastNameSearch;
    }

    public void setLastNameSearch(String lastNameSearch) {
        this.lastNameSearch = lastNameSearch;
    }

    public List<Employee> findByLastName(String lastName) {
        return getService().findByLastName(getCurrentUser(), lastName);
    }

    public String search() {
        this.employeeList = findByLastName(getLastNameSearch().toUpperCase());
        return "";
    }

    public boolean isEmployeeWoven() {
        return PersistenceEntity.class.isAssignableFrom(Employee.class);
    }

}
