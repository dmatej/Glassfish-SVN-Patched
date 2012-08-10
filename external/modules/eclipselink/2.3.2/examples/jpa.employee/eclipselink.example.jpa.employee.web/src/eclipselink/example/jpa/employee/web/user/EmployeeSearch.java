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

import java.util.List;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.bean.SessionScoped;
import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.web.EmployeeDemoContext;

@ManagedBean
@SessionScoped
public class EmployeeSearch {

    private String firstName = "%";

    private String lastName = "%";

    private List<Employee> results;

    @ManagedProperty(value = "#{employeeDemoContext}")
    private EmployeeDemoContext context;

    public EmployeeDemoContext getContext() {
        return context;
    }

    public void setContext(EmployeeDemoContext context) {
        this.context = context;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public List<Employee> getResults() {
        return results;
    }

    public void setResults(List<Employee> results) {
        this.results = results;
    }

    public String search() {
        EntityManager em = getContext().createEntityManager();

        try {
            String jpql = "SELECT e FROM Employee e WHERE e.firstName LIKE :FNAME AND e.lastName LIKE :LNAME";

            TypedQuery<Employee> query = em.createQuery(jpql, Employee.class);
            query.setParameter("FNAME", getFirstName());
            query.setParameter("LNAME", getLastName());

            this.results = query.getResultList();
            return "employee_search";
        } finally {
            em.close();
        }
    }
}
