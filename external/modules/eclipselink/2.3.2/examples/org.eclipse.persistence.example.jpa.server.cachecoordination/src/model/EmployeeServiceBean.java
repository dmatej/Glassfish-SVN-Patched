/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     James Sutherland - initial impl
 ******************************************************************************/  
 package model;

import java.util.List;

import javax.ejb.Remote;
import javax.ejb.Stateless;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

/**
 * EmployeeService session bean.
 */
@Stateless(mappedName="EmployeeService")
@Remote(EmployeeService.class)
public class EmployeeServiceBean implements EmployeeService {
    @PersistenceContext(name="employee")
    protected EntityManager entityManager;

    public List<Employee> findAll() {
        Query query = this.entityManager.createQuery("Select e from Employee e");
        return query.getResultList();
    }
    
    public Employee findById(long id) {
        Employee employee = this.entityManager.find(Employee.class, id);
        employee.getAddress();
        return employee;
    }
    
    public Employee fetchById(long id) {
        Employee employee = this.entityManager.find(Employee.class, id);
        employee.getAddress();
        employee.getManager();
        return employee;
    }
    
    public void update(Employee employee) {
        this.entityManager.merge(employee);
    }
    
    public long insert(Employee employee) {
        this.entityManager.persist(employee);
        this.entityManager.flush();
        return employee.getId();
    }
    
    public void setup() {
        // Populate database.
        // First delete all old records.
        this.entityManager.createQuery("Delete from PhoneNumber p").executeUpdate();
        this.entityManager.createQuery("Delete from Employee e").executeUpdate();
        this.entityManager.createQuery("Delete from Address a").executeUpdate();
        for (int j = 0; j < 100; j++) {
            Employee empInsert = new Employee();
            empInsert.setFirstName("Brendan");
            empInsert.setMale();
            empInsert.setLastName("" + j + "");
            empInsert.setSalary(100000);
            EmploymentPeriod employmentPeriod = new EmploymentPeriod();
            java.sql.Date startDate = new java.sql.Date(new java.util.Date().getTime());
            employmentPeriod.setStartDate(startDate);
            empInsert.setPeriod(employmentPeriod);
            empInsert.setAddress(new Address());
            empInsert.getAddress().setCity("Nepean");
            empInsert.getAddress().setPostalCode("N5J2N5");
            empInsert.getAddress().setProvince("ON");
            empInsert.getAddress().setStreet("1111 Mountain Blvd. Floor 13, suite " + j);
            empInsert.getAddress().setCountry("Canada");
            empInsert.addPhoneNumber(new PhoneNumber("Work Fax", "613", "2255943"));
            empInsert.addPhoneNumber(new PhoneNumber("Home", "613", "2224599"));
            this.entityManager.persist(empInsert);
        }

        for (int j = 0; j < 50; j++) {
            Project project = new SmallProject();
            project.setName("Tracker");
            this.entityManager.persist(project);
            project = new LargeProject();
            project.setName("Tracker");
            this.entityManager.persist(project);
        }
    }
}
