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

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import eclipselink.example.jpa.employee.model.Employee;

/**
 * 
 * @author dclarke
 * @since EclipseLink
 */
public class EmployeeRepository {

    protected PersistenceService persistence;

    public EmployeeRepository(PersistenceService persistence) {
        this.persistence = persistence;
    }

    protected PersistenceService getPersistence() {
        return this.persistence;
    }
    
    public Employee find(int id) {
        EntityManager em = getPersistence().createEntityManager();
        
        try {
            return em.find(Employee.class, id);
        } finally {
            em.close();
        }
    }

    public List<Employee> search(String firstName, String lastName) {
        EntityManager em = getPersistence().createEntityManager();
        
        try {
            TypedQuery<Employee> query = em.createNamedQuery("Employee.findByNamesLike", Employee.class);
            query.setParameter("FNAME", firstName);
            query.setParameter("LNAME", lastName);
            
           return query.getResultList();

        } finally {
            em.close();
        }
    }

    public Employee persist(Employee employee) {
        EntityManager em = getPersistence().createEntityManager();
        
        try {
            em.getTransaction().begin();
            em.persist(employee);
            em.getTransaction().commit();
            return employee;
        } finally {
            em.close();
        }
    }

    public Employee merge(Employee employee) {
        EntityManager em = getPersistence().createEntityManager();
        
        try {
            em.getTransaction().begin();
            Employee result = em.merge(employee);
            em.getTransaction().commit();
            return result;
        } finally {
            em.close();
        }
    }
    
    public void delete(Employee employee) {
        EntityManager em = getPersistence().createEntityManager();
        
        try {
            Employee managedEmp = em.find(Employee.class, employee.getId());
            
            if (managedEmp == null) {
                throw new RuntimeException("Employee not found in DB: " + employee);
            }
            
            // TODO: Break references
            
            em.getTransaction().begin();
            em.remove(managedEmp);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }
}
