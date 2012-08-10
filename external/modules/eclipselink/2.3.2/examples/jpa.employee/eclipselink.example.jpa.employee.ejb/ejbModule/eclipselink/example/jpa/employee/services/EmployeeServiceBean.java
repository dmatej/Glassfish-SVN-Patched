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
 *      dclarke - Bug 324357 - Employee example using JSF-EJB-JPA for 2.1.2 
 ******************************************************************************/
package eclipselink.example.jpa.employee.services;

import java.util.List;

import javax.ejb.Stateless;
import javax.interceptor.Interceptors;
import javax.persistence.EntityManager;
import javax.persistence.OptimisticLockException;
import javax.persistence.Query;

import org.eclipse.persistence.config.QueryHints;

import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.User;
import eclipselink.example.jpa.employee.services.diagnostics.SQLCaptureInterceptor;

/**
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
@Stateless
@Interceptors({ SQLCaptureInterceptor.class })
public class EmployeeServiceBean extends EmployeeBaseService implements EmployeeService {

    public Employee findById(User user, int id) {
        EntityManager em = getEntityManager(user, null);

        Employee emp = em.find(Employee.class, id);
        if (emp != null) {
            return emp;
        } else {
            return emp;
        }
    }

    @SuppressWarnings("unchecked")
    public List<Employee> findByLastName(User user, String lastName) {
        EntityManager em = getEntityManager(user, null);

        Query query = em.createNamedQuery("Employee.findByLastNameLike");
        query.setParameter("LN", lastName);
        query.setHint(QueryHints.LEFT_FETCH, "e.address");
        query.setMaxResults(20);
        List<Employee> results = query.getResultList();
        return results;
    }

    /**
     * 
     * @see eclipselink.example.jpa.employee.services.EmployeeService#createEmployee(eclipselink.example.jpa.employee.services.EmployeeAppContext,
     *      eclipselink.example.jpa.employee.model.Employee)
     */
    public Employee createEmployee(User user, Employee employee) {
        EntityManager em = getEntityManager(user, null);

        em.persist(employee);
        em.flush();
        return employee;
    }

    /**
     * 
     * @see eclipselink.example.jpa.employee.services.EmployeeService#deleteEmployee(eclipselink.example.jpa.employee.services.EmployeeAppContext,
     *      eclipselink.example.jpa.employee.model.Employee)
     */
    public Employee deleteEmployee(User user, Employee employee) {
        EntityManager em = getEntityManager(user, null);

        Employee empWC = em.find(Employee.class, employee.getId());

        if (empWC != null) {
            if (!empWC.remove()) {
                empWC = null;
            }
        }
        return empWC;
    }

    /**
     * @see eclipselink.example.jpa.employee.services.EmployeeService#editEmployee(eclipselink.example.jpa.employee.services.EmployeeAppContext,
     *      eclipselink.example.jpa.employee.model.Employee)
     */
    public Employee editEmployee(User user, Employee employee) {
        EntityManager em = getEntityManager(user, null);

        Employee wc = null;

        try {
            em.merge(employee);
        } catch (IllegalArgumentException iae) {
            throw EmployeeServiceException.entityNotFound(Employee.class, employee.getId());
        }
        try {
            em.flush();
        } catch (OptimisticLockException ole) {
            throw EmployeeServiceException.optimisticLockFailure(ole);
        }
        return wc;
    }

    /**
     * 
     * @see eclipselink.example.jpa.employee.services.EmployeeService#refresh(eclipselink.example.jpa.employee.services.EmployeeAppContext,
     *      eclipselink.example.jpa.employee.model.Employee)
     */
    public Employee refresh(User user, Employee employee) throws EmployeeServiceException {
        EntityManager em = getEntityManager(user, null);

        Employee emp = em.find(Employee.class, employee.getId());

        if (emp == null) {
            throw EmployeeServiceException.entityNotFound(employee.getClass(), employee.getId());
        }

        em.refresh(emp);
        return emp;
    }

}
