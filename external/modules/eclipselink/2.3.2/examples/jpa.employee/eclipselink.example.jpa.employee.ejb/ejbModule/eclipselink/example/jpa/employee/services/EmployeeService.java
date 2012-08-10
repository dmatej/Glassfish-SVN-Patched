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

import javax.ejb.Local;

import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.User;

/**
 * TODO 
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
@Local
public interface EmployeeService {

    /**
     * TODO
     * @param context
     * @param id
     * @return
     */
    Employee findById(User user, int id);

    /**
     * TODO
     * @param context
     * @param lastName
     * @return
     */
    List<Employee> findByLastName(User user, String lastName);

    /**
     * TODO
     * @param context
     * @param employee
     * @return
     */
    Employee deleteEmployee(User user, Employee employee);

    /**
     * TODO
     * @param context
     * @param employee
     * @return
     */
    Employee editEmployee(User user, Employee employee);

    /**
     * TODO
     * @param context
     * @param employee
     * @return
     */
    Employee createEmployee(User user, Employee employee);

    /**
     * Return a refreshed version of the provided entity.
     * 
     * @param appContext
     * @param employee
     * @return
     */
    Employee refresh(User user, Employee employee);

}
