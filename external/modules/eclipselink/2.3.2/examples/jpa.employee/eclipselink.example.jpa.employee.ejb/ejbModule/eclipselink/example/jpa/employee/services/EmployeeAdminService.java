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

import eclipselink.example.jpa.employee.model.Company;
import eclipselink.example.jpa.employee.model.User;

@Local
public interface EmployeeAdminService {

    /**
     * 
     * @param userId
     * @param password
     * @return
     */
    User login(String userId, String password);

    void createSchema(User context);

    void replaceSchema();

    void populate(Company company);

    List<Company> createCompany(String code, String name);

    /**
     * TODO
     */
    List<Company> getCompanies();
}
