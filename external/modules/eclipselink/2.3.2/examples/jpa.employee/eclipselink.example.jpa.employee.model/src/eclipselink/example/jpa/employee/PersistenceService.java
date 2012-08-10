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

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.internal.helper.ConversionManager;
import org.eclipse.persistence.internal.jpa.metadata.columns.TenantDiscriminatorColumnMetadata;
import org.eclipse.persistence.jpa.JpaHelper;

import eclipselink.example.jpa.employee.model.User;

/**
 * <b>Purpose</b>: Primary persistence interface for application usage.
 * <b>Responsibilities</b>:
 * <ul>
 * <li>Manage singleton {@link EntityManagerFactory} (application managed)
 * <li>Handle user login validation and current {@link #user}
 * <li>Repository access based on user type
 * </ul>
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class PersistenceService {

    private EntityManagerFactory emf;

    private User user;

    private EmployeeRepository employeeRepository;

    private CompanyAdminRepository adminRepository;

    public PersistenceService(String userid, String password) {
        this.emf = Persistence.createEntityManagerFactory("employee");

        // Lookup User
        EntityManager em = createEntityManager();
        try {
            User candidate = em.find(User.class, userid);
            if (candidate == null || !candidate.validate(password)) {
                throw new IllegalArgumentException("Invalid userId or password");
            }
            this.user = candidate;
        } finally {
            em.close();
        }

        // Configure Model repositories
        this.employeeRepository = new EmployeeRepository(this);
        if (getUser().isCompanyAdmin()) {
            this.adminRepository = new CompanyAdminRepository(this);
        } else if (getUser().isAdministrator()) {
            this.adminRepository = new SystemAdminRepository(this);
        }
    }

    private EntityManagerFactory getEntityManagerFactory() {
        return this.emf;
    }

    protected EntityManager createEntityManager() {
        Map<String, Object> properties = new HashMap<String, Object>(1);

        if (getUser() != null && getUser().getCompany() != null) {
            properties.put(TenantDiscriminatorColumnMetadata.CONTEXT_PROPERTY_DEFAULT, user.getCompany().getCode());
        }
        return getEntityManagerFactory().createEntityManager(properties);
    }

    protected User getUser() {
        return this.user;
    }

    /**
     * Return current user as a view so that the user interface doe snot have access to password or  
     */
    public UserView getUserView() {
        return new UserView(getUser());
    }

    public EmployeeRepository getEmployeeRepository() {
        return employeeRepository;
    }

    public CompanyAdminRepository getAdminRepository() {
        return this.adminRepository;
    }

    /**
     * Simple utility to convert object values using the underlying EclipseLink
     * {@link ConversionManager}. 
     */
    @SuppressWarnings("unchecked")
    public <T> T convert(Object value, Class<T> type) {
        return (T) JpaHelper.getServerSession(getEntityManagerFactory()).getPlatform().convertObject(value, type);
    }
}
