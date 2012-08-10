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
package testing.ejb;

import javax.persistence.PersistenceContext;

import testing.util.EclipseLinkJPATest;

/**
 * 
 * @author dclarke
 * 
 */
@PersistenceContext(unitName = "employee")
public class EmployeeServiceTests extends EclipseLinkJPATest {
 /**   
    private User currentUser = null;
    
    protected EmployeeService createService(String userId, String password) {
        EmployeeAdminServiceBean adminService = new EmployeeAdminServiceBean();
        EntityManager em = getEMF().createEntityManager();
        adminService.setEntityManager(em);

        try {
            User user = adminService.login(userId, password);
            
            assertNotNull("Login failed with userid: " + userId + " and password: " + password, user);
            assertEquals(userId, user.getId());
            assertNull(user.getPassword());
            
            return createService(user);
        } finally {
            em.close();
        }
    }

    protected EmployeeService createService(User user) {
        EmployeeServiceBean service = new EmployeeServiceBean();
        service.setEntityManager(getEntityManager(user));
        this.currentUser = user;
        return service;
    }

    @Test
    public void verifyU1EmployeeFindAll() {
        EmployeeService service = createService("u1", "password");
        
        List<Employee> emps = service.findByLastName(this.currentUser, "%");
        
        assertEquals(11, emps.size());
    }

    @Test
    public void verifyU1CreateEmployee() {
        EmployeeService service = createService("u1", "password");
        EntityManager em = getEntityManager(this.currentUser);
        
        Employee emp = new Employee(this.currentUser.getCompany());
        
        em.getTransaction().begin();
        
        service.createEmployee(this.currentUser, emp);
        
        em.flush();
        
        // Assert Writes
        assertEquals(2, getQuerySQLTracker(em).getTotalSQLINSERTCalls());
        assertEquals(3, getQuerySQLTracker(em).getTotalSQLSELECTCalls());
        assertEquals(0, getQuerySQLTracker(em).getTotalSQLUPDATECalls());
        assertEquals(0, getQuerySQLTracker(em).getTotalSQLDELETECalls());
        
        em.getTransaction().rollback();
    }
**/}
