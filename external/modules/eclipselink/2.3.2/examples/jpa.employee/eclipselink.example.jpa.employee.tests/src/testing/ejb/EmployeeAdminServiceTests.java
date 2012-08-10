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
 *      dclarke - TODO
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
public class EmployeeAdminServiceTests extends EclipseLinkJPATest {
/**    
    protected EmployeeAdminService createService(User user) {
        EmployeeAdminServiceBean adminService = new EmployeeAdminServiceBean();
        
        adminService.setEntityManager(getEntityManager(user));
        
        return adminService;
    }
    
    @Test
    public void testDirectEMCreate() {
        EntityManager em = getEMF().createEntityManager();
        
        try {
        em.createNamedQuery("Employee.findAll").getResultList();
        } catch (IllegalStateException e) {
            return;
        }
        fail("IllegalStateException not thrown");
    }

**/}
