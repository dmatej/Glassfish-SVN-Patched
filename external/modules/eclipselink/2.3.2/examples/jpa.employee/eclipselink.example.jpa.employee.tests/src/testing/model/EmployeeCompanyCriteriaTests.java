/*******************************************************************************
 * Copyright (c) 1998, 2010 Oracle. All rights reserved.
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
package testing.model;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.fail;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.eclipse.persistence.exceptions.QueryException;
import org.junit.Test;

import testing.util.EclipseLinkJPATest;
import eclipselink.example.jpa.employee.model.Employee;

/**
 * Simple query examples for the XML mapped Employee domain model.
 * 
 * @author dclarke
 * @since EclipseLink 1.2
 */
@SuppressWarnings("unchecked")
@PersistenceContext(unitName = "employee")
public class EmployeeCompanyCriteriaTests extends EclipseLinkJPATest {

    @Test
    public void verifyU1ReadAll() {
        EntityManager em = getEntityManager("u1", "password");

        List<Employee> allEmps = em.createNamedQuery("Employee.findAll").getResultList();
        assertEquals(12, allEmps.size());
    }

    @Test
    public void verifyAdminReadAll() {
        EntityManager em = getEntityManager("admin", "password");
        
        
        em.setProperty("eclipselink.tenant-id", "%");

        List<Employee> allEmps = em.createNamedQuery("Employee.findAll").getResultList();
        assertEquals(36, allEmps.size());
        em.close();
    }

    @Test
    public void verifyNoUser() {
        EntityManager em = getEMF().createEntityManager();

        try {
            em.createNamedQuery("Employee.findAll").getResultList();
            fail("IllegalStateException not thrown for lack of current user");
        } catch (QueryException e) {
            if (e.getErrorCode() == 6170) {
                return;
            }
        } finally {
            em.close();
        }
    }

    @Test
    public void VerifyNative() {
        EntityManager em = getEntityManager("u1", "password");

        try {
            em.createNativeQuery("SELECT * FROM EMPLOYEE", Employee.class).getResultList();
            fail("UnsupportedOperationException not thrown");
        } catch (UnsupportedOperationException e) {
            return;
        } finally {
            em.close();
        }
    }
}
