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
 * 		dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package testing.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.changetracking.ChangeTracker;
import org.eclipse.persistence.exceptions.QueryException;
import org.eclipse.persistence.internal.descriptors.PersistenceEntity;
import org.eclipse.persistence.internal.descriptors.PersistenceObject;
import org.eclipse.persistence.internal.weaving.PersistenceWeaved;
import org.eclipse.persistence.internal.weaving.PersistenceWeavedChangeTracking;
import org.eclipse.persistence.internal.weaving.PersistenceWeavedFetchGroups;
import org.eclipse.persistence.internal.weaving.PersistenceWeavedLazy;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.mappings.DatabaseMapping;
import org.eclipse.persistence.queries.FetchGroupTracker;
import org.junit.Test;

import testing.util.EclipseLinkJPATest;
import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.EmploymentPeriod;
import eclipselink.example.jpa.employee.model.User;

/**
 * Set of tests to ensure the mappings are properly populated from the provided
 * annotations/xml.
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
@PersistenceContext(unitName = "employee")
public class ConfigTests extends EclipseLinkJPATest {

    @Test
    public void verifyEmployeeDescriptor() throws Exception {
        ClassDescriptor descriptor = getDescriptor("Employee");

        assertNotNull("No ClassDescriptor found for Employee", descriptor);
        assertEquals("Employee", descriptor.getAlias());
        assertNull(descriptor.getInheritancePolicyOrNull());

        // Check interfaces
        assertFalse(PersistenceWeaved.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceEntity.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceObject.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(FetchGroupTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedFetchGroups.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedLazy.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(ChangeTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedChangeTracking.class.isAssignableFrom(descriptor.getJavaClass()));

        // Multitenancy
        assertNotNull(descriptor.getQueryManager().getAdditionalJoinExpression());
        assertTrue(descriptor.getQueryManager().hasAdditionalCriteriaArguments());
        @SuppressWarnings("rawtypes")
        Map<String, Class> args = descriptor.getQueryManager().getAdditionalCriteriaArguments();
        // TODO - replace with contextProperty name provided
        assertEquals(1, args.size());
        assertTrue(args.containsKey("EMPLOYEE.TENANT_ID"));
        assertEquals(String.class, args.get("EMPLOYEE.TENANT_ID"));

        // Extensibility
        // TODO

        // Address Mapping
        DatabaseMapping mapping = descriptor.getMappingForAttributeName("address");
        assertNotNull("No mapping for attribute: address", mapping);
        assertTrue("Not 1:1 :: address", mapping.isOneToOneMapping());
        assertTrue("Mapping is not lazy: address", mapping.isLazy());
        assertTrue("Mapping is not private-owned: address", mapping.isPrivateOwned());

        // Manager Mapping
        mapping = descriptor.getMappingForAttributeName("manager");
        assertNotNull("No mapping for attribute: manager", mapping);
        assertTrue("Not 1:1 :: manager", mapping.isOneToOneMapping());
        assertTrue("Mapping is not lazy: manager", mapping.isLazy());
        assertFalse("Mapping is private-owned: manager", mapping.isPrivateOwned());

        // PhoneNumber Mapping
        mapping = descriptor.getMappingForAttributeName("phoneNumbers");
        assertNotNull("No mapping for attribute: phoneNumbers", mapping);
        assertTrue("Not 1:M :: phoneNumbers", mapping.isOneToManyMapping());
        assertTrue("Mapping is not lazy: phoneNumbers", mapping.isLazy());
        assertTrue("Mapping is private-owned: phoneNumbers", mapping.isPrivateOwned());
    }

    @Test
    public void verifyAddressDescriptor() throws Exception {
        ClassDescriptor descriptor = JpaHelper.getServerSession(getEMF()).getDescriptorForAlias("Address");

        assertNotNull("No ClassDescriptor found for Address", descriptor);
        assertEquals("Address", descriptor.getAlias());
        assertNull(descriptor.getInheritancePolicyOrNull());

        // Check interfaces
        assertFalse(PersistenceWeaved.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceEntity.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceObject.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(FetchGroupTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedFetchGroups.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedLazy.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(ChangeTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedChangeTracking.class.isAssignableFrom(descriptor.getJavaClass()));
    }

    @Test
    public void verifyPhoneNumberDescriptor() {
        ClassDescriptor descriptor = JpaHelper.getServerSession(getEMF()).getDescriptorForAlias("PhoneNumber");

        assertNotNull("No ClassDescriptor found for PhoneNumber", descriptor);
        assertEquals("PhoneNumber", descriptor.getAlias());
        assertNull(descriptor.getInheritancePolicyOrNull());

        // Check interfaces
        assertFalse(PersistenceWeaved.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceEntity.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceObject.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(FetchGroupTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedFetchGroups.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedLazy.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(ChangeTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedChangeTracking.class.isAssignableFrom(descriptor.getJavaClass()));
    }

    @Test
    public void verifyEmploymentPeriod() {
        ClassDescriptor descriptor = JpaHelper.getServerSession(getEMF()).getClassDescriptor(EmploymentPeriod.class);

        assertNotNull("No descriptor for EmploymentPeriod", descriptor);
        assertTrue("EmploymentPeriod is not an aggregate descriptor", descriptor.isAggregateDescriptor());

        // EclipseLinkJPAassertWoven(descriptor);
        assertNull(descriptor.getInheritancePolicyOrNull());

        // Check interfaces
        assertFalse(PersistenceWeaved.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceEntity.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceObject.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(FetchGroupTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedFetchGroups.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedLazy.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(ChangeTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedChangeTracking.class.isAssignableFrom(descriptor.getJavaClass()));
    }

    @Test
    public void verifyProjectDescriptor() {
        ClassDescriptor descriptor = JpaHelper.getServerSession(getEMF()).getDescriptorForAlias("Project");

        assertNotNull("No ClassDescriptor found for Project", descriptor);
        assertEquals("Project", descriptor.getAlias());
        assertNotNull(descriptor.getInheritancePolicyOrNull());

        // Check interfaces
        assertFalse(PersistenceWeaved.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceEntity.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceObject.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(FetchGroupTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedFetchGroups.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedLazy.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(ChangeTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedChangeTracking.class.isAssignableFrom(descriptor.getJavaClass()));
    }

    @Test
    public void verifySmallProjectDescriptor() {
        ClassDescriptor descriptor = JpaHelper.getServerSession(getEMF()).getDescriptorForAlias("SmallProject");

        assertNotNull("No ClassDescriptor found for SmallProject", descriptor);
        assertEquals("SmallProject", descriptor.getAlias());
        assertNotNull(descriptor.getInheritancePolicyOrNull());

        // Check interfaces
        assertFalse(PersistenceWeaved.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceEntity.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceObject.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(FetchGroupTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedFetchGroups.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedLazy.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(ChangeTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedChangeTracking.class.isAssignableFrom(descriptor.getJavaClass()));
    }

    @Test
    public void verifyLargeProjectDescriptor() {
        ClassDescriptor descriptor = JpaHelper.getServerSession(getEMF()).getDescriptorForAlias("LargeProject");

        assertNotNull("No ClassDescriptor found for LargeProject", descriptor);
        assertEquals("LargeProject", descriptor.getAlias());
        assertNotNull(descriptor.getInheritancePolicyOrNull());

        // Check interfaces
        assertFalse(PersistenceWeaved.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceEntity.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceObject.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(FetchGroupTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedFetchGroups.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedLazy.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(ChangeTracker.class.isAssignableFrom(descriptor.getJavaClass()));
        assertFalse(PersistenceWeavedChangeTracking.class.isAssignableFrom(descriptor.getJavaClass()));
    }

    /**
     * Handle the case where no
     */
    @Test
    public void verifyCompanyFilter_createEMWithoutUser() {
        EntityManager em = getEMF().createEntityManager();

        assertNotNull(em);

        try {
            em.createQuery("SELECT e FROM Employee e").getResultList();
        } catch (QueryException e) {
            if (e.getErrorCode() == 6170) {
                return;
            }
        }
        fail("QueryException(6170) not thrown for user lacking");
    }

    @Test
    public void verifyCompanyFilter_createEM_Admin_id() {
        EntityManager em = getEntityManager("admin", "password");

        assertNotNull(em);

        // Set tenant identifier in query since current user is admin
        em.setProperty("EMPLOYEE.TENANT_ID", "C1");
        
        TypedQuery<Employee> q = em.createQuery("SELECT e FROM Employee e", Employee.class);
        q.getResultList();

        User admin = getCurrentUser();
        assertNotNull("No current user found", admin);
        assertEquals("admin", admin.getId());
        assertEquals("Admin", admin.getFirstName());
        assertNull(admin.getLastName());
        assertTrue(admin.isAdministrator());
    }

    @Test
    public void verifyCompanyFilter_createEM_U1_id() {
        EntityManager em = getEntityManager("u1", "password");

        assertNotNull(em);

        // Need to perform an operation to have the user looked up and verified
        em.createQuery("SELECT e FROM Employee e").getResultList();

        User u1 = getCurrentUser();
        assertNotNull("No current user found", u1);
        assertEquals("u1", u1.getId());
        assertEquals("User", u1.getFirstName());
        assertEquals("One", u1.getLastName());
        assertFalse(u1.isAdministrator());
    }

    /**
     * Verify that a calling app can pass in a User instances
     */
    @Test
    public void verifyCompanyFilter_createEM_U1() {
        User user1 = findCachedUser("u1");
        assertNotNull(user1);

        EntityManager em = getEntityManager(user1.getId(), user1.getPassword());

        assertNotNull(em);

        // Need to perform an operation to have the user looked up and verified
        em.createQuery("SELECT e FROM Employee e").getResultList();

        User u1 = getCurrentUser();
        assertNotNull("No current user found", u1);
        assertEquals("u1", u1.getId());
        assertEquals("User", u1.getFirstName());
        assertEquals("One", u1.getLastName());
        assertFalse(u1.isAdministrator());

        assertSame(user1, u1);
    }

    /**
     * Ensure that queries for CompanyFiltered classes are not SQL where the
     * filters can be applied.
     */
    @Test
    public void verifyCompanyFilter_nativeQueriesFail() {
        EntityManager em = getEntityManager("u1", "password");

        assertNotNull(em);
        UnsupportedOperationException usoe = null;

        try {
            em.createNativeQuery("SELECT * FROM EMPLOYEE", Employee.class).getResultList();
        } catch (UnsupportedOperationException e) {
            usoe = e;
        }
        assertNotNull("UnsupportedOperationException expected for native query", usoe);
    }

}
