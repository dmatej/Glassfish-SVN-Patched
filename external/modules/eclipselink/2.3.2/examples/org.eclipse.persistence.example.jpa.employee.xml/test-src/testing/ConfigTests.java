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
 * 		dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package testing;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import javax.persistence.PersistenceContext;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.junit.Test;

/**
 * Set of tests to ensure the mappings are properly populated from the provided
 * annotations/xml.
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
@PersistenceContext(unitName = "employee-xml")
public class ConfigTests extends EclipseLinkJPATest {

    @Test
    public void verifyEmployeeDescriptor() throws Exception {
        ClassDescriptor descriptor = EclipseLinkJPAAssert.assertEntity(getEMF(), "Employee");
        EclipseLinkJPAAssert.assertWoven(descriptor);
        assertEquals("Employee", descriptor.getAlias());
        assertNull(descriptor.getInheritancePolicyOrNull());

        // Address Mapping
        EclipseLinkJPAAssert.assertLazy(descriptor, "address");
        EclipseLinkJPAAssert.assertPrivateOwned(descriptor, "address");

        // Manager Mapping
        EclipseLinkJPAAssert.assertLazy(descriptor, "manager");

        // PhoenNumber Mapping
        EclipseLinkJPAAssert.assertLazy(descriptor, "phoneNumbers");
        EclipseLinkJPAAssert.assertPrivateOwned(descriptor, "phoneNumbers");
    }

    @Test
    public void verifyAddressDescriptor() throws Exception {
        ClassDescriptor descriptor = EclipseLinkJPAAssert.assertEntity(getEMF(), "Address");
        EclipseLinkJPAAssert.assertWoven(descriptor);
        assertEquals("Address", descriptor.getAlias());
        assertNull(descriptor.getInheritancePolicyOrNull());
    }

    // TODO @Test
    public void verifyPhoneNumberDescriptor() {        
        ClassDescriptor descriptor = EclipseLinkJPAAssert.assertEntity(getEMF(), "PhoneNumber");
        EclipseLinkJPAAssert.assertWoven(descriptor);
        assertEquals("PhoneNumber", descriptor.getAlias());
        assertNull(descriptor.getInheritancePolicyOrNull());
    }

    @Test
    public void verifyProjectDescriptor() {
        ClassDescriptor descriptor = EclipseLinkJPAAssert.assertEntity(getEMF(), "Project");
        EclipseLinkJPAAssert.assertWoven(descriptor);
        assertEquals("Project", descriptor.getAlias());
        assertNotNull(descriptor.getInheritancePolicyOrNull());
    }

    @Test
    public void verifySmallProjectDescriptor() {
        ClassDescriptor descriptor = EclipseLinkJPAAssert.assertEntity(getEMF(), "SmallProject");
        EclipseLinkJPAAssert.assertWoven(descriptor);
        assertEquals("SmallProject", descriptor.getAlias());
        assertNotNull(descriptor.getInheritancePolicyOrNull());
    }

    @Test
    public void verifyLargeProjectDescriptor() {
        ClassDescriptor descriptor = EclipseLinkJPAAssert.assertEntity(getEMF(), "LargeProject");
        EclipseLinkJPAAssert.assertWoven(descriptor);
        assertEquals("LargeProject", descriptor.getAlias());
        assertNotNull(descriptor.getInheritancePolicyOrNull());
    }

}
