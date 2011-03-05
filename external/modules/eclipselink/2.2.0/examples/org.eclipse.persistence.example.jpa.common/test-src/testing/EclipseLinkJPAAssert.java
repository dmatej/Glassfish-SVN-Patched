/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
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

import javax.persistence.EntityManagerFactory;

import junit.framework.Assert;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.internal.descriptors.PersistenceEntity;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.mappings.DatabaseMapping;
import org.eclipse.persistence.mappings.ForeignReferenceMapping;

/**
 * Testing utility to assert various EclipseLink configurations in the
 * descriptors and mappings.
 * 
 * @author dclarke
 * @since EclipseLink 2.0
 */
public abstract class EclipseLinkJPAAssert {

    public static ClassDescriptor assertEntity(EntityManagerFactory emf, String entityTypeName) {
        ClassDescriptor descriptor = JpaHelper.getServerSession(emf).getDescriptorForAlias(entityTypeName);

        Assert.assertNotNull("No ClassDescriptor found for: " + entityTypeName, descriptor);
        return descriptor;
    }

    public static void assertWoven(ClassDescriptor descriptor) {
        Assert.assertTrue(PersistenceEntity.class.isAssignableFrom(descriptor.getJavaClass()));
    }

    public static void assertWoven(EntityManagerFactory emf, String entityTypeName) {
        assertWoven(assertEntity(emf, entityTypeName));
    }

    public static DatabaseMapping assertMapping(ClassDescriptor descriptor, String attributeName) {
        DatabaseMapping mapping = descriptor.getMappingForAttributeName(attributeName);

        Assert.assertNotNull("No mapping found on " + descriptor + " for attribute named: " + attributeName, mapping);
        return mapping;
    }

    public static DatabaseMapping assertMapping(EntityManagerFactory emf, String entityTypeName, String attributeName) {
        return assertMapping(assertEntity(emf, entityTypeName), attributeName);
    }

    public static void assertLazy(ClassDescriptor descriptor, String attributeName) {
        DatabaseMapping mapping = assertMapping(descriptor, attributeName);

        Assert.assertTrue(mapping.isLazy());
    }

    public static void assertLazy(EntityManagerFactory emf, String entityTypeName, String attributeName) {
        assertLazy(assertEntity(emf, entityTypeName), attributeName);
    }

    public static ForeignReferenceMapping assertRelationship(ClassDescriptor descriptor, String attributeName) {
        DatabaseMapping mapping = assertMapping(descriptor, attributeName);

        Assert.assertTrue(mapping.isForeignReferenceMapping());

        return (ForeignReferenceMapping) mapping;
    }

    public static ForeignReferenceMapping assertRelationship(EntityManagerFactory emf, String entityTypeName, String attributeName) {
        return assertRelationship(assertEntity(emf, entityTypeName), attributeName);
    }

    public static void assertPrivateOwned(ClassDescriptor descriptor, String attributeName) {
        ForeignReferenceMapping mapping = assertRelationship(descriptor, attributeName);
        Assert.assertTrue(mapping.isPrivateOwned());
    }

    public static void assertPrivateOwned(EntityManagerFactory emf, String entityTypeName, String attributeName) {
        assertPrivateOwned(assertEntity(emf, entityTypeName), attributeName);
    }
}
