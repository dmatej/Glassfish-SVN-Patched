/*******************************************************************************
 * Copyright (c) 1998, 2008 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Bug 277731: Simple Map Dynamic JPA Example
 *               http://wiki.eclipse.org/EclipseLink/Examples/JPA/Dynamic/SimpleDynamicMap
 ******************************************************************************/
package example.jpa;

import javax.persistence.EntityManagerFactory;

import model.DynamicMapEntity;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.Session;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.1.1
 */
public class DynamicMapHelper {

    /**
     * Lookup descriptor for a dynamic type based on its name (descriptor alias)
     */
    public static ClassDescriptor getDescriptor(EntityManagerFactory emf, String typeName) {
        ClassDescriptor descriptor = JpaHelper.getServerSession(emf).getClassDescriptorForAlias(typeName);

        if (descriptor == null) {
            throw new IllegalArgumentException("No descriptor found for type: " + typeName);
        }

        return descriptor;
    }

    public static Class getClass(EntityManagerFactory emf, String typeName) {
        return getDescriptor(emf, typeName).getJavaClass();
    }

    /**
     * Factory for creating new dynamic instances 
     */
    public static DynamicMapEntity newInstance(Session session, String typeName) {
        ClassDescriptor descriptor = session.getClassDescriptorForAlias(typeName);
        return (DynamicMapEntity) descriptor.getInstantiationPolicy().buildNewInstance();
    }

    /**
     * Factory for creating new dynamic JPA instances 
     */
    public static DynamicMapEntity newInstance(EntityManagerFactory emf, String typeName) {
        return newInstance(JpaHelper.getServerSession(emf), typeName);
    }

}
