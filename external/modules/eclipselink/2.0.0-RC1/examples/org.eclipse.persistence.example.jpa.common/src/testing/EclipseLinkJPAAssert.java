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

/**
 * 
 * 
 * @author dclarke
 * @since EclipseLink 1.2
 */
public abstract class EclipseLinkJPAAssert {

    public static void assertIsWoven(EntityManagerFactory emf, String entityTypeName) {

    }

    public static void assertIsLazy(EntityManagerFactory emf, String entityTypeName, String attributeName) {

    }
}
