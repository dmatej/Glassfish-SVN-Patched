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
 *     dclarke - initial GeoNames EclipseLink JPA example
 ******************************************************************************/
package testing;

import javax.persistence.EntityManagerFactory;

import org.junit.AfterClass;
import org.junit.BeforeClass;

import utils.PersistenceHelper;

/**
 * Base test case providing support for creation of the JavaSE EMF.
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public abstract class BaseTestCase {
	private static EntityManagerFactory emf;

	public static EntityManagerFactory getEMF() {
		return emf;
	}
	
	@BeforeClass
	public static void createEMF() throws Exception {
		emf = PersistenceHelper.createEMF(null, true);
	}
	
	@AfterClass
	public static void closeEMF() throws Exception {
		emf.close();
	}
}
