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
 *     dclarke - Initial JPA Proxy Authentication demo
 ******************************************************************************/
package testing;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.*;

import javax.persistence.EntityManager;

import oracle.jdbc.pool.OracleOCIConnectionPool;

import org.junit.Test;

public class JavaSEProxyTests extends EMFHelper {

	@Test
	public void findAll() throws Exception {
		assertNotNull(getEMF());

		EntityManager em = getEMF().createEntityManager();

		assertNotNull(em);

		List emps = em.createQuery("SELECT e FROM Employee e").getResultList();

		assertNotNull(emps);
		assertFalse(emps.isEmpty());

		em.close();
	}

	@Test
	public void proxyAuthenticationExample() {
		Map loginProps = new HashMap();
		loginProps.put(OracleOCIConnectionPool.PROXYTYPE,
				OracleOCIConnectionPool.PROXYTYPE_USER_NAME);
		loginProps.put(OracleOCIConnectionPool.PROXY_USER_NAME, "sarah");

		Map emProps = new HashMap();
		emProps.put("TODO", loginProps);

		EntityManager em = getEMF().createEntityManager(emProps);

		//em.find(Employee.class, 1);

		em.close();
	}
}
