/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */


package org.glassfish.fighterfish.sample.uas.entities;

import java.util.Properties;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

/**
 * This activator is responsible for registering an EMF as an OSGi service
 * 
 * @author Sanjeeb Sahoo
 *
 */
public class EntitiesActivator implements BundleActivator {

	@Override
	public void start(BundleContext context) throws Exception {
		final String puName = "sample.uas.entities";
		
		EntityManagerFactory emf = createEMF(puName);
		Properties props = new Properties();
		props.setProperty("persistence-unit", puName);
		context.registerService(EntityManagerFactory.class.getName(), emf, props);
		log("registered " + emf);
	}

	private EntityManagerFactory createEMF(final String puName) {
		ClassLoader old = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getClass().getClassLoader());
			EntityManagerFactory emf = Persistence.createEntityManagerFactory(puName);

            // createEMF does not cause java2db to happen - at least that's the behavior in EclipseLink.
            // so, calling createEM will force java2db to happen.
            // If we don't java2db here, it will happen the first time someone uses our EMF and that could
            // be part of a transaction and we can get into deadlocks based on RDBMS trype.
            emf.createEntityManager().close();
			return emf;
		} finally {
			Thread.currentThread().setContextClassLoader(old);
		}
	}

	@Override
	public void stop(BundleContext context) throws Exception {
	}
	
	private void log(String msg) {
		System.out.println("EntitiesActivator: " + msg);
	}

}
