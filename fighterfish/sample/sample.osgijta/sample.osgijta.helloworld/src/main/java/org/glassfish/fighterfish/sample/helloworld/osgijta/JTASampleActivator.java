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


package org.glassfish.fighterfish.sample.helloworld.osgijta;

import javax.transaction.Synchronization;
import javax.transaction.TransactionSynchronizationRegistry;
import javax.transaction.UserTransaction;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

public class JTASampleActivator implements BundleActivator {

	@Override
	public void start(BundleContext context) throws Exception {
		debug("Started");
		ServiceReference txRef = context
				.getServiceReference(UserTransaction.class.getName());
		UserTransaction utx = (UserTransaction) context.getService(txRef);
		ServiceReference tsrRef = context
				.getServiceReference(TransactionSynchronizationRegistry.class
						.getName());
		TransactionSynchronizationRegistry tsr = (TransactionSynchronizationRegistry) context
				.getService(tsrRef);
		try {
			debug("Status: before utx.begin: "
					+ statusToString(utx.getStatus()));
			utx.begin();
			debug("Status: after utx.begin: " + statusToString(utx.getStatus()));

			// Get hold of JTA DataSource and do some operations using
			// connection obtained from there
			// ...

			// Let's listen to transaction life cycle event.
			tsr.registerInterposedSynchronization(new Synchronization() {
				public void beforeCompletion() {
					debug("beforeCompletion");
				}

				public void afterCompletion(int i) {
					debug("afterCompletion");
				}
			});

			utx.commit();
			debug("Transaction test completed");
		} catch (Exception e) {
			e.printStackTrace();
		}
		context.ungetService(txRef);
		context.ungetService(tsrRef);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		debug("Stopped");
	}

	/**
	 * Maps status integer as returned by getStatus() method to a String
	 */
	private static String statusToString(int status) {
		switch (status) {
		case 0:
			return "ACTIVE";
		case 1:
			return "MARKED_ROLLBACK";
		case 2:
			return "PREPARED";
		case 3:
			return "COMMITTED";
		case 4:
			return "ROLLEDBACK";
		case 5:
			return "UNKNOWN";
		case 6:
			return "NO_TRANSACTION";
		case 7:
			return "PREPARING";
		case 8:
			return "COMMITTING";
		case 9:
			return "ROLLING_BACK";
		default:
			return "NOT_YET_MAPPED";
		}
	}

	private void debug(String msg) {
		System.out.println("JTATestBundleActivator: " + msg);
	}
}