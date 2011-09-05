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


package org.glassfish.fighterfish.test.util;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

import java.util.concurrent.TimeUnit;

/**
 * This class is used by tests to deploy Ejb Bundles. Since EJB deployment happens asynchronously when an EJB Bundle
 * is activated, for a test case to know whether the deployment is successful or not is not as simple as checking if
 * b.start() returns succesfully or not. This is where this class is helpful. Unlike OSGi Web Application container,
 * OSGi EJB Spec does not raise events to indicating success or failure of events. So, this class relies on user to
 * tell it at least one service that's being exported by this bundle to OSGi service registry. This class then
 * uses a service tracker to wait for such a service to appear. If such a service does not show up in a specified
 * amount of time, it times out the deployment operation.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class EjbBundle {
    private BundleContext ctx;
    private Bundle b;
    private String[] services;

    /**
     * A handle to the EJB bundle being deployed.
     * @param ctx BundleContext of test used for various OSGi operation.
     * @param b Bundle to be deployed.
     * @param services Services that are expected to be made available by this EJB bundle if deployment is successful.
     */
    public EjbBundle(BundleContext ctx, Bundle b, String[] services) {
        this.ctx = ctx;
        this.b = b;
        this.services = services;
    }

    /**
     * Deploy the given EJB OSGi bundle. Deployment is triggered asynchronously by starting the bundle. If none of the
     * user specified services show up in service registry in the specified amount of time, it assumes the operation
     * has failed and throws TimeoutOperation.
     *
     * @param timeout
     * @param timeUnit
     * @throws BundleException
     * @throws InterruptedException
     * @throws TimeoutException
     */
    public void deploy(long timeout, TimeUnit timeUnit) throws BundleException, InterruptedException, TimeoutException {
        b.start(Bundle.START_TRANSIENT);
        for (String service : services) {
            if (OSGiUtil.waitForService(ctx, b, service, timeUnit.toMillis(timeout)) == null) {
                throw new TimeoutException("Deployment timed out. No service of type " + service + " found.");
            }
        }
    }

    /**
     * Undeploy the EJB OSGi bundle. There is no need for any timeout argument, as undeployment is a synchronous
     * process unlike deployment.
     * @throws BundleException
     */
    public void undeploy() throws BundleException {
        b.stop();
    }

    public Bundle getBundle() {
        return b;
    }
}
