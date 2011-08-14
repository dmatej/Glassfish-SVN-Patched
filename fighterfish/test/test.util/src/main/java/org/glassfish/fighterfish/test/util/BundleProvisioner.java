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

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class BundleProvisioner {
    /**
     * List of bundles installed by a test method
     */
    private List<Bundle> testBundles = new ArrayList<Bundle>();

    private BundleContext ctx;

    private Logger logger = Logger.getLogger(getClass().getPackage().getName());

    public BundleProvisioner(BundleContext ctx) {
        this.ctx = ctx;
    }

    /**
     * Install a bundle and add it to the list of bundles.
     *
     * @param location
     * @throws BundleException
     */
    protected Bundle installTestBundle(String location) throws BundleException {
        logger.logp(Level.INFO, "AbstractTestObject", "installTestBundle", "Installing bundle = {0}", new Object[]{location});
        final Bundle bundle = ctx.installBundle(location);
        testBundles.add(bundle);
        logger.logp(Level.INFO, "AbstractTestObject", "installTestBundle", "Installed bundle = {0} from {1} ", new Object[]{bundle, location});
        return bundle;
    }

    /**
     * Uninstall a bundle if it has been installed by
     *
     * @param bundle
     * @throws BundleException
     */
    protected void uninstallTestBundle(Bundle bundle) throws BundleException {
        if (testBundles.remove(bundle)) {
            logger.logp(Level.INFO, "AbstractTestObject", "uninstallTestBundle", "Uninstalling bundle = {0}", new Object[]{bundle});
            bundle.uninstall();
            logger.logp(Level.INFO, "AbstractTestObject", "uninstallTestBundle", "Uninstalled bundle = {0}", new Object[]{bundle});
        } else {
            throw new RuntimeException(bundle + " is not a test bundle");
        }
    }

    protected void uninstallAllTestBundles() throws BundleException {
        // take a copy because uninstallTstBundle removes from this list
        for (Bundle b : testBundles.toArray(new Bundle[0])) {
            uninstallTestBundle(b);
        }
    }

}
