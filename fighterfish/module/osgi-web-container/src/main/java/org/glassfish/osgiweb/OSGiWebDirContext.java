/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
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


package org.glassfish.osgiweb;

import org.apache.naming.resources.WebDirContext;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * As per WAB spec, resources must not be allowed from OSGI-INF or OSGI-OPT dirs.
 * So, we install a special dir context that takes care of this requirement.
 *
 * @author sanjeeb.sahoo@oracle.com
 */
class OSGiWebDirContext extends WebDirContext {

    private static Logger logger = Logger.getLogger(OSGiWebDirContext.class.getPackage().getName());

    @Override
    protected File file(String name) {
        final String s = name.toUpperCase();
        if (s.startsWith("/OSGI-INF/") || s.startsWith("/OSGI-OPT/")) {
            logger.logp(Level.FINE, getClass().getSimpleName(), "file",
                    "Forbidding access to resource called {0}", new Object[]{name});
            return null;
        } else {
            return super.file(name);
        }
    }
}
