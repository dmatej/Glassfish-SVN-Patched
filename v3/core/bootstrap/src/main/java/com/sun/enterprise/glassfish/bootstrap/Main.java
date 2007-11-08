/*
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the License).  You may not use this file except in
 * compliance with the License.
 * 
 * You can obtain a copy of the license at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html or
 * glassfish/bootstrap/legal/CDDLv1.0.txt.
 * See the License for the specific language governing 
 * permissions and limitations under the License.
 * 
 * When distributing Covered Code, include this CDDL 
 * Header Notice in each file and include the License file 
 * at glassfish/bootstrap/legal/CDDLv1.0.txt.  
 * If applicable, add the following below the CDDL Header, 
 * with the fields enclosed by brackets [] replaced by
 * you own identifying information: 
 * "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Copyright 2006 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.enterprise.glassfish.bootstrap;

import com.sun.enterprise.module.ModulesRegistry;
import com.sun.enterprise.module.Repository;
import com.sun.enterprise.module.Module;
import com.sun.enterprise.module.bootstrap.BootException;

import java.io.File;
import java.io.IOException;
import java.net.URLClassLoader;
import java.net.URL;
import java.net.URI;
import java.util.List;
import java.util.ArrayList;

/**
 * Tag Main to get the manifest file 
 */
public class Main extends com.sun.enterprise.module.bootstrap.Main {

    public static void main(final String args[]) {
        (new Main()).run(args);   
    }

    @Override
    protected void createRepository(File root, ModulesRegistry modulesRegistry) throws BootException {
        try {
            Repository lib = new GFRepository("lib", root);
            lib.initialize();
            modulesRegistry.addRepository(lib);

            modulesRegistry.setParentClassLoader(this.getClass().getClassLoader());
            // mask the JAXB and JAX-WS API in the bootstrap classloader so that
            // we get to load our copies in the modules
            Module shared = modulesRegistry.makeModuleFor("com.sun.enterprise.glassfish:glassfish-jaxb", null);

            if (shared==null) {
                // we don't ship jaxb as part of this distribution, just return.
                return;
            }
            List<URL> urls = new ArrayList<URL>();
            for (URI location : shared.getModuleDefinition().getLocations()) {
                urls.add(location.toURL());
            }


            modulesRegistry.setParentClassLoader(new MaskingClassLoader(
                this.getClass().getClassLoader(),
                urls.toArray(new URL[0]),
                "javax.xml.bind.",
                "javax.xml.ws.",
                "com.sun.xml."
            ));


            //modulesRegistry.setParentClassLoader(shared.getClassLoader());

        } catch(IOException ioe) {
            throw new BootException("Error while initializing lib repository at : "+root, ioe);
        }
    }

}
