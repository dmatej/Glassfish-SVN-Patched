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

package com.sun.enterprise.v3.bootstrap;

import com.sun.enterprise.module.ManifestConstants;
import com.sun.enterprise.module.ModuleDefinition;
import com.sun.enterprise.module.ModuleDependency;
import com.sun.enterprise.module.impl.*;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.Attributes;
import java.net.URI;

/**
 *
 * @author dochez
 */
public class AppserverCookedRepository extends CookedLibRepository {
    
    private List<ModuleDefinition> moduleDefs = new ArrayList<ModuleDefinition>();
    
    /** Creates a new instance of WebServicesRepository */
    public AppserverCookedRepository(String installRoot) {
        super(installRoot + File.separatorChar + "lib");
    }
    
    /**
     * Initialize the repository for use. This need to be called at least
     * once before any find methods is invoked.
     * @throws IOException if an error occur accessing the repository
     */
    public void initialize() throws IOException {
        
        Attributes sharedAttr = new Attributes();
        sharedAttr.putValue(ManifestConstants.BUNDLE_NAME.toString(), "shared");

        CookedModuleDefinition shared = new CookedModuleDefinition(
                new File(rootLocation, "javaee.jar"), sharedAttr);

        List<URI> sharedClassPath = new ArrayList<URI>();
        // mandatory override of the jaxb libraries so we never use
        // the jdk ones if they are present.
        File jars = new File(rootLocation, "jars");
        File jaxb_api = new File(jars, "jaxb-api-2.1.jar");
        sharedClassPath.add(jaxb_api.toURI());
        File jaxb_impl = new  File(jars, "jaxb-impl-2.1-SNAPSHOT.jar");
        sharedClassPath.add(jaxb_impl.toURI());

        // shared librairies
        File sharedLib = new File(rootLocation, "shared");
        if (sharedLib.exists()) {
            for (File sharedFile : sharedLib.listFiles()) {
                sharedClassPath.add(sharedFile.toURI());
            }
        }

        // derby database
        File derbyLib = rootLocation.getParentFile();
        derbyLib = new File(derbyLib, "javadb");
        derbyLib = new File(derbyLib, "lib");
        File derby = new File(derbyLib, "derby.jar");
        sharedClassPath.add(derby.toURI());
        File derbyclient = new File(derbyLib, "derbyclient.jar");
        sharedClassPath.add(derbyclient.toURI());

        shared.add(sharedClassPath);
        moduleDefs.add(shared);       
    }
    
    public List<ModuleDefinition> findAll() {
        return moduleDefs;
    }
}
