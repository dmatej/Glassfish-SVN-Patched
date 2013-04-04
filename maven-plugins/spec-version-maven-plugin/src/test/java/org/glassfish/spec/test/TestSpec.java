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
 * https://glassfish.java.net/public/CDDL+GPL_1_1.html
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
package org.glassfish.spec.test;

import java.io.File;
import java.io.IOException;
import java.util.jar.JarFile;
import org.glassfish.spec.Artifact;
import org.glassfish.spec.Metadata;
import org.glassfish.spec.Spec;
import org.junit.Assert;


/**
 *
 * @author Romain Grecourt
 */
public abstract class TestSpec extends Spec {

    public TestSpec(
            Artifact artifact,
            String specVersion,
            String newSpecVersion,
            String specImplVersion,
            String implVersion,
            String newImplVersion,
            String specBuild,
            String implBuild,
            String apiPackage,
            String implNamespace) {
        super(
                artifact,
                specVersion,
                newSpecVersion,
                specImplVersion,
                implVersion,
                newImplVersion,
                specBuild,
                implBuild,
                apiPackage,
                implNamespace);
    }
    
    public abstract String getExpectedBundleVersion();
    public abstract String getExpectedBundleSpecVersion();
    public abstract String getExpectedBundleSymbolicName();
    public abstract String getExpectedJarExtensionName();
    public abstract String getExpectedJarImplementationVersion();
    public abstract String getExpectedJarSpecificationVersion();
    public abstract String getJarPath();
    
    public Spec getSpec(){
        return this;
    }
    
    private static void positive(String key, String expected, String value){
        String msg = "Testing "+key;
        Assert.assertNotNull(msg,value);
        Assert.assertEquals(msg,expected,value);
    }
    
    public void assertMetadata(){
        Assert.assertNotNull(getMetadata());
        positive(
                Metadata.BUNDLE_SYMBOLIC_NAME,
                getExpectedBundleSymbolicName(),
                getMetadata().getBundleSymbolicName());
//        positive(
//                Metadata.BUNDLE_SPEC_VERSION,
//                getExpectedBundleSpecVersion(),
//                getMetadata().getBundleSpecVersion());
        positive(
                Metadata.BUNDLE_VERSION,
                getExpectedBundleVersion(),
                getMetadata().getBundleVersion());
        positive(
                Metadata.JAR_EXTENSION_NAME,
                getExpectedJarExtensionName(),
                getMetadata().getJarExtensionName());
        positive(
                Metadata.JAR_SPECIFICATION_VERSION,
                getExpectedJarSpecificationVersion(),
                getMetadata().getJarSpecificationVersion());
        positive(
                Metadata.JAR_IMPLEMENTATION_VERSION,
                getExpectedJarImplementationVersion(),
                getMetadata().getjarImplementationVersion());
    }
    
    public void assertMetadataFromJar() {
        try {
            File f = new File(getJarPath());
            Assert.assertTrue(
                    "test that " 
                    + f.getCanonicalPath() 
                    + " exists",
                    f.exists());
            read(new JarFile(f));
            assertMetadata();
        } catch (IOException ioe) {
            Assert.fail(ioe.getMessage());
        }
    }
}
