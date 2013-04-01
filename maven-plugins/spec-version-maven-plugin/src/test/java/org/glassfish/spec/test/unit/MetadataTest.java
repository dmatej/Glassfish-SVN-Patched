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

package org.glassfish.spec.test.unit;

import org.glassfish.spec.Artifact;
import org.glassfish.spec.ComplianceException;
import org.glassfish.spec.Metadata;
import org.glassfish.spec.test.sets.Courgette;
import org.glassfish.spec.test.sets.Womba;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Romain Grecourt
 */
public class MetadataTest {
    private static Artifact wombaArtifact;
    private static Artifact courgetteArtifact;
    
    @BeforeClass
    public static void init() {
        // womba is a non final API artifact
        wombaArtifact = new Artifact(
                Womba.GROUPID,
                Womba.ARTIFACTID,
                Womba.MAVEN_VERSION);
        
        // courgette is a final API artifact
        courgetteArtifact = new Artifact(
                Courgette.GROUPID,
                Courgette.ARTIFACTID,
                Courgette.MAVEN_VERSION);
        
        // TODO non final Standalone artifact
        // TODO final Standalone artifact
    }
    
    public static void positive(String key, String expected, String value){
        String msg = "Testing "+key;
        Assert.assertNotNull(msg,value);
        Assert.assertEquals(msg,expected,value);
    }
    
    @Test
    public void nonFinalAPITest() {
        Assert.assertEquals("womba is an API artifact",true,wombaArtifact.isAPI());
        Assert.assertEquals("womba is not final",false,wombaArtifact.isFinal());
        
        Metadata mdata = Metadata.generate(
                wombaArtifact,
                Womba.SPEC_VERSION,
                Womba.NEW_SPEC_VERSION,
                null);
        Assert.assertNotNull(mdata);
        
        positive(Metadata.BUNDLE_SYMBOLIC_NAME,Womba.BUNDLE_SYMBOLIC_NAME,mdata.getBundleSymbolicName());
        positive(Metadata.BUNDLE_SPEC_VERSION,Womba.BUNDLE_SPEC_VERSION,mdata.getBundleSpecVersion());
        positive(Metadata.BUNDLE_VERSION,Womba.BUNDLE_VERSION,mdata.getBundleVersion());
        positive(Metadata.JAR_EXTENSION_NAME,Womba.JAR_EXTENSION_NAME,mdata.getJarExtensionName());
        positive(Metadata.JAR_SPECIFICATION_VERSION,Womba.JAR_SPECIFICATION_VERSION,mdata.getJarSpecificationVersion());
        positive(Metadata.JAR_IMPLEMENTATION_VERSION,Womba.JAR_IMPLEMENTATION_VERSION,mdata.getjarImplementationVersion());
    }
    
    @Test
    public void finalAPITest() {
        Assert.assertEquals("courgette is an API artifact",true,courgetteArtifact.isAPI());
        Assert.assertEquals("courgette is final",true,courgetteArtifact.isFinal());
        
        Metadata mdata = Metadata.generate(
                courgetteArtifact,
                Courgette.SPEC_VERSION,
                null,
                Courgette.IMPL_VERSION);
        Assert.assertNotNull(mdata);
        
        positive(Metadata.BUNDLE_SYMBOLIC_NAME,Courgette.BUNDLE_SYMBOLIC_NAME,mdata.getBundleSymbolicName());
        positive(Metadata.BUNDLE_SPEC_VERSION,Courgette.BUNDLE_SPEC_VERSION,mdata.getBundleSpecVersion());
        positive(Metadata.BUNDLE_VERSION,Courgette.BUNDLE_VERSION,mdata.getBundleVersion());
        positive(Metadata.JAR_EXTENSION_NAME,Courgette.JAR_EXTENSION_NAME,mdata.getJarExtensionName());
        positive(Metadata.JAR_SPECIFICATION_VERSION,Courgette.JAR_SPECIFICATION_VERSION,mdata.getJarSpecificationVersion());
        positive(Metadata.JAR_IMPLEMENTATION_VERSION,Courgette.JAR_IMPLEMENTATION_VERSION,mdata.getjarImplementationVersion());
    }
    
    public static void negative(
            Artifact artifact,
            String version,
            String newVersion,
            String implVersion) {

        String msg = artifact + " - specVersion (" + version + ")"
                + " - newVersion (" + newVersion + ")"
                + " - implVersion (" + implVersion + ")"
                + " should not be compliant";
        try {
            Metadata.generate(artifact, version, newVersion, implVersion);
            Assert.fail(msg);
        } catch (ComplianceException cex) {
            Assert.assertFalse(msg, cex.isCompliant());
        }
    }
    
    @Test
    public void negativeNonFinalAPITest() {
        // test specVersion == newSpecVersion for non final API
        negative(wombaArtifact, Womba.SPEC_VERSION, Womba.SPEC_VERSION, null);
        
        // test with specVersion > newSpecVersion for non final API
        negative(wombaArtifact, Womba.NEW_SPEC_VERSION, Womba.SPEC_VERSION, null);
        
        // test specVersion with a big random number
        negative(wombaArtifact, "50", Womba.SPEC_VERSION, null);
    }
}