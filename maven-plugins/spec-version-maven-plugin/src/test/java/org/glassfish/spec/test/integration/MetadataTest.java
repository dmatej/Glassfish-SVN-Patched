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

package org.glassfish.spec.test.integration;

import java.io.File;
import java.io.IOException;
import java.util.jar.JarFile;
import org.glassfish.spec.Metadata;
import org.glassfish.spec.test.sets.Courgette;
import org.glassfish.spec.test.sets.Ratatouille;
import org.glassfish.spec.test.sets.Womba;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Romain Grecourt
 */
public class MetadataTest {
    
    private static Metadata wombaMdata;
    private static Metadata courgetteMdata;
    private static Metadata ratatouilleMdata;
    
    @BeforeClass
    public static void init() throws IOException{
        File f = new File(Womba.JAR);
        Assert.assertTrue("test that "+f.getCanonicalPath()+" exists", f.exists());
        wombaMdata = Metadata.fromJar(new JarFile(f));
        
        File g = new File(Courgette.JAR);
        Assert.assertTrue("test that "+g.getCanonicalPath()+" exists", g.exists());
        courgetteMdata = Metadata.fromJar(new JarFile(g));
        
        File h = new File(Ratatouille.JAR);
        Assert.assertTrue("test that "+h.getCanonicalPath()+" exists", g.exists());
        ratatouilleMdata = Metadata.fromJar(new JarFile(g));
    }
    
    @Test
    public void verifyWombaMetadata() {
        String msg = "Testing "+Metadata.BUNDLE_VERSION;
        Assert.assertNotNull(msg, wombaMdata.getBundleVersion());
        Assert.assertEquals(msg, Womba.BUNDLE_VERSION, wombaMdata.getBundleVersion());
        
        msg = "Testing "+Metadata.BUNDLE_SYMBOLIC_NAME;
        Assert.assertNotNull(msg, wombaMdata.getBundleSymbolicName());
        Assert.assertEquals(msg, Womba.BUNDLE_SYMBOLIC_NAME, wombaMdata.getBundleSymbolicName());
        
        msg = "Testing "+Metadata.JAR_EXTENSION_NAME;
        Assert.assertNotNull(msg, wombaMdata.getJarExtensionName());
        Assert.assertEquals(msg, Womba.JAR_EXTENSION_NAME, wombaMdata.getJarExtensionName());
        
        msg = "Testing "+Metadata.JAR_SPECIFICATION_VERSION;
        Assert.assertNotNull(msg,wombaMdata.getJarSpecificationVersion());
        Assert.assertEquals(msg,Womba.JAR_SPECIFICATION_VERSION, wombaMdata.getJarSpecificationVersion());
        
        msg = "Testing "+Metadata.JAR_IMPLEMENTATION_VERSION;
        Assert.assertNotNull(msg,wombaMdata.getjarImplementationVersion());
        Assert.assertEquals(msg,Womba.JAR_IMPLEMENTATION_VERSION, wombaMdata.getjarImplementationVersion());
    }
    
    @Test
    public void verifyCourgetteMetadata() {
        String msg = "Testing "+Metadata.BUNDLE_VERSION;
        Assert.assertNotNull(msg, courgetteMdata.getBundleVersion());
        Assert.assertEquals(msg, Courgette.BUNDLE_VERSION, courgetteMdata.getBundleVersion());
        
        msg = "Testing "+Metadata.BUNDLE_SYMBOLIC_NAME;
        Assert.assertNotNull(msg, courgetteMdata.getBundleSymbolicName());
        Assert.assertEquals(msg, Courgette.BUNDLE_SYMBOLIC_NAME, courgetteMdata.getBundleSymbolicName());
        
        msg = "Testing "+Metadata.JAR_EXTENSION_NAME;
        Assert.assertNotNull(msg, courgetteMdata.getJarExtensionName());
        Assert.assertEquals(msg, Courgette.JAR_EXTENSION_NAME, courgetteMdata.getJarExtensionName());
        
        msg = "Testing "+Metadata.JAR_SPECIFICATION_VERSION;
        Assert.assertNotNull(msg,courgetteMdata.getJarSpecificationVersion());
        Assert.assertEquals(msg,Courgette.JAR_SPECIFICATION_VERSION, courgetteMdata.getJarSpecificationVersion());
        
        msg = "Testing "+Metadata.JAR_IMPLEMENTATION_VERSION;
        Assert.assertNotNull(msg,courgetteMdata.getjarImplementationVersion());
        Assert.assertEquals(msg,Courgette.JAR_IMPLEMENTATION_VERSION, courgetteMdata.getjarImplementationVersion());
    }
    
    @Test
    public void verifyRatatouilleMetadata() {
        String msg = "Testing "+Metadata.BUNDLE_VERSION;
        Assert.assertNotNull(msg, ratatouilleMdata.getBundleVersion());
        Assert.assertEquals(msg, Courgette.BUNDLE_VERSION, ratatouilleMdata.getBundleVersion());
        
        msg = "Testing "+Metadata.BUNDLE_SYMBOLIC_NAME;
        Assert.assertNotNull(msg, ratatouilleMdata.getBundleSymbolicName());
        Assert.assertEquals(msg, Courgette.BUNDLE_SYMBOLIC_NAME, ratatouilleMdata.getBundleSymbolicName());
        
        msg = "Testing "+Metadata.JAR_EXTENSION_NAME;
        Assert.assertNotNull(msg, ratatouilleMdata.getJarExtensionName());
        Assert.assertEquals(msg, Courgette.JAR_EXTENSION_NAME, ratatouilleMdata.getJarExtensionName());
        
        msg = "Testing "+Metadata.JAR_SPECIFICATION_VERSION;
        Assert.assertNotNull(msg,ratatouilleMdata.getJarSpecificationVersion());
        Assert.assertEquals(msg,Courgette.JAR_SPECIFICATION_VERSION, ratatouilleMdata.getJarSpecificationVersion());
        
        msg = "Testing "+Metadata.JAR_IMPLEMENTATION_VERSION;
        Assert.assertNotNull(msg,ratatouilleMdata.getjarImplementationVersion());
        Assert.assertEquals(msg,Courgette.JAR_IMPLEMENTATION_VERSION, ratatouilleMdata.getjarImplementationVersion());
    }
}