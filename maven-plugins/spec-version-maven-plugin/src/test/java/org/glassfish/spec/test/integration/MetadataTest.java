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
import org.glassfish.spec.test.sets.Aubergine;
import org.glassfish.spec.test.sets.Moussaka;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Romain Grecourt
 */
public class MetadataTest {
    
    private static Metadata aubergineMdata;
    private static Metadata courgetteMdata;
    private static Metadata ratatouilleMdata;
    private static Metadata moussakaMdata;
    
    @BeforeClass
    public static void init() throws IOException{
        File f = new File(Aubergine.JAR);
        Assert.assertTrue("test that "+f.getCanonicalPath()+" exists", f.exists());
        aubergineMdata = Metadata.fromJar(new JarFile(f));
        
        File g = new File(Courgette.JAR);
        Assert.assertTrue("test that "+g.getCanonicalPath()+" exists", g.exists());
        courgetteMdata = Metadata.fromJar(new JarFile(g));
        
        File h = new File(Ratatouille.JAR);
        Assert.assertTrue("test that "+h.getCanonicalPath()+" exists", g.exists());
        ratatouilleMdata = Metadata.fromJar(new JarFile(h));
        
        File i = new File(Moussaka.JAR);
        Assert.assertTrue("test that "+h.getCanonicalPath()+" exists", g.exists());
        moussakaMdata = Metadata.fromJar(new JarFile(i));
    }
    
    @Test
    public void verifyAubergineMetadata() {
        String msg = "Testing "+Metadata.BUNDLE_VERSION;
        Assert.assertNotNull(msg, aubergineMdata.getBundleVersion());
        Assert.assertEquals(msg, Aubergine.BUNDLE_VERSION, aubergineMdata.getBundleVersion());
        
        msg = "Testing "+Metadata.BUNDLE_SYMBOLIC_NAME;
        Assert.assertNotNull(msg, aubergineMdata.getBundleSymbolicName());
        Assert.assertEquals(msg, Aubergine.BUNDLE_SYMBOLIC_NAME, aubergineMdata.getBundleSymbolicName());
        
        msg = "Testing "+Metadata.JAR_EXTENSION_NAME;
        Assert.assertNotNull(msg, aubergineMdata.getJarExtensionName());
        Assert.assertEquals(msg, Aubergine.JAR_EXTENSION_NAME, aubergineMdata.getJarExtensionName());
        
        msg = "Testing "+Metadata.JAR_SPECIFICATION_VERSION;
        Assert.assertNotNull(msg,aubergineMdata.getJarSpecificationVersion());
        Assert.assertEquals(msg,Aubergine.JAR_SPECIFICATION_VERSION, aubergineMdata.getJarSpecificationVersion());
        
        msg = "Testing "+Metadata.JAR_IMPLEMENTATION_VERSION;
        Assert.assertNotNull(msg,aubergineMdata.getjarImplementationVersion());
        Assert.assertEquals(msg,Aubergine.JAR_IMPLEMENTATION_VERSION, aubergineMdata.getjarImplementationVersion());
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
        Assert.assertEquals(msg, Ratatouille.BUNDLE_VERSION, ratatouilleMdata.getBundleVersion());
        
        msg = "Testing "+Metadata.BUNDLE_SYMBOLIC_NAME;
        Assert.assertNotNull(msg, ratatouilleMdata.getBundleSymbolicName());
        Assert.assertEquals(msg, Ratatouille.BUNDLE_SYMBOLIC_NAME, ratatouilleMdata.getBundleSymbolicName());
        
        msg = "Testing "+Metadata.JAR_EXTENSION_NAME;
        Assert.assertNotNull(msg, ratatouilleMdata.getJarExtensionName());
        Assert.assertEquals(msg, Ratatouille.JAR_EXTENSION_NAME, ratatouilleMdata.getJarExtensionName());
        
        msg = "Testing "+Metadata.JAR_SPECIFICATION_VERSION;
        Assert.assertNotNull(msg,ratatouilleMdata.getJarSpecificationVersion());
        Assert.assertEquals(msg,Ratatouille.JAR_SPECIFICATION_VERSION, ratatouilleMdata.getJarSpecificationVersion());
        
        msg = "Testing "+Metadata.JAR_IMPLEMENTATION_VERSION;
        Assert.assertNotNull(msg,ratatouilleMdata.getjarImplementationVersion());
        Assert.assertEquals(msg,Ratatouille.JAR_IMPLEMENTATION_VERSION, ratatouilleMdata.getjarImplementationVersion());
    }
    
    @Test
    public void verifyMoussakaMetadata() {
        String msg = "Testing "+Metadata.BUNDLE_VERSION;
        Assert.assertNotNull(msg, moussakaMdata.getBundleVersion());
        Assert.assertEquals(msg, Moussaka.BUNDLE_VERSION, moussakaMdata.getBundleVersion());
        
        msg = "Testing "+Metadata.BUNDLE_SYMBOLIC_NAME;
        Assert.assertNotNull(msg, moussakaMdata.getBundleSymbolicName());
        Assert.assertEquals(msg, Moussaka.BUNDLE_SYMBOLIC_NAME, moussakaMdata.getBundleSymbolicName());
        
        msg = "Testing "+Metadata.JAR_EXTENSION_NAME;
        Assert.assertNotNull(msg, moussakaMdata.getJarExtensionName());
        Assert.assertEquals(msg, Moussaka.JAR_EXTENSION_NAME, moussakaMdata.getJarExtensionName());
        
        msg = "Testing "+Metadata.JAR_SPECIFICATION_VERSION;
        Assert.assertNotNull(msg,moussakaMdata.getJarSpecificationVersion());
        Assert.assertEquals(msg,Moussaka.JAR_SPECIFICATION_VERSION, moussakaMdata.getJarSpecificationVersion());
        
        msg = "Testing "+Metadata.JAR_IMPLEMENTATION_VERSION;
        Assert.assertNotNull(msg,moussakaMdata.getjarImplementationVersion());
        Assert.assertEquals(msg,Moussaka.JAR_IMPLEMENTATION_VERSION, moussakaMdata.getjarImplementationVersion());
    } 
}