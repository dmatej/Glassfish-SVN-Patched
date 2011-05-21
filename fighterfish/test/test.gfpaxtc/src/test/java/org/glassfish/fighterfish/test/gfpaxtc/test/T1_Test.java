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


package org.glassfish.fighterfish.test.gfpaxtc.test;

import org.glassfish.embeddable.GlassFish;
import org.glassfish.embeddable.GlassFishException;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ops4j.pax.exam.Option;
import org.ops4j.pax.exam.junit.Configuration;
import org.ops4j.pax.exam.junit.ExamReactorStrategy;
import org.ops4j.pax.exam.junit.JUnit4TestRunner;
import org.ops4j.pax.exam.spi.reactors.AllConfinedStagedReactorFactory;
import org.ops4j.pax.exam.spi.reactors.EagerSingleStagedReactorFactory;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;
import org.osgi.framework.ServiceReference;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.*;
import static org.ops4j.pax.exam.CoreOptions.options;
import static org.ops4j.pax.exam.CoreOptions.systemProperty;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
@RunWith(JUnit4TestRunner.class)
@ExamReactorStrategy( EagerSingleStagedReactorFactory.class )
public class T1_Test {

    private File gfHome = new File(System.getProperty("com.sun.aas.installRoot"));
    private File domainDir = new File(gfHome, "domains/domain1");
    @Configuration
    public Option[] configure() {
        return options(systemProperty("com.sun.aas.installRoot").value(gfHome.getAbsolutePath()),
                systemProperty("com.sun.aas.instanceRoot").value(domainDir.getAbsolutePath()),
                systemProperty("GlassFish_Platform").value(System.getProperty("GlassFish_Platform"))
        );
    }

    @Test
    public void foo(BundleContext context) throws InterruptedException, BundleException, IOException, GlassFishException {
        System.out.println("Hello World - I am inside GlassFish");
        final ServiceReference reference = context.getServiceReference(GlassFish.class.getName());
        assertNotNull(reference);
        GlassFish gf = (GlassFish) context.getService(reference);
        assertNotNull(gf);
        assertEquals("GF Started", GlassFish.Status.STARTED, gf.getStatus());
//        GlassFishTracker.waitForService(context, TIMEOUT);
    }
}
