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

import java.util.Properties;
import org.glassfish.spec.SpecVersion;
import org.glassfish.spec.SpecVersion.MavenVersion;
import org.glassfish.spec.test.sets.Womba;
import org.junit.Assert;
import org.junit.Test;

/**
 *
 * @author Romain Grecourt
 */
public class SpecVersionTest {
    
    @Test
    public void simpleAPITest() {
        final boolean _nonfinal = true;
        final boolean _api = true;
        final boolean _impl = false;
        final String _apipackage = Womba.API_PACKAGE;
        final boolean _standalone = false;
        final String _implpackage = null;
        final String _implversion = null;
        final String _newimplversion = null;
        final String _specversion = Womba.SPEC_VERSION;
        final String _newspecversion = Womba.NEW_SPEC_VERSION;
        final String _specimplversion = Womba.SPEC_IMPL_VERSION;
        final MavenVersion mavenVersion = Womba.MAVEN_VERSION;
        final String groupId = Womba.GROUPID;
        final String artifactId = Womba.ARTIFACTID;
        
        try {
            SpecVersion.checkParams(
                    _nonfinal,
                    _api,
                    _impl,
                    _apipackage,
                    _standalone,
                    _implpackage,
                    _implversion,
                    _newimplversion,
                    _specversion,
                    _newspecversion,
                    mavenVersion.getBuildNumber(),
                    mavenVersion.getBuildNumber(),
                    _specimplversion,
                    mavenVersion,
                    groupId,
                    artifactId);
        } catch (SpecVersion.SpecException ex) {
            Assert.fail(ex.getMessage());
        }

        Properties specProps = SpecVersion.computeProperties(
                _nonfinal,
                _api,
                _impl,
                _apipackage,
                _specversion,
                mavenVersion.getBuildNumber(),
                _implpackage,
                _implversion,
                mavenVersion.getBuildNumber());

        // JAR Extension-Name
        String extensionName = specProps.getProperty("spec.extension.name");
        Assert.assertNotNull(extensionName);
        Assert.assertEquals(Womba.EXTENSION_NAME,extensionName);
        
        // OSGi bundle specversion
        String bundleSpecVersion = specProps.getProperty("spec.bundle.spec.version");
        Assert.assertNotNull(bundleSpecVersion);
        Assert.assertEquals(Womba.BUNDLE_SPEC_VERSION,bundleSpecVersion);
        
        // OSGi Bundle-SymbolicName
        String bundleSymbolicName = specProps.getProperty("spec.bundle.symbolic-name");
        Assert.assertNotNull(bundleSymbolicName);
        Assert.assertEquals(Womba.BUNDLE_SYMBOLIC_NAME,bundleSymbolicName);
        
        // OSGi Bundle-Version
        String bundleVersion = specProps.getProperty("spec.bundle.version");
        Assert.assertNotNull(bundleVersion);
        Assert.assertEquals(Womba.BUNDLE_VERSION,bundleVersion);
        
        // JAR Implementation-Version
        String implementationVersion = specProps.getProperty("spec.implementation.version");
        Assert.assertNotNull(implementationVersion);
        Assert.assertEquals(Womba.IMPLEMENTATION_VERSION,implementationVersion);
    }
}