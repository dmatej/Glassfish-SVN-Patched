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
import org.glassfish.spec.Spec;
import org.glassfish.spec.test.sets.Aubergine;
import org.glassfish.spec.test.sets.Courgette;
import org.glassfish.spec.test.sets.Ratatouille;
import org.glassfish.spec.test.sets.Moussaka;
import org.junit.Assert;
import org.junit.Test;

/**
 * 
 * @author Romain Grecourt
 */
public class SpecTest {
    public static void positive(
            Artifact artifact,
            String version,
            String newVersion,
            String implVersion) {

        Spec spec = new Spec(artifact, version, newVersion, implVersion);
        spec.verify();
        if(!spec.getErrors().isEmpty()){
            String msg = artifact + " - specVersion (" + version + ")"
                + " - newVersion (" + newVersion + ")"
                + " - implVersion (" + implVersion + ")"
                + " should be compliant";
            for(String error : spec.getErrors()){
                msg = "\n" + error;
            }
            Assert.fail(msg);
        }
    }
    
    @Test
    public void nonFinalAPI() {
        positive(
                new Artifact(
                Aubergine.GROUPID,
                Aubergine.ARTIFACTID,
                Aubergine.MAVEN_VERSION),
                Aubergine.SPEC_VERSION,
                Aubergine.NEW_SPEC_VERSION,
                Aubergine.IMPL_VERSION);
    }

    @Test
    public void finalAPI() {
        positive(
                new Artifact(
                Courgette.GROUPID,
                Courgette.ARTIFACTID,
                Courgette.MAVEN_VERSION),
                Courgette.SPEC_VERSION,
                null,
                Courgette.IMPL_VERSION);
    }

    @Test
    public void nonFinalStandlone() {
        positive(
                new Artifact(
                Moussaka.GROUPID,
                Moussaka.ARTIFACTID,
                Moussaka.MAVEN_VERSION),
                Moussaka.SPEC_VERSION,
                Moussaka.NEW_IMPL_VERSION,
                Moussaka.IMPL_VERSION);
    }
    
    @Test
    public void finalStandalone() {
        positive(
                new Artifact(
                Ratatouille.GROUPID,
                Ratatouille.ARTIFACTID,
                Ratatouille.MAVEN_VERSION),
                Ratatouille.SPEC_VERSION,
                null,
                Ratatouille.IMPL_VERSION);
    }
}
