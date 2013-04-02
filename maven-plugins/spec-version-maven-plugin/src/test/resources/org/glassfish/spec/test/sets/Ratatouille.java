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

package org.glassfish.spec.test.sets;

/**
 *
 * @author Romain Grecourt
 */
public class Ratatouille {
    public static final String GROUPID = "${ratatouille.groupId}";
    public static final String ARTIFACTID = "${ratatouille.artifactId}";
    public static final String SPEC_VERSION = "${ratatouille.specVersion}";
    public static final String API_PACKAGE = "${ratatouille.apiPackage}";
    public static final String IMPL_PACKAGE = "${ratatouille.implPackage}";
    public static final String IMPL_VERSION = "${ratatouille.implVersion}";
    public static final String BUNDLE_VERSION = "${ratatouille.bundleVersion}";
    public static final String BUNDLE_SYMBOLIC_NAME = "${ratatouille.bundleSymbolicName}";
    public static final String BUNDLE_SPEC_VERSION = "${ratatouille.bundleSpecVersion}";
    public static final String JAR_EXTENSION_NAME = "${ratatouille.jarExtensionName}";
    public static final String JAR_IMPLEMENTATION_VERSION = "${ratatouille.jarImplementationVersion}";
    public static final String JAR_SPECIFICATION_VERSION = "${ratatouille.jarSpecificationVersion}";
    public static final String MAVEN_VERSION = "${ratatouille.mavenVersion}";
    private static final String MODULES_DIR = "target/it/modules";
    public static final String JAR = MODULES_DIR+"/"+ARTIFACTID+"/target/"+ARTIFACTID+".jar";
}