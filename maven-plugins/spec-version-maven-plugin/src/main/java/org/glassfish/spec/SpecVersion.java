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
package org.glassfish.spec;

import java.util.Properties;


/**
 *
 * @author Romain Grecourt
 */
public abstract class SpecVersion {
    
    public static class SpecException extends Exception {
        
        public SpecException(String s) {
            super(s);
        }
    }

    public static class MavenVersion {

        private final String SNAPSHOT_QUALIFIER = "SNAPSHOT";
        private String qualifier;
        private String buildNumber = null;
        private String absoluteVersion = null;

        public MavenVersion(String major, String micro, String _qualifier) {
            qualifier = _qualifier;
            if (qualifier != null && qualifier.endsWith(SNAPSHOT_QUALIFIER)) {
                qualifier = qualifier.substring(0, qualifier.length() - SNAPSHOT_QUALIFIER.length() - 1);
            }
            if (qualifier != null && qualifier.startsWith("b")) {
                buildNumber = qualifier.substring(1);
            }
            absoluteVersion = major + "." + micro;
            if (qualifier != null) {
                absoluteVersion += "-" + qualifier;
            }
        }

        public String getBuildNumber() {
            return buildNumber;
        }

        public String getAbsoluteVersion() {
            return absoluteVersion;
        }
    }
    
    public static void fail(String s) throws SpecException{
        throw new SpecException(s);
    }
    
    public static void checkParams (
            boolean _nonfinal,
            boolean _api,
            boolean _impl,
            String _apipackage,
            boolean _standalone,
            String _implpackage,
            String _implversion,
            String _newimplversion,
            String _specversion,
            String _newspecversion,
            String _specbuild,
            String _implbuild,
            String _specimplversion,
            MavenVersion mavenVersion,
            String groupId,
            String artifactId) throws SpecException {

        if (!_nonfinal && mavenVersion.getBuildNumber() != null) {
            fail("maven version can't include a build number for final spec");
        }

        if (_apipackage == null) {
            fail("apipackage is required");
        }
        if (!_apipackage.startsWith("javax.")) {
            fail("API packages must start with \"javax.\"");
        }

        if (_standalone) {
            if (_implpackage == null) {
                fail("implpackage is required for standalone");
            }
            if (_implversion == null) {
                fail("implversion is required for standalone");
            }
        } else {
            if (_implpackage != null) {
                fail("implpackage must not be specified "
                        + "if no standalone implementation");
            }
            if (_implversion != null) {
                fail("implversion must not be specified "
                        + "if no standalone implementation");
            }
            if (_newimplversion != null) {
                fail("newimplversion must not be specified "
                        + "if no standalone implementation");
            }
        }

        if (_implpackage != null && _implpackage.startsWith("javax.")) {
            fail("Implementation packages must NOT start with \"javax.\"");
        }

        if (_specversion == null) {
            fail("specversion is required");
        }

        if (!_specversion.matches("[0-9]+\\.[0-9]+")) {
            fail("JCP specification version number must be "
                    + "of the form <major>.<minor>");
        }
        if (_specimplversion == null) {
            fail("specimplversion is required");
        }

        String sv = _nonfinal ? _newspecversion : _specversion;
        if (!(_specimplversion.equals(sv)
                || _specimplversion.startsWith(sv + ".")
                || _specimplversion.startsWith(sv + "-"))) {
            fail("API jar file version must start with "
                    + "JCP specification version number");
        }

        if (_nonfinal) {
            if (_newspecversion == null) {
                fail("newspecversion is required for nonfinal");
            }
            if (_specbuild == null && mavenVersion.getBuildNumber() == null) {
                fail("specbuild is required for nonfinal");
            }
            if (_standalone) {
                if (_newimplversion == null) {
                    fail("newimplversion is required for nonfinal standalone");
                }
                if (_standalone && _implbuild == null) {
                    fail("implbuild is required for nonfinal standalone");
                }
            }

            // check maven version
            if (_api) {
                if (!mavenVersion.getAbsoluteVersion().contentEquals(_newspecversion + "-b" + _specbuild)) {
                    fail("maven version should be " + _newspecversion + "-b" + _specbuild + "[-SNAPSHOT]");
                }
            } else {
                if (!mavenVersion.getAbsoluteVersion().contentEquals(_newimplversion + "-b" + _implbuild)) {
                    fail("maven version should be " + _newimplversion + "-b" + _implbuild + "[-SNAPSHOT]");
                }
            }
        } else {
            if (_newspecversion != null) {
                fail("newspecversion must not be specified for final specification");
            }
            if (_specbuild != null) {
                fail("specbuild must not be specified for final specification");
            }
            if (_newimplversion != null) {
                fail("newimplversion must not be specified for final specification");
            }
            if (_implbuild != null) {
                fail("implbuild must not be specified for final specification");
            }

            // check maven version
            if (_api) {
                if (!mavenVersion.getAbsoluteVersion().contentEquals(_specversion)) {
                    fail("maven version should be " + _specversion + "[-SNAPSHOT]");
                }

            } else {
                if (!mavenVersion.getAbsoluteVersion().contentEquals(_implversion)) {
                    fail("maven version should be " + _implversion + "[-SNAPSHOT]");
                }
            }
        }

        if (_api) {
            // check groupId
            if (!groupId.contentEquals(_apipackage)) {
                fail("groupId should be " + _apipackage + ", not " + groupId);
            }
            // check artifactId
            if (!artifactId.contentEquals(_apipackage + "-api")) {
                fail("artifactId should be " + _apipackage + "-api" + ", not " + artifactId);
            }
        } else if (_impl || _standalone) {
            // check artifactId
            if (!artifactId.contentEquals(_apipackage)) {
                fail("artifactId should be " + _apipackage + ", not " + artifactId);
            }
        }
    }      
    
    public static Properties computeProperties(
            boolean _nonfinal,
            boolean api,
            boolean impl,
            String _apipackage,
            String _specversion,
            String _specbuild,
            String _implpackage,
            String _implversion,
            String _implbuild) {

        Properties specProps = new Properties();

        specProps.put("spec.extension.name", _apipackage);
        if (_nonfinal) {
            specProps.put("spec.bundle.spec.version", _specversion + ".99.b" + _specbuild);
        } else {
            specProps.put("spec.bundle.spec.version", _specversion);
        }

        if (api) {
            specProps.put("spec.bundle.symbolic-name", _apipackage + "-api");
            if (_nonfinal) {
                specProps.put("spec.bundle.version", _specversion + ".99.b" + _specbuild);
                specProps.put("spec.implementation.version", _specversion + "-b" + _specbuild);
            } else {
                specProps.put("spec.bundle.version", _specversion);
                specProps.put("spec.implementation.version", _specversion);
            }
        } else {
            specProps.put("spec.bundle.symbolic-name", _implpackage + "." + _apipackage);
            if (_nonfinal) {
                specProps.put("spec.specification.version", _implversion + ".99." + _specbuild);
                specProps.put("spec.implementation.version", _implversion + "-b" + _implbuild);
            } else {
                specProps.put("spec.specification.version", _implversion);
                specProps.put("spec.implementation.version", _implversion);
            }
        }
        return specProps;
    }
}