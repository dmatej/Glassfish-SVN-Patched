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


package org.glassfish.fighterfish.test.util;

/**
 * Constants used in various places.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public final class Constants {
    /**
     * Name of the properties file containing OSGi framework configuration
     */
    static final String FW_CONFIG_FILE_NAME = "OSGiFramework.properties";

    /**
     * Property name used to configure pax-exam timeout behavior. The value of this property indicates how long will
     * pax-exam wait before timing out in operations that can possibly never return. When this happens,
     * pax-exam fails the test as opposed to hanging for ever. One example is when a provisioned bundle never returns
     * from its activator.
     */
    static final String EXAM_TIMEOUT_PROP = "pax-exam.timeout";

    /**
     * Name of property used to indicate which platform is being used to run GlassFish. Values are Felix or Equinox
     */
    static final String GLASSFISH_PLATFORM_PROP = "GlassFish_Platform";

    /**
     * Default value for {@link #GLASSFISH_PLATFORM_PROP}
     */
    static final String DEFAULT_GLASSFISH_PLATFORM = "Felix";

    /**
     * Property name to specify installation root of GlassFish.
     */
    static final String GLASSFISH_INSTALL_ROOT_PROP = "com.sun.aas.installRoot";

    /**
     * Property name used to configure timeout values of tests. This depends on tests, but we have a global timeout
     * value for all tests. So, the value must be maximum of each test's timeout value.
     */
    static final String FIGHTERFISH_TEST_TIMEOUT_PROP = "fighterfish.test.timeout";

    /**
     * Default timeout value in ms. If no timeout is set using {@link #FIGHTERFISH_TEST_TIMEOUT_PROP}, this value is used.
     */
    static final String FIGHTERFISH_TEST_TIMEOUT_DEFAULT_VALUE = "30000"; // in ms

}
