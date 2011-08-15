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


package org.glassfish.fighterfish.test.it;

import org.glassfish.fighterfish.test.util.TestsConfiguration;
import org.junit.runner.RunWith;
import org.ops4j.pax.exam.Option;
import org.ops4j.pax.exam.junit.Configuration;
import org.ops4j.pax.exam.junit.ExamReactorStrategy;
import org.ops4j.pax.exam.junit.JUnit4TestRunner;
import org.ops4j.pax.exam.spi.reactors.EagerSingleStagedReactorFactory;

import java.io.IOException;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 *
 * PAX-EXAM requires each test class to be annotated with {@link RunWith} annotation with a value of
 * {@link JUnit4TestRunner}. We have also set {@link ExamReactorStrategy} as {@link EagerSingleStagedReactorFactory}
 * which means for every test method invocation, a new test container instance won't be created. Pax-Exam will create a
 * new test container instance for each TestClass and reuse it for every test method found in that class. Each test
 * can optionally configure the test container by having one or more configuration methods. Each such method must be
 * annotated with {@link Configuration} and return an array of {@link Option}. The options returned by such a method
 * is used to configure the OSGi framework that's going to be launched by PAX-EXAM. If a test has more than one such
 * methods, then pax-exam will create multiple test container and run the test in each such container.
 *
 * Most of our tests require the OSGi platform to be configured similarly, so we provide this base class as
 * a convenience for our tests. In a lot of way, this shields individual tests from pax-exam details.
 * In addition to providing pax-exam contracts, it also provides some helper methods
 * which are needed in every test.
 * It is not mandatory for tests to extend this class.
 *
 * This class does not currently belong to test.util bundle as we don't want to depend on
 * pax-exam-junit packages from test.util bundle. There are some issues when we do so. This is mainly because
 * pax-exam-junit4 is not an OSGi bundle, so we can't provision it. We have to either wrap it or configure system
 * packages appropriately. Either of them suck. So, we just leave this base class in the test bundle itself.
 *
 */
@RunWith(JUnit4TestRunner.class)
@ExamReactorStrategy( EagerSingleStagedReactorFactory.class )
public abstract class AbstractTestObject {

    /**
     * PaxExamJunit driver treats methods in Junit Test class annotated with @Configuration specially.
     * For each such method, it creates a separate test container configuring it with the options as returned
     * by the method.
     *
     * @return Options used to configure a test container
     * @throws IOException
     */
    @Configuration
    public Option[] getPaxExamConfiguration() throws IOException {
        return TestsConfiguration.getInstance().getPaxExamConfiguration();
    }

    // helper method
    protected Long getTimeout() {
        return TestsConfiguration.getInstance().getTimeout();
    }

}
