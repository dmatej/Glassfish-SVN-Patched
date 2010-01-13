/*
* DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
*
* Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
*
* The contents of this file are subject to the terms of either the GNU
* General Public License Version 2 only ("GPL") or the Common Development
* and Distribution License("CDDL") (collectively, the "License").  You
* may not use this file except in compliance with the License. You can obtain
* a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
* or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
* language governing permissions and limitations under the License.
*
* When distributing the software, include this License Header Notice in each
* file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
* Sun designates this particular file as subject to the "Classpath" exception
* as provided by Sun in the GPL Version 2 section of the License file that
* accompanied this code.  If applicable, add the following below the License
* Header, with the fields enclosed by brackets [] replaced by your own
* identifying information: "Portions Copyrighted [year]
* [name of copyright owner]"
*
* Contributor(s):
*
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

package org.glassfish.bean_validator;

import com.gargoylesoftware.htmlunit.html.HtmlPage;
import junit.framework.Test;
import junit.framework.TestSuite;

public class HeartbeatTestCase extends BVTestCaseBase {
    
    public HeartbeatTestCase() {
        this("HeartbeatTestCase");
    }

    public HeartbeatTestCase(String name) {
        super(name);
    }

    /**
     * Set up instance variables required by this test case.
     */
    public void setUp() throws Exception {
        super.setUp();
    }


    /**
     * Return the tests included in this test suite.
     */
    public static Test suite() {
        return (new TestSuite(HeartbeatTestCase.class));
    }


    /**
     * Tear down instance variables required by this test case.
     */
    public void tearDown() {
        super.tearDown();
    }

    public void testHeartbeat() throws Exception {
        HtmlPage page = getPage("/HeartbeatTestServlet/index.html");
        String pageText = page.asText();
        assertTrue(pageText.contains("Obtained ValidatorFactory:"));
        assertTrue(pageText.contains("case1: No ConstraintViolations found."));
        assertTrue(pageText.contains("case2: caught IllegalArgumentException. Message: Invalid property path. There is no property nonExistentProperty in entity org.glassfish.bean_validator.Person"));
        assertTrue(pageText.contains("case3: ConstraintViolation: message: may not be null propertyPath: lastName"));
        assertTrue(pageText.contains("case3: ConstraintViolation: message: may not be null propertyPath: firstName"));
        assertTrue(pageText.contains("case3: ConstraintViolation: message: may not be null propertyPath: listOfString"));
        assertTrue(pageText.contains("case4: No ConstraintViolations found."));


    }



}
