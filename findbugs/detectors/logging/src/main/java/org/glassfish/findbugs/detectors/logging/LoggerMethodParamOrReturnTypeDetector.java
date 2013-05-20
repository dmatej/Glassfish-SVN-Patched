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
package org.glassfish.findbugs.detectors.logging;

import java.util.HashSet;
import java.util.Set;

import org.apache.bcel.classfile.Code;

import edu.umd.cs.findbugs.BugInstance;
import edu.umd.cs.findbugs.BugReporter;
import edu.umd.cs.findbugs.BytecodeScanningDetector;
import edu.umd.cs.findbugs.ba.XMethod;

/**
 * @author sandeep.shrivastava
 * 
 */
public class LoggerMethodParamOrReturnTypeDetector extends
        BytecodeScanningDetector {

    private static final Set<String> ALLOWED_CLASSES = new HashSet<String>() {
        
        private static final long serialVersionUID = 205639971410062143L;

        {
            add("org/glassfish/api/logging/LogHelper");
            add("org/glassfish/api/ActionReport");
        }
    };
    
    private BugReporter bugReporter;

    public LoggerMethodParamOrReturnTypeDetector(BugReporter bugReporter) {
        this.bugReporter = bugReporter;
    }

    @Override
    public void visit(Code code) {
        XMethod method = getXMethod();
        if (method.isSynthetic()) {
            return;
        }
        String signature = method.getSignature();        
        if (signature.contains("Ljava/util/logging/Logger;")) {
            bugReporter.reportBug(new BugInstance(
                    "GF_LOGGER_PARAM_OR_RETURN_TYPE", NORMAL_PRIORITY)
                    .addClassAndMethod(this));
        }
        super.visit(code);
    }    
    
    @Override
    public void sawOpcode(int seen) {     
        // Detect the invocation of the Logger.isLoggable() method 
        if (seen == INVOKEVIRTUAL || seen == INVOKESTATIC) 
        {
            String invokedClass = getClassConstantOperand();
            if (!ALLOWED_CLASSES.contains(invokedClass) ) {
                String signature = getSigConstantOperand();
                if (signature.matches("\\(.*Ljava/util/logging/Logger;.*\\).*")) {
                    bugReporter.reportBug(new BugInstance(
                            "GF_LOGGER_PARAM_OR_RETURN_TYPE", NORMAL_PRIORITY)
                            .addClassAndMethod(this).addSourceLine(this));                
                }         
            }
        }
    }

}
