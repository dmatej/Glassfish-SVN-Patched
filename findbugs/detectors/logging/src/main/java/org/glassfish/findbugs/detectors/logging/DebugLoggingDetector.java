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

import java.util.HashMap;
import java.util.Map;

import org.apache.bcel.classfile.Code;

import edu.umd.cs.findbugs.BugInstance;
import edu.umd.cs.findbugs.BugReporter;
import edu.umd.cs.findbugs.BytecodeScanningDetector;

/**
 * @author sandeep.shrivastava
 * 
 */
public class DebugLoggingDetector extends BytecodeScanningDetector {

    private static final Map<String, String> LEVEL_METHOD_NAME_MAP = new HashMap<String, String>() {
        private static final long serialVersionUID = -8663990309350093574L;

        {
            put("FINE", "fine");
            put("FINER", "finer");
            put("FINEST", "finest");
        }
    };
    
    private int seenGetStaticLevelAt;
    private int seenGuardClauseAt;
    private int seenALoadAt;
    private int logBlockStart;
    private int logBlockEnd;
    private String levelName;
    private BugReporter bugReporter;

    public DebugLoggingDetector(BugReporter bugReporter) {
        this.bugReporter = bugReporter;
    }

    @Override
    public void visit(Code code) {
        seenGetStaticLevelAt = Integer.MIN_VALUE;
        seenGuardClauseAt = Integer.MIN_VALUE;
        seenALoadAt = Integer.MIN_VALUE;
        logBlockStart = 0;
        logBlockEnd = 0;
        levelName = "";
        super.visit(code);
    }

    @Override
    public void sawOpcode(int seen) {     
        // Detect access to java.util.logging.Level.FINE, FINER or FINEST objects. 
        if (seen == ALOAD_2) {
            seenALoadAt = getPC();
        }
        if (seen == GETSTATIC 
                && "java/util/logging/Level".equals(getClassConstantOperand())
                && LEVEL_METHOD_NAME_MAP.containsKey(getNameConstantOperand())) 
        {
            seenGetStaticLevelAt = getPC();
            levelName = getNameConstantOperand();
        }
        // Detect the invocation of the Logger.isLoggable() method 
        if (seen == INVOKEVIRTUAL 
                && "java/util/logging/Logger".equals(getClassConstantOperand())
                && "isLoggable".equals(getNameConstantOperand())
                && "(Ljava/util/logging/Level;)Z".equals(getSigConstantOperand())
                && getPC() == seenGetStaticLevelAt+3) 
        {
            seenGuardClauseAt = getPC();
        }
        // Detect the If conditional check, note the start and end of the if block.
        if (seen == IFEQ
                && (getPC() == seenGuardClauseAt + 3)) 
        {
            logBlockStart = getBranchFallThrough();
            logBlockEnd = getBranchTarget();
        }        
        // Detect the invocation of the fine(), finer() or finest() methods on Logger.
        if (seen == INVOKEVIRTUAL
                && LEVEL_METHOD_NAME_MAP
                        .containsValue(getNameConstantOperand())) 
        {
            if (getPC() < logBlockStart || getPC() >= logBlockEnd) 
            {
                if (getPC() == (seenALoadAt+1)) {
                    bugReporter.reportBug(new BugInstance(
                            "GF_UNCONDITIONAL_DEBUG_LOGGING", NORMAL_PRIORITY)
                            .addClassAndMethod(this).addSourceLine(this));                    
                } 
            } else if (!getNameConstantOperand().equals(
                    LEVEL_METHOD_NAME_MAP.get(levelName))) 
            {
                bugReporter.reportBug(new BugInstance(
                        "GF_INCORRECT_CONDITIONAL_DEBUG_LOGGING",
                        NORMAL_PRIORITY).addClassAndMethod(this).addSourceLine(
                        this));
            }
        }
    }

}
