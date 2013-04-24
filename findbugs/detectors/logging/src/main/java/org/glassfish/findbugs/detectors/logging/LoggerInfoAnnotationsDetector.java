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

import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.Code;
import org.apache.bcel.classfile.ConstantString;
import org.apache.bcel.classfile.Field;

import edu.umd.cs.findbugs.BugInstance;
import edu.umd.cs.findbugs.BugReporter;
import edu.umd.cs.findbugs.BytecodeScanningDetector;

/**
 * @author sanshriv
 * 
 */
public class LoggerInfoAnnotationsDetector extends BytecodeScanningDetector {

    private static final boolean DEBUG = false;

    private BugReporter bugReporter;

    private Map<String, String> loggerInfoAnnotations = new HashMap<String, String>();

    private Map<String, String> logMessagesResourceBundleAnnotations = new HashMap<String, String>();

    private Map<Integer, String> constantsVisited = new HashMap<Integer, String>();

    public LoggerInfoAnnotationsDetector(BugReporter bugReporter) {
        this.bugReporter = bugReporter;
    }

    public void visit(Code code) {
        super.visit(code);
        constantsVisited.clear();
    }

    @Override
    public void visit(Field field) {
        super.visit(field);
        String name = field.getName();
        AnnotationEntry[] annotationEntries = field.getAnnotationEntries();
        for (AnnotationEntry annoEntry : annotationEntries) {
            String annoType = annoEntry.getAnnotationType();
            if (annoType
                    .equals("Lorg/glassfish/logging/annotation/LoggerInfo;")) {
                String value = field.getConstantValue().toString();
                value = value.substring(1);
                value = value.substring(0, value.length() - 1);
                loggerInfoAnnotations.put(name, value);                
            } else if (annoType
                    .equals("Lorg/glassfish/logging/annotation/LogMessagesResourceBundle;")) {
                String value = field.getConstantValue().toString();
                value = value.substring(1);
                value = value.substring(0, value.length() - 1);
                logMessagesResourceBundleAnnotations.put(name, value);
            }
        }
    }

    @Override
    public void sawOpcode(int opCode) {

        if (opCode == LDC && getConstantRefOperand() instanceof ConstantString) {
            constantsVisited.put(getPC(), getStringConstantOperand());
        }

        // Detects the invocation of the Logger.getLogger() method where the
        // logger name is not annotated with LoggerInfo annotation
        if (opCode == INVOKESTATIC || opCode == INVOKEVIRTUAL) {
            if ("java/util/logging/Logger".equals(getClassConstantOperand())
                    && "getLogger".equals(getNameConstantOperand())) 
            {
                
                if (DEBUG) {
                    System.out.println("Signature="+getSigConstantOperand());
                    System.out.println("class=" + this.getClassName()
                            + ",loggerInfoAnnotations=" + loggerInfoAnnotations);
                }
                                
                if ("(Ljava/lang/String;Ljava/lang/String;)Ljava/util/logging/Logger;".equals(getSigConstantOperand())) 
                {
                    int pc = getPC();
                    String param1 = constantsVisited.get(pc - 4);
                    String param2 = constantsVisited.get(pc - 2);
                    if (DEBUG) {
                        System.out.println("param1=" + param1 + ",param2=" + param2);
                    }
                    if ((param1 != null && !loggerInfoAnnotations.containsValue(param1)) 
                            || (param1 == null)) 
                    {
                        bugReporter.reportBug(new BugInstance(
                                "GF_MISSING_LOGGER_INFO_ANNOTATION", NORMAL_PRIORITY)
                                .addClassAndMethod(this).addSourceLine(this));
                    }

                    if ((param2 != null && !logMessagesResourceBundleAnnotations.containsValue(param2)) 
                            || (param2 == null)) 
                    {
                        bugReporter.reportBug(new BugInstance(
                                "GF_MISSING_LOGMESSAGES_RB_ANNOTATION", NORMAL_PRIORITY)
                                .addClassAndMethod(this).addSourceLine(this));
                    }  
                    
                } else if ("(Ljava/lang/String;)Ljava/util/logging/Logger;".equals(getSigConstantOperand())) {
                    int pc = getPC();
                    String param1 = constantsVisited.get(pc - 2);
                    if (DEBUG) {
                        System.out.println("param1=" + param1);
                    }
                    if ((param1 != null && !loggerInfoAnnotations.containsValue(param1)) 
                            || (param1 == null)) 
                    {
                        bugReporter.reportBug(new BugInstance(
                                "GF_MISSING_LOGGER_INFO_ANNOTATION", NORMAL_PRIORITY)
                                .addClassAndMethod(this).addSourceLine(this));
                    }                    
                }
            }
        }
    }

}
