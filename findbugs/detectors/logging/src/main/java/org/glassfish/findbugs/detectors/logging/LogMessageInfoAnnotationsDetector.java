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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.logging.Level;

import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.Code;
import org.apache.bcel.classfile.ConstantString;
import org.apache.bcel.classfile.Field;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;

import edu.umd.cs.findbugs.BugInstance;
import edu.umd.cs.findbugs.BugReporter;
import edu.umd.cs.findbugs.BytecodeScanningDetector;
import edu.umd.cs.findbugs.FieldAnnotation;

/**
 * @author sandeep.shrivastava
 *
 */
public class LogMessageInfoAnnotationsDetector extends BytecodeScanningDetector {

    private static final Set<String> EXCLUDED_LEVELS = new HashSet<String>() {

        private static final long serialVersionUID = 6845716234986486398L;
        {
            add("FINE");
            add("FINER");
            add("FINEST");
        }
        
    };

    private static final boolean DEBUG = false;

    private Set<String> annotatedLogMessages = new HashSet<String>();
    
    private Set<BugInstance> invalidMgsIds = new HashSet<BugInstance>();
    
    private Map<String, BugInstance> visitedLogMessages = new HashMap<String, BugInstance>();
    
    private SortedMap<Integer, String> levelsVisited = new TreeMap<Integer, String>();

    private SortedMap<Integer, String> constantsVisited = new TreeMap<Integer, String>();
    
    private BugReporter bugReporter;
    
    private boolean ignoreClass = false;
    
    public LogMessageInfoAnnotationsDetector(BugReporter bugReporter) {
        this.bugReporter = bugReporter;
    }
    
    @Override
    public void visit(JavaClass javaClass) {
        super.visit(javaClass);
        String superClassName = javaClass.getSuperclassName();
        if (superClassName.equals("com.sun.enterprise.admin.cli.CLICommand")) {
            ignoreClass = true;
        }
        if(javaClass.getPackageName().startsWith("com.sun.enterprise.admin.cli")) {
            ignoreClass = true;
        }
        if (DEBUG) {
            System.out.println("Analyzing class="+javaClass.getClassName() 
                    + ", superClassName="+superClassName
                    + ", ignoreClass="+ignoreClass);
        }
    }

    @Override
    public void visit(Field field) {
        super.visit(field);
        if (ignoreClass) {
            return;
        }
        AnnotationEntry[] annotationEntries = field.getAnnotationEntries();
        for (AnnotationEntry annoEntry : annotationEntries) {
            String annoType = annoEntry.getAnnotationType();
            if (annoType
                    .equals("Lorg/glassfish/logging/annotation/LogMessageInfo;")) 
            {
                String msgId = field.getConstantValue().toString();
                msgId = msgId.substring(1);
                msgId = msgId.substring(0, msgId.length() - 1);
                annotatedLogMessages.add(msgId);
                // Validate the message ID
                if(!checkMessagePattern(msgId)) {
                    FieldAnnotation fieldAnno = FieldAnnotation.fromVisitedField(this);
                    bugReporter.reportBug(new BugInstance(
                            "GF_INVALID_MSG_ID_PATTERN", NORMAL_PRIORITY).addClass(
                                    getDottedClassName()).addField(fieldAnno));
                }
            }
        }
    }

    @Override
    public void visit(Method method) {
        if (DEBUG) {
            System.out.println("Analyzing method="+method.getName());   
        }
        super.visit(method);
    }

    @Override
    public void visit(Code code) {
        levelsVisited.clear();
        constantsVisited.clear();
        super.visit(code);
    }
    
    @Override
    public void sawOpcode(int opCode) {
        if (ignoreClass) {
            return;
        }                
        if (opCode == GETSTATIC 
                && "java/util/logging/Level".equals(getClassConstantOperand())
                ) 
        {
            String levelName  = getNameConstantOperand();
            levelsVisited.put(getPC(), levelName);
        }
        
        if ((opCode == LDC || opCode == LDC_W) && (getConstantRefOperand() instanceof ConstantString)) 
        {
            constantsVisited.put(getPC(), getStringConstantOperand());
        }
        // Detects the invocation of the Logger.logXXX() methods where the
        // message ID is not confirming to GlassFish Logging conventions
        if (opCode == INVOKEVIRTUAL && "java/util/logging/Logger".equals(getClassConstantOperand())) 
        {
            String methodName = getNameConstantOperand();
            String signature = getSigConstantOperand();
            String levelName = null;
            String message = null;
            
            if (methodName.equals("log")) 
            {
                if (DEBUG) {
                  System.out.println("levelsVisited="+levelsVisited);    
                }
            	  int lastLevelPC = -1;            	
                if (!levelsVisited.isEmpty()) {
                	lastLevelPC = levelsVisited.lastKey();
                	levelName = levelsVisited.get(lastLevelPC);
                }
                if (!constantsVisited.isEmpty()) {
                	for (int constantPC : constantsVisited.keySet()) {
                		if (lastLevelPC > -1 && constantPC > lastLevelPC) {
                        	message = constantsVisited.get(constantPC);
                        	break;
                		}
                	}
                }
                if (DEBUG) {
                    System.out.println("levelName="+levelName + ",message="+message);    
                }
            }
            
            if (methodName.equals("config") 
                    && signature.equals("(Ljava/lang/String;)V"))
            {
               levelName = Level.CONFIG.getName();
               message = constantsVisited.get(getPC() - 2);    
            }

            if (methodName.equals("info") 
                    && signature.equals("(Ljava/lang/String;)V")) 
            {
               levelName = Level.INFO.getName();
               message = constantsVisited.get(getPC() - 2);    
            }

            if (methodName.equals("warning") 
                    && signature.equals("(Ljava/lang/String;)V"))
            {
               levelName = Level.WARNING.getName();
               message = constantsVisited.get(getPC() - 2);    
            }

            if (methodName.equals("severe") 
                    && signature.equals("(Ljava/lang/String;)V"))
            {
               levelName = Level.SEVERE.getName();
               message = constantsVisited.get(getPC() - 2);    
            }
                        
            if (levelName != null && !EXCLUDED_LEVELS.contains(levelName)) 
            {
            	if (message == null) {            	    
            		bugReporter.reportBug(new BugInstance(
                            "GF_MISSING_LOGMESSAGE_INFO_ANNOTATION", NORMAL_PRIORITY)
                            .addClassAndMethod(this).addSourceLine(this));
                if (DEBUG) {
                    System.out.println("Reported bug="+new BugInstance(
                            "GF_MISSING_LOGMESSAGE_INFO_ANNOTATION", NORMAL_PRIORITY).addClassAndMethod(this).addSourceLine(this));    
                }
            	} else if (!message.isEmpty()) {
                    visitedLogMessages.put(message, new BugInstance(
                            "GF_MISSING_LOGMESSAGE_INFO_ANNOTATION", NORMAL_PRIORITY)
                            .addClassAndMethod(this).addSourceLine(this)); 
            	}
            }
            
        }
    }

    private boolean checkMessagePattern(String message) 
    {
        if (!message.isEmpty()) {
            // Message IDs are expected to be in the form AA-BBB-12345
            String[] tokens = message.split("-");            
            if (tokens.length < 3) {
                return false;
            }             
            
            String messageNumber = tokens[tokens.length-1];
            if (messageNumber.length() != 5) {
                return false;
            }            
        }
        return true;
    }
    
    public void report() {
        for (String logMsg : visitedLogMessages.keySet()) {
            if (!annotatedLogMessages.contains(logMsg)) {
                bugReporter.reportBug(visitedLogMessages.get(logMsg));
            } 
        }
        
        for (BugInstance bug : invalidMgsIds) {
            bugReporter.reportBug(bug);
        }
    }    
    
}
    
