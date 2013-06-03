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

import edu.umd.cs.findbugs.BugInstance;
import edu.umd.cs.findbugs.BugReporter;
import edu.umd.cs.findbugs.BytecodeScanningDetector;

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

    private Map<String, String> annotatedLogMessages = new HashMap<String, String>();
    
    private Map<String, BugInstance> visitedLogMessages = new HashMap<String, BugInstance>();
    
    private SortedMap<Integer, String> levelsVisited = new TreeMap<Integer, String>();

    private SortedMap<Integer, String> constantsVisited = new TreeMap<Integer, String>();
    
    private BugReporter bugReporter;
    
    public LogMessageInfoAnnotationsDetector(BugReporter bugReporter) {
        this.bugReporter = bugReporter;
    }

    @Override
    public void visit(Field field) {
        super.visit(field);
        String fieldName = getClassName() + "." + field.getName();
        AnnotationEntry[] annotationEntries = field.getAnnotationEntries();
        for (AnnotationEntry annoEntry : annotationEntries) {
            String annoType = annoEntry.getAnnotationType();
            if (annoType
                    .equals("Lorg/glassfish/logging/annotation/LogMessageInfo;")) 
            {
                String msgId = field.getConstantValue().toString();
                msgId = msgId.substring(1);
                msgId = msgId.substring(0, msgId.length() - 1);
                annotatedLogMessages.put(msgId, fieldName);                
            }
        }
    }

    @Override
    public void visit(Code code) {
        levelsVisited.clear();
        constantsVisited.clear();
        super.visit(code);
    }
    
    @Override
    public void sawOpcode(int opCode) {
                
        if (opCode == GETSTATIC 
                && "java/util/logging/Level".equals(getClassConstantOperand())
                ) 
        {
            String levelName  = getNameConstantOperand();
            levelsVisited.put(getPC(), levelName);
        }
        
        if (opCode == LDC && getConstantRefOperand() instanceof ConstantString) {
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
            
            if(!checkMessagePattern(levelName, message)) {
                bugReporter.reportBug(new BugInstance(
                        "GF_INVALID_MSG_ID_PATTERN", HIGH_PRIORITY)
                        .addClassAndMethod(this).addSourceLine(this));
            } 
            
            if (levelName != null && !EXCLUDED_LEVELS.contains(levelName)) 
            {
            	if (message == null) {
            		bugReporter.reportBug(new BugInstance(
                            "GF_MISSING_LOGMESSAGE_INFO_ANNOTATION", HIGH_PRIORITY)
                            .addClassAndMethod(this).addSourceLine(this));
            	} else if (!message.isEmpty()) {
                    visitedLogMessages.put(message, new BugInstance(
                            "GF_MISSING_LOGMESSAGE_INFO_ANNOTATION", HIGH_PRIORITY)
                            .addClassAndMethod(this).addSourceLine(this));            		
            	}
            }
            
        }
    }

    private boolean checkMessagePattern(String levelName, String message) 
    {
        if (levelName != null && message != null && !message.isEmpty()) {
            if (EXCLUDED_LEVELS.contains(levelName)) {
                return true;
            }
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
            if (!annotatedLogMessages.keySet().contains(logMsg)) {
                bugReporter.reportBug(visitedLogMessages.get(logMsg));
            }
        }
        
    }
    
    
}
    
