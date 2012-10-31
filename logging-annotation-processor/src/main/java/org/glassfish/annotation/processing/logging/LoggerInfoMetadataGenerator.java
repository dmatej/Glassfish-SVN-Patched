/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012 Oracle and/or its affiliates. All rights reserved.
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

package org.glassfish.annotation.processing.logging;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Pattern;

import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;

import org.glassfish.logging.annotation.LoggerInfo;

@SupportedAnnotationTypes({"org.glassfish.logging.annotation.LoggerInfo"})
public class LoggerInfoMetadataGenerator extends BaseLoggingProcessor {

    private static final String PUBLISH_SUFFIX = ".publish";
    private static final String SUBSYSTEM_SUFFIX = ".subsystem";
    private static final String DESCRIPTION_SUFFIX = ".description";
    // private static final String RBNAME = "loggerinfo.LoggerInfoMetadata";
    private static final String RBNAME = "META-INF/loggerinfo/LoggerInfoMetadata";
    private static final String VALID_PATTERN = "[a-z[A-Z]][^|]*";

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }

    @Override
    public boolean process (Set<? extends TypeElement> annotations, 
            RoundEnvironment env) {

        debug("LoggerInfoMetadataGenerator invoked.");

        LoggingMetadata loggerMetadata = new LoggingMetadata();
        
        if (!env.processingOver()) {

            SortedMap<String, Element> loggerInfoElements = new TreeMap<String, Element>();

            Set<? extends Element> elements = env.getElementsAnnotatedWith(LoggerInfo.class);
            if (elements.isEmpty()) {
                return false;
            }
            
            Iterator<? extends Element> it = elements.iterator();
            while (it.hasNext()) {
                VariableElement element = (VariableElement)it.next();
                String loggerName = (String)element.getConstantValue();
                if (loggerName == null) {
                    StringBuffer buf = new StringBuffer();
                    buf.append("Logger name must be a constant string literal value, it cannot be a compile time computed expression.");
                    buf.append(System.getProperty("line.separator"));
                    buf.append("Please check if the LoggerInfo annotation is on the logger name constant.");
                    error(buf.toString());
                    return false;
                }
                debug("Processing: " + loggerName + " on element " + element.getSimpleName());
                debug("Enclosing type is " + element.getEnclosingElement().asType());
                
                LoggerInfo loggerInfo = element.getAnnotation(LoggerInfo.class);
                validateLoggerInfo(loggerInfo);
                // Save the log message...
                // Message ids must be unique
                if (loggerInfoElements.containsKey(loggerName)) {
                    // Previous entry with same logger name found.
                    LoggerInfo prevLoggerInfo = loggerInfoElements.get(loggerName).getAnnotation(LoggerInfo.class);
                    if (!compareLoggerInfos(loggerInfo, prevLoggerInfo)) {
                        error("Duplicate use of logger name " + loggerName + " with inconsistent LoggerInfo");
                    }
                } else {
                    renderLoggerInfo(loggerMetadata, loggerName, loggerInfo);
                    loggerInfoElements.put(loggerName, element);
                }
            }
            debug("Loggers found so far: " + loggerMetadata);
            info("Generating logger metadata service.");
            // Get the root logger element
            Element baseLoggerElement = loggerInfoElements.get(loggerInfoElements.firstKey());
            boolean result = generateLoggerInfoMetadataService(baseLoggerElement, loggerMetadata);
            info("Annotation processing finished successfully.");
            return result; // Claim the annotations
        } else {
            return false;
        }
    }
    
    private boolean compareLoggerInfos(LoggerInfo info1, LoggerInfo info2) {
        return (info1.description().equals(info2.description()) &&
                info1.subsystem().equals(info2.subsystem()) &&
                info1.publish() == info2.publish());
    }

    private void validateLoggerInfo(LoggerInfo loggerInfo) {
        if (!Pattern.matches(VALID_PATTERN, loggerInfo.subsystem())) {
            error("Subsystem name is not valid: " + loggerInfo.subsystem());
        }       
        if (!Pattern.matches(VALID_PATTERN, loggerInfo.description())) {
            error("Description for the Logger is not valid: " + loggerInfo.description());            
        }        
    }

    private boolean generateLoggerInfoMetadataService(Element element, LoggingMetadata loggerInfos) {
        String packageName = null;
        do {
            Element enclosing = element.getEnclosingElement();
            debug("Found enclosing element " + element);
            if (enclosing.getKind() == ElementKind.PACKAGE) {
                packageName = enclosing.toString();
            }
            element = enclosing;
        } while(packageName == null);
        
        BufferedWriter bufferedWriter = null;
        try {
            // Now persist the resource bundle
            // String resourceName = packageName + "." + RBNAME;
            String resourceName = RBNAME;
            loadLogMessages(loggerInfos, resourceName);
            debug("Total Messages including ones found from disk so far: " + loggerInfos);
            storeLogMessages(loggerInfos, resourceName);            
        } catch (Exception e) {
            error("Unable to generate LoggerMetadataInfoService class", e);
            return false;
        }  finally {
            if (bufferedWriter != null) {
                try {
                    bufferedWriter.close();
                } catch (IOException e) {
                    error("Unable to close LoggerMetadataInfoService writer", e);
                }
            }
        }
        return true;
    }

    private boolean renderLoggerInfo(LoggingMetadata loggerMetadata, 
            String loggerName, LoggerInfo loggerInfo) {
        loggerMetadata.put(loggerName + DESCRIPTION_SUFFIX, loggerInfo.description());
        loggerMetadata.put(loggerName + SUBSYSTEM_SUFFIX, loggerInfo.subsystem());
        loggerMetadata.put(loggerName + PUBLISH_SUFFIX, loggerInfo.publish());
        return true;
    }
    
}
