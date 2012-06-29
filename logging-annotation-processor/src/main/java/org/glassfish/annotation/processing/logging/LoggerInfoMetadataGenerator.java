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

package org.glassfish.annotation.processing.logging;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Pattern;

import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.tools.FileObject;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.glassfish.logging.annotation.LoggerInfo;
import org.glassfish.logging.annotation.LoggerInfoMetadataServiceClassName;

@SupportedAnnotationTypes({"org.glassfish.logging.annotation.LoggerInfo", "org.glassfish.logging.annotation.LoggerInfoMetadataService"})
public class LoggerInfoMetadataGenerator extends BaseLoggingProcessor {

    private static final char DELIMITER = '|';
    private static final String RBNAME = "org.glassfish.api.logging.LoggerInfoMetadata";
    private static final String VALID_PATTERN = "[a-z[A-Z]][^|]*";

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }

    @Override
    public boolean process (Set<? extends TypeElement> annotations, 
            RoundEnvironment env) {

        debug("LoggerInfoMetadataGenerator invoked.");

        LoggingMetadata loggerInfos = new LoggingMetadata();
        
        if (!env.processingOver()) {

            Set<String> loggerNames = new HashSet<String>();

            Set<? extends Element> elements = env.getElementsAnnotatedWith(LoggerInfo.class);
            if (elements.isEmpty()) {
                return false;
            }
            
            Iterator<? extends Element> it = elements.iterator();
            while (it.hasNext()) {
                VariableElement element = (VariableElement)it.next();
                String loggerName = (String)element.getConstantValue();
                if (loggerName == null) {
                    error("Logger name must be a constant string literal value, it cannot be a compile time computed expression.");
                    return false;
                }
                debug("Processing: " + loggerName + " on element " + element.getSimpleName());
                debug("Enclosing type is " + element.getEnclosingElement().asType());
                // Message ids must be unique
                if (!loggerNames.contains(loggerName)) {
                    LoggerInfo loggerInfo = element.getAnnotation(LoggerInfo.class);
                    validateLoggerInfo(loggerInfo);
                    // Save the log message...
                    loggerInfos.put(loggerName, renderLoggerInfo(loggerInfo));
                    loggerNames.add(loggerName);
                } else {
                    error("Duplicate use of logger name " + loggerName);
                    return false;
                }
            }
            debug("Loggers found so far: " + loggerInfos);
            loadLogMessages(loggerInfos, RBNAME);
            debug("Total Messages including ones found from disk so far: " + loggerInfos);
            storeLogMessages(loggerInfos, RBNAME);
            info("Generating logger metadata service.");
            boolean result = generateLoggerInfoMetadataService(env);
            info("Annotation processing finished successfully.");
            return result; // Claim the annotations
        } else {
            return false;
        }
    }

    private void validateLoggerInfo(LoggerInfo loggerInfo) {
        if (!Pattern.matches(VALID_PATTERN, loggerInfo.subsystem())) {
            error("Subsystem name is not valid: " + loggerInfo.subsystem());
        }       
        if (!Pattern.matches(VALID_PATTERN, loggerInfo.description())) {
            error("Description for the Logger is not valid: " + loggerInfo.description());            
        }        
    }

    private boolean generateLoggerInfoMetadataService(RoundEnvironment env) {
        Set<? extends Element> elements = env.getElementsAnnotatedWith(LoggerInfoMetadataServiceClassName.class);
        if (elements.isEmpty()) {
            warn("No LoggerInfoMetadataService annotation defined for the module.");
            return false;
        }
        if (elements.size() > 1) {
            error("Multiple LoggerInfo metadata service class names specified.");
            return false;
        }
        VariableElement element = (VariableElement) elements.iterator().next();
        String className = (String)element.getConstantValue();
        int endIndex = className.lastIndexOf(".");
        String packageName = (endIndex > 0) ? className.substring(0, endIndex) : "";
        String simpleName = className.substring(endIndex+1);
        BufferedWriter bufferedWriter = null; 
        try {
            FileObject srcFileObject = processingEnv.getFiler().createSourceFile(className);
            bufferedWriter = new BufferedWriter(srcFileObject.openWriter());
            VelocityEngine ve = new VelocityEngine();
            Properties props = new Properties();
            props.put("resource.loader", "classpath");
            props.put("classpath.resource.loader.class","org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader");
            ve.init(props);
            VelocityContext vc = new VelocityContext();
            vc.put("simpleName", simpleName);
            vc.put("packageName", packageName);
            Template vt = ve.getTemplate("/org/glassfish/annotation/processing/logging/LoggerInfoMetadataService.template");
            vt.merge(vc, bufferedWriter);
            bufferedWriter.flush();
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

    private Object renderLoggerInfo(LoggerInfo loggerInfo) {
        StringBuffer buffer = new StringBuffer();
        addLoggerInfoField(buffer, loggerInfo.subsystem().trim());
        addLoggerInfoField(buffer, DELIMITER);
        addLoggerInfoField(buffer, loggerInfo.description().trim());
        addLoggerInfoField(buffer, DELIMITER);
        addLoggerInfoField(buffer, loggerInfo.publish());
        return buffer.toString();
    }

    private void addLoggerInfoField(StringBuffer buffer, Object fieldValue) {
        buffer.append(fieldValue);
    }
    
}
