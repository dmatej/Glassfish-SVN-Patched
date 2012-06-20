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

import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;

import org.glassfish.logging.annotation.LogMessageInfo;
import org.glassfish.logging.annotation.LogMessagesResourceBundle;

@SupportedAnnotationTypes({"org.glassfish.logging.annotation.LogMessageInfo","org.glassfish.logging.annotation.LogMessagesResourceBundle"})
public class LogMessagesResourceBundleGenerator extends BaseLoggingProcessor {

    private static final String VALIDATE_LEVELS[] = {
      "EMERGENCY",
      "ALERT",
      "SEVERE",
    };
    
    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }

    @Override
    public boolean process (Set<? extends TypeElement> annotations, 
            RoundEnvironment env) {

        debug("LogMessagesResourceBundleGenerator invoked.");

        LoggingMetadata logMessagesMap = new LoggingMetadata();
        
        if (!env.processingOver()) {
            Set<? extends Element> elements;

            Set<? extends Element> rbElems = env.getElementsAnnotatedWith(LogMessagesResourceBundle.class);
            Set<String> rbNames = new HashSet<String>();
            for (Element rbElem : rbElems) {
                Object rbValue = ((VariableElement) rbElem).getConstantValue();
                if (rbValue == null) {
                    error("The resource bundle name value could not be computed. Specify the LogMessagesResourceBundle annotation only on a compile time constant String literal.");
                    return false;                    
                }
                rbNames.add(rbValue.toString());
            }
            if (rbNames.isEmpty()) {
                error("No resource bundle name found. Atleast one String literal constant needs to be decorated with the LogMessagesResourceBundle annotation.");
                return false;                
            }
            if (rbNames.size() > 1) {
                error("More than one resource bundle name specified. Found the following resource bundle names: " 
                        + rbNames + ". Please specify only one resource bundle name per module.");
                return false;
            }
            
            String rbName = rbNames.iterator().next();
            if (!rbName.endsWith("LogMessages")) {
                error("The fully qualified resource bundle name needs to be end with LogMessages as the best practice.");
                return false;
            }

            Set<String> messageIds = new HashSet<String>();

            elements = env.getElementsAnnotatedWith(LogMessageInfo.class);
            Iterator<? extends Element> it = elements.iterator();

            while (it.hasNext()) {
                VariableElement element = (VariableElement)it.next();
                String msgId = (String)element.getConstantValue();
                debug("Processing: " + msgId);
                // Message ids must be unique
                if (!messageIds.contains(msgId)) {
                    LogMessageInfo lmi = element.getAnnotation(LogMessageInfo.class);
                    checkLogMessageInfo(msgId, lmi);

                    // Save the log message...
                    logMessagesMap.put(msgId, lmi.message());
                    // Save the message's comment if it has one...
                    if (!lmi.comment().isEmpty()) {
                        logMessagesMap.putComment(msgId, lmi.comment());
                    }
                    messageIds.add(msgId);
                } else {
                    error("Duplicate use of message-id " + msgId);
                }
            }
            debug("Messages found so far: " + logMessagesMap);
            loadLogMessages(logMessagesMap, rbName);
            debug("Total Messages including ones found from disk so far: " + logMessagesMap);
            storeLogMessages(logMessagesMap, rbName);
            info("Annotation processing finished successfully.");
            return true; // Claim the annotations
        } else {
            return false;
        }
    }    

    private void checkLogMessageInfo(String msgId, LogMessageInfo lmi) {
      boolean needsCheck = false;
      for (String checkLevel : VALIDATE_LEVELS) {
        if (checkLevel.equals(lmi.level())) {
          needsCheck = true;
        }
      }
      debug("Message " + msgId + " needs checking for cause/action: " + needsCheck);
      if (needsCheck) {
        if (lmi.cause().trim().length() == 0) {
          error("Missing cause for message id '" + msgId + "' for levels SEVERE and above.");
        }
        if (lmi.action().trim().length() == 0) {
          error("Missing action for message id '" + msgId + "' for levels SEVERE and above.");
        }
      }
    }
    
}
