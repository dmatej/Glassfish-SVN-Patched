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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedSourceVersion;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.tools.Diagnostic.Kind;
import javax.tools.FileObject;
import javax.tools.StandardLocation;

import org.glassfish.logging.annotation.LogMessageInfo;
import org.glassfish.logging.annotation.LogMessagesResourceBundle;

@SupportedAnnotationTypes({"org.glassfish.logging.annotation.LogMessageInfo","org.glassfish.logging.annotation.LogMessagesResourceBundle"})
@SupportedSourceVersion(SourceVersion.RELEASE_6)
public class LogMessagesResourceBundleGenerator extends AbstractProcessor {

    private static final String VALIDATE_LEVELS[] = {
      "EMERGENCY",
      "ALERT",
      "SEVERE",
    };

    @Override
    public boolean process (Set<? extends TypeElement> annotations, 
            RoundEnvironment env) {

        debug("LogMessagesResourceBundleGenerator invoked.");

        LogMessagesTreeMap logMessagesMap = new LogMessagesTreeMap();
        
        if (!env.processingOver()) {
            Set<? extends Element> elements;
            String elementPackage = null;

            Set<? extends Element> rbElems = env.getElementsAnnotatedWith(LogMessagesResourceBundle.class);
            Set<String> rbNames = new HashSet<String>();
            for (Element rbElem : rbElems) {
                Object rbValue = ((VariableElement) rbElem).getConstantValue();
                if (rbValue == null) {
                    error("The resource bundle name needs to be a constant value. Specify the LogMessagesResourceBundle annotation only on a compile time constant String literal.");
                    return false;                    
                }
                rbNames.add(rbValue.toString());
            }
            if (rbNames.size() > 1) {
                error("More than one resource bundle name specified. Found the following resource bundle names: " 
                        + rbNames + ". Please specify only one resource bundle name per module.");
                return false;
            }
            
            String rbName = null;
            Iterator<? extends Element> rbElemsIterator = rbElems.iterator();                        
            while (rbElemsIterator.hasNext()) {
                Element rbElem = rbElemsIterator.next();
                if (rbName == null) {
                    rbName = ((VariableElement) rbElem).getConstantValue().toString();
                }
            }
            // XXX: The annotation processor should try to detect the
            //      reuse of an existing log message id.
            //      Degree 1: processed during same build pass.
            //      Degree 2: processed during different builds.

            Set<String> messageIds = new HashSet<String>();

            elements = env.getElementsAnnotatedWith(LogMessageInfo.class);
            Iterator<? extends Element> it = elements.iterator();

            while (it.hasNext()) {
                VariableElement element = (VariableElement)it.next();

                elementPackage = processingEnv.getElementUtils().getPackageOf(element).
                                                getQualifiedName().toString();

                String msgId = (String)element.getConstantValue();
                debug("Annotated pkg: " + elementPackage);
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
            if (rbName == null) {
                error("Resource bundle name not specified for logger");
            }
            loadLogMessages(logMessagesMap, rbName);
            storeLogMessages(logMessagesMap, rbName);
        }

        return true; // Claim the annotations
    }    

    private void checkLogMessageInfo(String msgId, LogMessageInfo lmi) {
      boolean needsCheck = false;
      for (String checkLevel : VALIDATE_LEVELS) {
        if (checkLevel.equals(lmi.level())) {
          needsCheck = true;
        }
      }
      debug("Message " + msgId + " needs checks: " + needsCheck);
      if (needsCheck) {
        if (lmi.cause().trim().length() == 0) {
          error("Missing cause for message id '" + msgId + "' for levels SEVERE and above.");
        }
        if (lmi.action().trim().length() == 0) {
          error("Missing action for message id '" + msgId + "' for levels SEVERE and above.");
        }
      }
    }

    /**
     * This method, given a pkg name will determine the path to the resource,
     * create the LogResourceBundle for that path and load any resources
     * from the existing resource bundle file.
     * 
     * @param rbName the package the resource bundle is relative
     * @return a LogResourceBundle
     */ 
    private void loadLogMessages(LogMessagesTreeMap lrb, String rbName) {

        BufferedReader bufferedReader = null;
        try {
            FileObject rbFileObject = getRBFileObject(rbName, true);
            if (rbFileObject.getLastModified() > 0) {
                bufferedReader = new BufferedReader(rbFileObject.openReader(true));
                lrb.load(bufferedReader);                
            }
        } catch (IllegalArgumentException e) {
            error("Unable to load log message resource bundle: " + 
                    rbName, e);
        } catch (IOException e) {
            debug("Unable to load log message resource bundle: " +
                    rbName, e);
        } finally {
            if (bufferedReader != null) {
                try {
                    bufferedReader.close();
                } catch (IOException e) {
                    error("Unable to close reader for log message resource bundle: " +
                            rbName, e);
                }
            }
        }
    }

    private void storeLogMessages(LogMessagesTreeMap lrb, String rbName) {
        BufferedWriter bufferedWriter = null; 
        try {
            FileObject rbFileObject = getRBFileObject(rbName, false);
            bufferedWriter = new BufferedWriter(rbFileObject.openWriter());
            lrb.store(bufferedWriter);
        } catch (IllegalArgumentException e) {
            error("Unable to store log message resource bundle: " +
                    rbName, e);
        } catch (IOException e) {
            error("Unable to store log message resource bundle: " +
                    rbName, e);
        }  finally {
            if (bufferedWriter != null) {
                try {
                    bufferedWriter.close();
                } catch (IOException e) {
                    error("Unable to store log message resource bundle: " +
                            rbName, e);
                }
            }
        } 
    }

    /**
     * We cache paths to the resource bundle because the compiler does not
     * allow us to call createResource() more than once for an object.
     * Note that in Java 7 getResource() throws FileNotFound if the
     * target resource does not exist.   The behavior is different in Java 6.
     * 
     * @param rbName
     * @return path to resource bundle relative to pkg 
     */
    private FileObject getRBFileObject(String rbName, boolean readObject) throws IllegalArgumentException, 
            IOException {        
        String rbFileName = rbName;
        String rbPkg = "";
        int lastIndex = rbName.lastIndexOf('.');
        if (lastIndex > 0) {
          rbFileName = rbName.substring(lastIndex+1);
          rbPkg = rbName.substring(0, lastIndex);
        } 
        rbFileName = rbFileName + ".properties";
        if (readObject) {
            return processingEnv.getFiler().getResource(
                    StandardLocation.CLASS_OUTPUT, rbPkg, rbFileName);
        } else {
            return processingEnv.getFiler().createResource(
                    StandardLocation.CLASS_OUTPUT, rbPkg, rbFileName, 
                    (javax.lang.model.element.Element[]) null);            
        }
    }

    protected void debug(String msg) {
        processingEnv.getMessager().printMessage(Kind.OTHER, msg);
    }

    protected void debug(String msg, Throwable t) {
        processingEnv.getMessager().printMessage(Kind.OTHER, t.getMessage() + ":" + msg);
    }

    protected void info(String msg) {
        processingEnv.getMessager().printMessage(Kind.NOTE, 
            getClass().getName() + ": " + msg);
    }

    protected void warn(String msg) {
        processingEnv.getMessager().printMessage(Kind.WARNING, 
                getClass().getName() + ": " + msg);
    }
    protected void warn(String msg, Throwable t) {
        String errMsg = msg + ": " + t.getMessage();
        processingEnv.getMessager().printMessage(Kind.WARNING, 
                getClass().getName() + ": " + errMsg);
    }

    protected void error(String msg) {
        processingEnv.getMessager().printMessage(Kind.ERROR, 
                getClass().getName() + ": " + msg);
    }
    
    protected void error(String msg, Throwable t) {
        String errMsg = msg + ": " + t.getMessage();
        processingEnv.getMessager().printMessage(Kind.ERROR, 
                getClass().getName() + ": " + errMsg);
    }
    
}
