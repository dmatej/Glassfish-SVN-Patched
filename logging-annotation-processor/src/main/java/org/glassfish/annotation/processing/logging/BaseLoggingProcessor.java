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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;

import javax.annotation.processing.AbstractProcessor;
import javax.tools.FileObject;
import javax.tools.StandardLocation;
import javax.tools.Diagnostic.Kind;

public abstract class BaseLoggingProcessor extends AbstractProcessor {

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

    /**
     * This method, given a pkg name will determine the path to the resource,
     * create the LogResourceBundle for that path and load any resources
     * from the existing resource bundle file.
     * 
     * @param rbName the package the resource bundle is relative
     * @return a LogResourceBundle
     */
    protected void loadLogMessages(LoggingMetadata lrb, String rbName) {
    
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

    protected void storeLogMessages(LoggingMetadata lrb, String rbName) {
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
    private FileObject getRBFileObject(String rbName, boolean readObject) 
    throws IllegalArgumentException, IOException 
    {        
        String rbFileName = rbName;
        String rbPkg = "";
        int lastIndex = rbName.lastIndexOf('.');
        if (lastIndex > 0) {
            rbFileName = rbName.substring(lastIndex + 1);
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
}
