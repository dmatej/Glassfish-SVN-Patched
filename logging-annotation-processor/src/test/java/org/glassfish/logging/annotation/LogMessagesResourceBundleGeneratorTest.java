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

package org.glassfish.logging.annotation;

import java.io.File;
import java.io.FilenameFilter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.ResourceBundle;

import javax.annotation.processing.AbstractProcessor;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import javax.tools.JavaCompiler.CompilationTask;

import junit.framework.TestCase;

import org.glassfish.annotation.processing.logging.LogMessagesResourceBundleGenerator;
import org.glassfish.annotation.processing.logging.LoggerInfoMetadataGenerator;
import org.junit.Test;

import com.foo.bar.Chocolate;
import com.foo.bar.JavaBean;

public class LogMessagesResourceBundleGeneratorTest extends TestCase {

    private static final String  BASE_PATH = "./src/test/java/com/foo/bar";
    
    protected void setUp() {
        System.out.println("Starting test " + this.getName());
        File[] resourceBundles = getResourceBundles();
        for (File f : resourceBundles) {
            System.out.println("Deleting " + f.getAbsolutePath());
            f.delete();
        }
        ResourceBundle.clearCache();
    }
    
    private File[] getResourceBundles() {
        File baseDir = new File("./target/test-classes/com/foo/bar");
        File[] resourceBundles = baseDir.listFiles(new FilenameFilter() {            
            @Override
            public boolean accept(File dir, String name) {
                if (name.endsWith("properties")) {
                    return true;
                } else {
                    return false;                    
                }
            }
        });
        return resourceBundles;
    }
    
    @Test
    public void testNoLoggingAnnotationsCompilation() {        
        File f1 = new File(BASE_PATH, "Vanilla.java");
        String output = executeCompiler(f1);
        // The annotation processor is not invoked in this case.
        assertTrue(!output.contains("LogMessagesResourceBundleGenerator invoked."));
    }

    @Test
    public void testSimpleLoggingAnnotations() {
        File f1 = new File(BASE_PATH, "JavaBean.java");
        String output = executeCompiler(f1);
        assertTrue(output.contains("Annotation processing finished successfully."));
        ResourceBundle rb = ResourceBundle.getBundle(JavaBean.LOGMESSAGES_RB);
        String value = rb.getString(JavaBean.EJB_SYSTEM_INITIALIZED);
        assertEquals("EJB subsystem initialized.", value);
        value = rb.getString(JavaBean.EJB_DEPLOYMENT_FAILED);
        assertEquals("EJB module {0} failed to deploy.", value);
    }

    @Test
    public void testNonStandardResourceBundleName() {
        File f1 = new File(BASE_PATH, "Coffee.java");
        String output = executeCompiler(f1);
        assertTrue(output.contains("annotated by @LogMessagesResourceBundle does not end with 'LogMessages'"));
        File[] resourceBundles = getResourceBundles();
        assertEquals(resourceBundles.length,0);        
    }

    @Test
    public void testNonUniqueResourceBundleName() {
        File f1 = new File(BASE_PATH, "JavaBean.java");
        File f2 = new File(BASE_PATH, "Coffee.java");
        String output = executeCompiler(f1, f2);
        assertTrue(output.contains("More than one resource bundle name specified."));
        File[] resourceBundles = getResourceBundles();
        assertEquals(resourceBundles.length,0);
    }
    
    @Test
    public void testNoResourceBundleName() {
        File f1 = new File(BASE_PATH, "Cocoa.java");
        String output = executeCompiler(f1);
        assertTrue(output.contains("Skipping LogMessages resource bundle generation"));
        File[] resourceBundles = getResourceBundles();
        assertEquals(resourceBundles.length,0);        
    }

    @Test
    public void testCompileMultipleFiles() {
        File f1 = new File(BASE_PATH, "JavaBean.java");
        File f2 = new File(BASE_PATH, "Chocolate.java");
        String output = executeCompiler(f1,f2);
        assertTrue(output.contains("Annotation processing finished successfully."));
        ResourceBundle rb = ResourceBundle.getBundle(JavaBean.LOGMESSAGES_RB);
        String value = rb.getString(JavaBean.EJB_SYSTEM_INITIALIZED);
        assertEquals("EJB subsystem initialized.", value);
        value = rb.getString(JavaBean.EJB_DEPLOYMENT_FAILED);
        assertEquals("EJB module {0} failed to deploy.", value);
        value = rb.getString(Chocolate.EJB_SYSTEM_SHUTDOWN);
        assertEquals("EJB subsystem has been shutdown.", value);
    }
    
    @Test
    public void testIncorrectlyPlacedLoggerInfoAnnotation() {
        File f1 = new File(BASE_PATH, "Tea.java");
        String output = executeCompiler(f1);
        assertTrue(output.contains("Logger name must be a constant string literal value, it cannot be a compile time computed expression."));
        assertTrue(output.contains("Please check if the LoggerInfo annotation is on the logger name constant."));
    }
    
    private static String executeCompiler(File... srcFiles) {
        // Get an instance of java compiler
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

        // Get a new instance of the standard file manager implementation
        StandardJavaFileManager fileManager = compiler.getStandardFileManager(null, null, null);
        List<String> options = new ArrayList<String>();
        options.add("-d");
        File outputDir = new File("target", "test-classes");
        outputDir.mkdirs();
        options.add(outputDir.getAbsolutePath());

        options.add("-s");
        options.add(outputDir.getAbsolutePath());

        Iterable<? extends JavaFileObject> compilationUnits = 
            fileManager.getJavaFileObjects(srcFiles);
        
        for (JavaFileObject fo : compilationUnits) {
            System.out.println("Compiling " + fo);
        }

        StringWriter output = new StringWriter();
        CompilationTask task = compiler.getTask(output, fileManager, null, options, null, compilationUnits);

        // Create a list to hold annotation processors
        LinkedList<AbstractProcessor> processors = new LinkedList<AbstractProcessor>();

        // Add an annotation processor to the list
        processors.add(new LogMessagesResourceBundleGenerator());
        processors.add(new LoggerInfoMetadataGenerator());

        // Set the annotation processor to the compiler task
        task.setProcessors(processors);

        // Perform the compilation task.
        // The compiler will return false for us because the files we are
        // creating won't compile as we don't have the required fields.
        task.call(); 
        
        System.out.println("Compiler output follows: ");
        String compilerOutput = output.toString();
        System.out.println(compilerOutput);
        
        return compilerOutput;
        
    }
    
}
