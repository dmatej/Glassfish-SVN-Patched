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
        assertTrue(output.contains("The fully qualified resource bundle name needs to be end with LogMessages as the best practice."));
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
        assertTrue(output.contains("Atleast one String literal constant needs to be decorated with the LogMessagesResourceBundle annotation."));
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
