package relaxngcc.ant;

import java.io.File;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.LogOutputStream;
import org.apache.tools.ant.types.FileSet;
import relaxngcc.Options;
import relaxngcc.RelaxNGCC;


/**
 * Ant task that invokes RelaxNGCC.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class RelaxNGCCTask extends Task {

    /** Source RELAX NG grammar. */
    private File source;
    /** Target directory. */
    private File target;
    /** Directory to output automata gif files. */
    private File automataDir;

    /**
     * Files that are used as the input to RelaxNGCC.
     * Used for up-to-date check. List of FileSets.
     */
    private final ArrayList dependsSet = new ArrayList();
    /**
     * Files that are produced by RelaxNGCC.
     */
    private final ArrayList producesSet = new ArrayList();


    public void execute() throws BuildException {
        if( source==null )
            throw new BuildException("The source attribute is required",location);
        if( target==null )
            throw new BuildException("The targetdir attribute is required",location);

        long srcTime = findTimestamp(dependsSet,true);
        long dstTime = findTimestamp(producesSet,false);
        log("the source time stamp is      "+srcTime, Project.MSG_VERBOSE);
        log("the destination time stamp is "+dstTime, Project.MSG_VERBOSE);
        
        if( srcTime < dstTime ) {
            log("skipped RelaxNGCC because files are up-to-date");
            return;
        }

//        ErrorHandlerImpl eh =
//            new ErrorHandlerImpl(
                new PrintStream(new LogOutputStream(this, Project.MSG_WARN)); // );

        boolean hadError = false;
        
        Options opt = new Options();
        opt.sourcefile = source;
        opt.targetdir = target;
        
        opt.printAutomata = automataDir;
        
        try {
            RelaxNGCC.run(opt);
        } catch( Exception e ) {
            e.printStackTrace();
            throw new BuildException(
                "Validation failed, messages should have been provided.",
                e, location);
        }
    }





    public void setSource(String rngFile) {
        source = project.resolveFile(rngFile);
    }

    public void setTargetdir(File file) {
        this.target = file;
    }

    public void setAutomata( String dir ) {
        automataDir = project.resolveFile(dir);
    }
    
    /** Nested &lt;depends> element. */
    public void addDepends( FileSet fs ) {
        dependsSet.add(fs);
    }
    
    /** Nested &lt;produces> element. */
    public void addProduces( FileSet fs ) {
        producesSet.add(fs);
    }




    /**
     * Finds the newest/oldest timestamp from the given file set.
     */
    private long findTimestamp( ArrayList filesets, boolean findNewest ) {
        
        long lastModified;
        if( findNewest )    lastModified = Long.MIN_VALUE;
        else                lastModified = Long.MAX_VALUE;
        
        for (Iterator itr = filesets.iterator(); itr.hasNext();) {
            FileSet fs = (FileSet) itr.next();
            
            DirectoryScanner ds = fs.getDirectoryScanner(project);
            String[] includedFiles = ds.getIncludedFiles();
            File baseDir = ds.getBasedir();
            
            for (int j = 0; j < includedFiles.length; ++j) {
                File file = new File(baseDir, includedFiles[j]);
            
                log("Up-to-date check on "+file.toString(), Project.MSG_VERBOSE );

                if( findNewest )
                    lastModified = Math.max( lastModified, file.lastModified() );
                else
                    lastModified = Math.min( lastModified, file.lastModified() );
            }
        }

        if( lastModified == Long.MIN_VALUE ) // no file was found
            return Long.MAX_VALUE;  // force re-run

        if( lastModified == Long.MAX_VALUE ) // no file was found
            return Long.MIN_VALUE;  // force re-run
            
        return lastModified;
    }
    

}
