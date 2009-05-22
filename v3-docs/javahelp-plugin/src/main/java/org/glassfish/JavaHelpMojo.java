package org.glassfish.javahelp;

import com.sun.java.help.search.Indexer;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.util.ArrayList;

/**
 * Invokes Javahelp indexer
 * @goal jhindex 
 * @phase generate-resources
 */
public class JavaHelpMojo extends AbstractMojo
{
    private static Indexer indexer = new Indexer();

    /**
     * The directory generating JavaHelp Indexer.
     * 
     * @parameter default-value="src/main/resources"
     */
    private File dir;

    /**
     * @parameter
     */
    private String locale;

    /**
     * @parameter default-value="target/classes"
     */
    private String output;

    /** 
     * @parameter expression="${project}" 
     */
    private org.apache.maven.project.MavenProject mavenProject;


    public void execute() throws MojoExecutionException {

        /**
         * arguments to jhindexer
         */
        ArrayList<String> args = new ArrayList<String>();

	if ( locale != null ) {
            args.add( "-locale" );
            args.add( locale );
        }

	if ( output != null ) {
            args.add( "-db" );
            args.add( output );
        }

        args.add( dir.getAbsolutePath() );
        try {
            indexer.compile( args.toArray( new String[args.size()] ) );
        }
        catch ( Exception e ) {
            throw new MojoExecutionException("Java Help indexing exception, a full search database may not have been created.", e );
        }
    }
}
