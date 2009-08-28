import java.io.IOException;
import java.io.PrintStream;
import java.util.List;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.opts.BooleanOption;
import org.kohsuke.args4j.opts.IntOption;
import org.kohsuke.args4j.opts.OutputStreamOption;
import org.kohsuke.args4j.opts.StringOption;


/**
 * Sample program that shows how you can use it.
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class SampleMain {
    
    //
    // option constants.
    //
    // define three options. -r, -o <file>, -str <token>, and -n <num>.
    public BooleanOption recursive = new BooleanOption("-r");
    public OutputStreamOption out  = new OutputStreamOption("-o","-");
    public StringOption str        = new StringOption("-str","(default value)");
        // you can also specify the default value
    public IntOption num           = new IntOption("-n");
    
    public static void main(String[] args) throws IOException {
        new SampleMain().doMain(args);
    }
    
    public void doMain(String[] args) throws IOException {
        CmdLineParser parser = new CmdLineParser();
        if(false) {
            // you can either add options individually
            parser.addOption(recursive);
            parser.addOption(out);
            parser.addOption(str);
            parser.addOption(num);
        } else {
            // or you can add all the CmdLineOption fields of the specified
            // object into the parser.
            parser.addOptionClass(this);
        }
        
        try {
            // parse the arguments.
            parser.parse(args);
            
            // you can parse more arguments if you want.
            // parser.parse(new String[]{"more","args"});
        } catch( CmdLineException e ) {
            // if there's a problem in the command line,
            // you'll get an exception. this will report
            // an error message.
            System.err.println(e.getMessage());
            return;
        }
        
        // this will redirect the output to the specified output
        System.setOut(new PrintStream(out.createOutputStream()));
        
        if( recursive.isOn() )
            System.out.println("-r flag is set");
        
        System.out.println("-str was "+str.value);

        if( num.isSet )
            System.out.println("-n was "+num.value);
        
        // access non-option arguments
        System.out.println("other arguments are:");
        List rest = parser.getArguments();
        for( int i=0; i<rest.size(); i++ )
            System.out.println(rest.get(i));
    }
}
