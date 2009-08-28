package org.kohsuke.args4j;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Command line argument parser.
 * 
 * @author
 *     Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public final class CmdLineParser {
    /**
     * Set of registered {@link CmdLineOption}s.
     */
    private final List options = new ArrayList(50);
    
    /**
     * Non-option arguments that are parsed.
     */
    private final List arguments = new ArrayList(50);
    
    /**
     * Read-only view of {@link #arguments}.
     */
    private final List readonlyArguments = Collections.unmodifiableList(arguments);
    
    /**
     * Adds a new option to the parser.
     * 
     * @return
     *      The value passed as the <code>opt</code> parameter.
     */
    public CmdLineOption addOption( CmdLineOption opt ) {
        this.options.add(opt);
        return opt;
    }
    
    /**
     * Adds all the {@link CmdLineOption}-derived fields on
     * this object.
     * 
     * <p>
     * This method uses Java reflection to the specified object,
     * and looks for fields whose types derive from {@link CmdLineOption}.
     * All such fields are added through {@link #addOption(CmdLineOption)}
     * method.
     * 
     * <p>
     * This method would be convenient if you have a class that
     * defines a bunch of {@link CmdLineOption}s as its fields.
     * For example,
     * 
     * <pre>
     * class MyMain {
     *     BooleanOption opt1 = ...;
     *     StringOption  opt2 = ...;
     *     IntOption     opt3 = ...;
     *     ...
     * 
     *     void doMain( String[] args ) {
     *         CmdLineParser parser = new CmdLineParser();
     *         parser.addOptionClass(this);
     * 
     *         parser.parse(args);
     * 
     *         ....
     *     }
     * }
     * 
     * <p>
     * Note that because of the access modifier restriction,
     * only public fields of a clss can be registered through
     * this method.
     * 
     * @throws IllegalArgumentException
     *      If the specified object doesn't contain any 
     *      {@link CmdLineOption} fields. Given the typical
     *      use case, this is more likely to be a bug of the
     *      caller, but I appreciate your input on this behavior.
     */
    public void addOptionClass( Object obj ) {
        Field[] fields = obj.getClass().getFields();
        
        if( fields.length==0 )
            throw new IllegalArgumentException();
        
        for( int i=0; i<fields.length; i++ ) {
            if( CmdLineOption.class.isAssignableFrom(fields[i].getType()) )
                try {
                    // a CmdLineOption field
                    addOption( (CmdLineOption) fields[i].get(obj) );
                } catch (IllegalArgumentException e) {
                    // can't happen
                    e.printStackTrace();
                } catch (IllegalAccessException e) {
                    // can't happen, too.
                    e.printStackTrace();
                }
        }
    }
    
    /**
     * Parse the arguments.
     * 
     * Can be invoked multiple times or recursively if necessary.
     */
    public void parse( String[] args ) throws CmdLineException {
        CmdLineImpl cmdLine = new CmdLineImpl(args);
        
        while( cmdLine.hasMore() ) {
            String arg = cmdLine.getOptionName();
            if( arg.startsWith("-") & arg.length()>1 ) {
                // parse this as an option.
                int j;
                for( j=0; j<options.size(); j++ ) {
                    CmdLineOption opt = (CmdLineOption)options.get(j);
                    if( opt.accepts(arg) ) {
                        int diff = opt.parseArguments(this,cmdLine);
                        cmdLine.proceed(diff+1);
                        break;
                    }
                }
                if( j==options.size() )
                    throw new UndefinedOptionException(arg);
            } else {
                // parse this as arguments
                arguments.add(arg);
                cmdLine.proceed(1);
            }
        }
    }
    
    /**
     * Returns the list of non-option arguments.
     * 
     * <p>
     * For example, if this parser is being used by javac
     * and the command line is like "-d x abc.java def.java -target 1.1",
     * then this method will return ["abc.java","def.java"].
     */
    public List getArguments() {
        return readonlyArguments;
    }
    
//    /**
//     * Gets the usage message generated from registered options.
//     * 
//     * @return
//     *      non-null valid string that ends with '\n' and that
//     *      doesn't begin with '\n' (or "" if no option is registered.)
//     */
//    public final String getUsage() {
//        StringBuffer buf = new StringBuffer();
//        for( int i=0; i<options.size(); i++ )
//            ((CmdLineOption)options.get(i)).appendUsage(buf);
//        return buf.toString();
//    }
//    
//    /**
//     * Prints the usage messages.
//     * 
//     * Just a convenience method for <code>out.print(getUsage())</code>.
//     */
//    public final void printUsage( PrintStream out ) {
//        out.print(getUsage());
//    }
    
    
    /**
     * Essentially a pointer over a {@link String} array.
     * Can move forward, can look ahead.
     */
    private class CmdLineImpl implements CmdLineOption.Parameters {
        private final String[] args;
        private int pos;
        
        CmdLineImpl( String[] args ) {
            this.args = args;
            pos = 0;
        }
        
        private boolean hasMore() {
            return pos<args.length;
        }

        private String getCurrentToken() {
            return args[pos];
        }
        
        private void proceed( int n ) {
            pos += n;
        }
        

        public String getOptionName() {
            return getCurrentToken();
        }

        public String getParameter(int idx) throws CmdLineException {
            if( pos+idx+1>=args.length )
                throw new MissingOptionParameterException(getOptionName());
            return args[pos+idx+1];
        }

        public int getIntParameter(int idx) throws CmdLineException {
            String token = getParameter(idx);
            try {
                return Integer.parseInt(token);
            } catch( NumberFormatException e ) {
                throw new IllegalOptionParameterException(getOptionName(),token);
            }
        }
    }
    
}
