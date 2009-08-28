package org.kohsuke.args4j.opts;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineOption;
import org.kohsuke.args4j.CmdLineParser;

/**
 * Option that takes a <tt>int</tt> as a parameter.
 * 
 * <p>
 * For example, you can parse "-n 9" or
 * "-n 0" into 9 and 0 respectively.
 * 
 * @author
 *     Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class IntOption implements CmdLineOption {
    
    private final String optionName;
    
    /**
     * Value of this option. 
     */
    public int value;
    
    /**
     * True if the option was explicitly set.
     */
    public boolean isSet = false;
        
    public IntOption( String optionName ) {
        this.optionName = optionName;
    }

    public IntOption( String optionName, int defaultValue ) {
        this(optionName);
        this.value = defaultValue;
    }
    
    public boolean accepts(String optionName) {
        return optionName.equals(this.optionName);
    }

    public int parseArguments(CmdLineParser parser, Parameters params) throws CmdLineException {
        value = params.getIntParameter(0);
        isSet = true;
        return 1;
    }

}
