package org.kohsuke.args4j.opts;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineOption;
import org.kohsuke.args4j.CmdLineParser;

/**
 * Option that takes a {@link String} as a parameter.
 * 
 * <p>
 * For example, you can parse "-mode RELEASE" or
 * "-mode DEBUG" into "RELEASE"/"DEBUG".
 * 
 * @author
 *     Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class StringOption implements CmdLineOption {
    
    private final String optionName;
    
    /**
     * Value of this option. 
     */
    public String value;
        
    public StringOption( String optionName ) {
        this.optionName = optionName;
    }

    public StringOption( String optionName, String defaultValue ) {
        this(optionName);
        this.value = defaultValue;
    }
    
    public boolean accepts(String optionName) {
        return optionName.equals(this.optionName);
    }

    public int parseArguments(CmdLineParser parser, Parameters params) throws CmdLineException {
        value = params.getParameter(0);
        return 1;
    }

}
