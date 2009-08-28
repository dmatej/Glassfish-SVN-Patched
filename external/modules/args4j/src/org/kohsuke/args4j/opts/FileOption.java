package org.kohsuke.args4j.opts;

import java.io.File;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineOption;
import org.kohsuke.args4j.CmdLineParser;

/**
 * Option that takes {@link File} as a parameter.
 * 
 * @author
 *     Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class FileOption implements CmdLineOption {
    
    private final String optionName;
    
    /**
     * Value of this option. 
     */
    public File value;
        
    public FileOption( String optionName ) {
        this.optionName = optionName;
    }

    public FileOption( String optionName, File defaultValue ) {
        this(optionName);
        this.value = defaultValue;
    }
    
    public boolean accepts(String optionName) {
        return optionName.equals(this.optionName);
    }

    public int parseArguments(CmdLineParser parser, Parameters params) throws CmdLineException {
        value = new File(params.getParameter(0));
        return 1;
    }
}
