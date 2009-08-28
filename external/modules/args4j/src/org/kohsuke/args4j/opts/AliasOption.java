package org.kohsuke.args4j.opts;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineOption;
import org.kohsuke.args4j.CmdLineParser;

/**
 * Option which is simply an alias to a set of other options. 
 * 
 * <p>
 * For example, you could define "-quiet" as "-verbose 0".
 * 
 * @author
 *     Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class AliasOption implements CmdLineOption {
    
    private final String optionName;
    private final String[] realOptions;
    
    public AliasOption( String optionName, String[] realOptions ) {
        this.optionName = optionName;
        this.realOptions = realOptions;
    }
    
    public boolean accepts(String optionName) {
        return optionName.equals(this.optionName);
    }

    public int parseArguments(CmdLineParser parser, Parameters params) throws CmdLineException {
        parser.parse(realOptions);
        return 0;
    }

}
