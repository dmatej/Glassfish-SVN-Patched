package org.kohsuke.args4j.opts;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineOption;
import org.kohsuke.args4j.CmdLineParser;

/**
 * Option that accompanies a strong value and
 * can be specified multiple times.
 * 
 * <p>
 * For example, you can parse <tt>"-J opt1 -J opt2 -J opt3"</tt>
 * into {"opt1","opt2","opt3"}.
 * 
 * @author
 *     Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class MultiStringOption implements CmdLineOption {
    
    private final String optionName;
    
    /**
     * Actual modifiable data storage.
     */
    private final List _values;
    
    /**
     * Read-only view of the data store.
     */
    public final List values; 
        
    public MultiStringOption( String optionName ) {
        this( optionName, new ArrayList() );
    }
    
    /**
     * Creates a new {@link MultiStringOption} that store
     * values into the specified {@link List}.
     */
    public MultiStringOption( String optionName, List storage ) {
        this.optionName = optionName;
        this._values = storage;
        this.values = Collections.unmodifiableList(_values);
    }
    
    
    public boolean accepts(String optionName) {
        return optionName.equals(this.optionName);
    }

    public int parseArguments(CmdLineParser parser, Parameters params) throws CmdLineException {
        _values.add(params.getParameter(0));
        return 1;
    }

}
