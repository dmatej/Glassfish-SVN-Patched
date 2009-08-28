package org.kohsuke.args4j.opts;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineOption;
import org.kohsuke.args4j.CmdLineParser;

/**
 * Boolean option.
 * 
 * The presence of the option sets the flag.
 * Also, it recognized <tt>-opt-</tt> <tt>-opt+</tt>
 * to turn on/off the flag respectively.
 * 
 * @author
 *     Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class BooleanOption implements CmdLineOption {
    
    private final String optionName;
    
    /**
     * Value of this option. 
     */
    public boolean value;
        
    public BooleanOption( String optionName ) {
        this.optionName = optionName;
    }

    public BooleanOption( String optionName, boolean defaultValue ) {
        this(optionName);
        this.value = defaultValue;
    }
    
    public boolean accepts(String optionName) {
        if( optionName.equals(this.optionName) ) {
            value = true;
            return true;
        }
        if( optionName.equals(this.optionName+"+") ) {
            value = true;
            return true;
        }
        if( optionName.equals(this.optionName+"-") ) {
            value = false;
            return true;
        }
        return false;
    }

    public int parseArguments(CmdLineParser parser, Parameters params) throws CmdLineException {
        return 0;
    }
    
    /**
     * Returns true if this switch is on.
     */
    public final boolean isOn() {
        return value;
    }

    /**
     * Returns true if this switch is off.
     */
    public final boolean isOff() {
        return !value;
    }
}
