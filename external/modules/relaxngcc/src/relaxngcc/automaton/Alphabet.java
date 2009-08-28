/*
 * Alphabet.java
 *
 * Created on 2001/08/04, 21:40
 */

package relaxngcc.automaton;
import java.io.PrintStream;
import java.util.Comparator;

import org.xml.sax.Locator;

import relaxngcc.MetaDataType;
import relaxngcc.NGCCGrammar;
import relaxngcc.builder.ScopeInfo;
import relaxngcc.grammar.NameClass;

/**
 * An alphabet in RelaxNGCC is one of following types:
 * 1. element start
 * 2. element end
 * 3. attribute start
 * 3. attribute end
 * 4. ref
 * 5. typed value (&lt;data>)
 * 6. fixed value (&lt;value>)
 *
 */
public abstract class Alphabet {
    // type of alphabets
    public static final int ENTER_ELEMENT      = 1;
    public static final int LEAVE_ELEMENT      = 2;
    public static final int ENTER_ATTRIBUTE    = 4;
    public static final int LEAVE_ATTRIBUTE    = 8;
    public static final int DATA_TEXT          = 16;
    public static final int VALUE_TEXT         = 32;
    public static final int REF_BLOCK          = 64;
    public static final int FORK               = 128;
    public static final int FOR_ACTION         = 256;
    
    /** Type of this alphabet. One of the above constants. */
    private final int _type;
    public final int getType() { return _type; }
    
    /** Source location where this alphabet came from. */
    public final Locator _locator;
    
    protected Alphabet( int type, Locator loc ) {
        _type = type;
        _locator = loc;
    }


    /** Prints the locator associated with this. */    
    public void printLocator( PrintStream out ) {
        if(_locator!=null) {
            out.print("  line ");
            out.print(_locator.getLineNumber());
            out.print(" of ");
            out.println(_locator.getSystemId());
        }
        else
            out.print("  line unknown");
    }


    
    //
    // dynamic cast functions
    //
    public Markup           asMarkup() { return null; }
        public EnterElement     asEnterElement() { return null; }
        public LeaveElement     asLeaveElement() { return null; }
        public EnterAttribute   asEnterAttribute() { return null; }
        public LeaveAttribute   asLeaveAttribute() { return null; }
    public Ref              asRef() { return null; }
    public Text             asText() { return null; }
        public ValueText        asValueText() { return null; }
        public DataText         asDataText() { return null; }
    public Fork             asFork() { return null; }
    public ForAction        asForAction() { return null; }
    
    //
    // type check functions
    //
    public final boolean isMarkup() { return asMarkup()!=null; }
    public final boolean isEnterElement() { return asEnterElement()!=null; }
    public final boolean isLeaveElement() { return asLeaveElement()!=null; }
    public final boolean isEnterAttribute() { return asEnterAttribute()!=null; }
    public final boolean isLeaveAttribute() { return asLeaveAttribute()!=null; }
    public final boolean isRef() { return asRef()!=null; }
    public final boolean isText() { return asText()!=null; }
    public final boolean isValueText() { return asValueText()!=null; }
    public final boolean isDataText() { return asDataText()!=null; }
    public final boolean isFork() { return asFork()!=null; }
    public final boolean isForAction() { return asForAction()!=null; }
    
    /**
     * Base class for (enter|leave)(Attribute|Element).
     */
    public static abstract class Markup extends Alphabet {
        protected Markup( int type, NameClass nc, Locator loc ) {
            super(type,loc);
            _nameClass = nc;
        }
        
        /**
         * Label of this transition.
         * A transition is valid if the element/attribute name
         * is accepted by this name class.
         */
        private final NameClass _nameClass;
        
        public NameClass getNameClass() { return _nameClass; }
        
        public Markup asMarkup() { return this; }
        
        public int hashCode() {
            return _nameClass.hashCode() ^ getType();
        }
        public boolean equals( Object o ) {
            if(!super.equals(o))    return false;
            return equals(_nameClass, ((Markup)o)._nameClass);
        }
    }
    
    /** Alphabet of the type "enter element." */
    public static class EnterElement extends Markup {
        public EnterElement( NameClass key, Locator loc ) {
            super( ENTER_ELEMENT, key, loc );
        }
        public EnterElement asEnterElement() { return this; }
        public String toString() { return "<"+getNameClass()+">"; }
    }
    
    /** Alphabet of the type "leave element." */
    public static class LeaveElement extends Markup {
        public LeaveElement( NameClass key, Locator loc ) {
            super( LEAVE_ELEMENT, key, loc );
        }
        public LeaveElement asLeaveElement() { return this; }
        public String toString() { return "</"+getNameClass()+">"; }
    }
    
    /** Alphabet of the type "enter attribute." */
    public static class EnterAttribute extends Markup {
        public EnterAttribute( NameClass key, Locator loc, State _leaveState ) {
            super( ENTER_ATTRIBUTE, key, loc );
            this.leaveState = _leaveState;
        }
        public EnterAttribute asEnterAttribute() { return this; }
        public String toString() { return "@"+getNameClass(); }
        
        /**
         * The state that will be reached when the whole attribute
         * is consumed.
         */
        public final State leaveState;
    }
    
    /** Alphabet of the type "leave attribute." */
    public static class LeaveAttribute extends Markup {
        public LeaveAttribute( NameClass key, Locator loc ) {
            super( LEAVE_ATTRIBUTE, key, loc );
        }
        public LeaveAttribute asLeaveAttribute() { return this; }
        public String toString() { return "/@"+getNameClass(); }
    }
    
    /**
     * Alphabet that "forks" a state into a set of sub-automata.
     * 
     * Used to handle &lt;interleave>s
     */
    public static final class Fork extends Alphabet {
        public Fork( State[] subAutomata,
            NameClass[] elementNC, NameClass[] attNC, boolean[] text,
            Locator loc ) {
            
            super( FORK, loc );
            _subAutomata = subAutomata;
            _elementNameClasses = elementNC;
            _attributeNameClasses = attNC;
            _canConsumeText = text;
        }
        
        /** Initial states of sub-automata. */
        public final State[] _subAutomata;
        
        /** NameClass that represents elements that can be consumed by each branch.*/
        public final NameClass[] _elementNameClasses;
        /** for attributes. */
        public final NameClass[] _attributeNameClasses;
        /** for texts. */
        public final boolean[] _canConsumeText;
        
        public String toString() {
            StringBuffer buf = new StringBuffer("fork&join ");
            for( int i=0; i<_subAutomata.length; i++ ) {
                if(i!=0)    buf.append(',');
                buf.append( Integer.toString( _subAutomata[i].getIndex() ) );
            }
            return buf.toString();
        }
        
        public int hashCode() {
            int h=0;
            for( int i=0; i<_subAutomata.length; i++ )
                h ^= _subAutomata[i].hashCode();
            return h;
        }
        public boolean equals( Object o ) {
            if(!super.equals(o)) return false;
            
            Fork rhs = (Fork)o;
            if(_subAutomata.length!=rhs._subAutomata.length)    return false;
            
            for( int i=_subAutomata.length-1; i>=0; i-- )
                if( _subAutomata[i]!=rhs._subAutomata[i] )
                    return false;
            
            return true;
        }
        public Fork asFork() { return this; }
        
        /**
         * Gets the name of the InterleaveFilter implementation class.
         */
        public String getClassName() {
            StringBuffer id = new StringBuffer("InterleaveFilter");
            for( int i=0; i<_subAutomata.length; i++ ) {
                id.append('_');
                id.append(_subAutomata[i].getIndex());
            }
            return id.toString();
        }
        
        /** Returns true if this fork&amp;join is nullable. */
        public boolean isNullable() {
            for( int i=0; i<_subAutomata.length; i++ ) {
                if(!_subAutomata[i].isAcceptable())
                    return false;
            }
            return true;
        }
    }
    
    /** Alphabet of the type "ref." */
    public static final class Ref extends Alphabet {
        public Ref( ScopeInfo target, String alias, String params, Locator loc ) {
            super( REF_BLOCK, loc );
            _target = target;
            _alias  = alias;
            _params = params;
        }
        public Ref( ScopeInfo _target, Locator loc ) {
            this(_target,null,null,loc);
        }
        public Ref asRef() { return this; }
        
        /** Name of the scope object to be spawned. */
        private final ScopeInfo _target;

        /** Gets the child scope to be spawned. */
        public ScopeInfo getTargetScope() {
            return _target;
        }
        
        /**
         * Additional parameters passed to
         * the constructor of the child object.
         * 
         * Used only with Alphabets of the REF_BLOCK type.
         */
        private final String _params;
        public String getParams() {
            // TODO: this might be an excessively work.
            // maybe I should just return the value as is
            if(_params==null)  return "";
            return ','+_params;
        }
        
        /**
         * User-defined variable name assigned to this alphabet.
         * User program can access this child object through this
         * variable.
         */
        private final String _alias;
        public String getAlias() { return _alias; }

        public String toString() { return "ref '"+_target.getClassName()+"'"; }
        
        public int hashCode() {
            return h(_target)^h(_alias)^h(_params);
        }
        public boolean equals( Object o ) {
            if(!super.equals(o)) return false;
            
            Ref rhs = (Ref)o;
            if(!equals(_target,rhs._target))    return false;
            if(!equals(_alias, rhs._alias ))    return false;
            return equals(_params,rhs._params);
        }
    }
    
    
    public static abstract class Text extends Alphabet {
        protected Text( int type, String alias, Locator loc ) {
            super(type,loc);
            _alias = alias;
        }
        public Text asText() { return this; }
        
        /**
         * User-defined variable name assigned to this alphabet.
         * User program can access this child object through this
         * variable.
         */
        private final String _alias;
        public String getAlias() { return _alias; }   
        
        public boolean equals( Object o ) {
            if(!super.equals(o)) return false;
            return equals(_alias,((Text)o)._alias);
        }
    }
    
    public static class ValueText extends Text {
        public ValueText( String value, String alias, Locator loc ) {
            super(VALUE_TEXT,alias,loc);
            _value = value;
        }
        public ValueText asValueText() { return this; }
        
        /**
         * Value of the &lt;value> element.
         */
        private final String _value;
        public String getValue() { return _value; }
        
        public String toString() { return "value '"+_value+"'"; }
        public int hashCode() { return _value.hashCode(); }
        public boolean equals( Object o ) {
            if(!super.equals(o)) return false;
            return _value.equals( ((ValueText)o)._value );
        }
    }
    
    public static class DataText extends Text {
        public DataText( MetaDataType dt, String alias, Locator loc ) {
            super(DATA_TEXT, alias, loc);
            _dataType = dt;
        }
        public DataText asDataText() { return this; }

        /** Datatype of this &lt;data> element. */
        private final MetaDataType _dataType;
        public MetaDataType getMetaDataType() { return _dataType; }
        
        public String toString() { return "data '"+_dataType._name+"'"; }
        public int hashCode() { return _dataType.hashCode(); }
        public boolean equals( Object o ) {
            if(!super.equals(o)) return false;
            return _dataType.equals( ((DataText)o)._dataType );
        }
    }

    public static class ForAction extends Alphabet {
        public ForAction() {
            super( FOR_ACTION, null);
        }
        public ForAction asForAction() { return this; }
        public int hashCode() { return FOR_ACTION; }
        public String toString() { return "[ACT]"; }
    }

    public  boolean equals( Object o ) {
        if(!this.getClass().isInstance(o))    return false;
        return _type==((Alphabet)o)._type;
    }
    
    // the hashCode method needs to be implemented properly
    public abstract int hashCode();

    /** Computes the hashCode of the object, even if it's null. */
    protected static int h( Object o ) {
        if(o==null) return 0;
        else        return o.hashCode();
    }
    /** Compares two objects even if some of them are null. */
    protected static boolean equals( Object o1, Object o2 ) {
        if(o1==null && o2==null)    return true;
        if(o1==null || o2==null)    return false;
        return o1.equals(o2);
    }
    
}
