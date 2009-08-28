/*
 * ScopeInfo.java
 *
 * Created on 2001/08/05, 14:43
 */

package relaxngcc.builder;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.Vector;

import relaxngcc.NGCCGrammar;
import relaxngcc.NGCCUtil;
import relaxngcc.Options;
import relaxngcc.automaton.Alphabet;
import relaxngcc.automaton.State;
import relaxngcc.automaton.Transition;
import relaxngcc.grammar.NGCCDefineParam;
import relaxngcc.grammar.Scope;
import relaxngcc.javabody.JavaBodyParser;
import relaxngcc.util.SelectiveIterator;
import relaxngcc.codedom.*;

/**
 * information about a scope
 */
public final class ScopeInfo
{
    public final NGCCGrammar _grammar;
    
    /** Scope object to which this object is attached. */
    public final Scope _scope;
    
    private Set _allStates;
    
    private Map _NSURItoStringConstant;
    public Iterator iterateNSURIConstants() {
        return _NSURItoStringConstant.entrySet().iterator();
    }
    
    /** Automaton that represents this _scope. */
    private State _initialState;
    public State getInitialState() { return _initialState; }
    public void setInitialState(State s) {
        _initialState = s;
    }

    /**
     * See {@link NullableChecker} for the definition of nullability.
     */
    private boolean _nullable;
    public boolean isNullable() { return _nullable; }    
    public void setNullable(boolean v) { _nullable = v; }
    
    public int getStateCount() { return _allStates.size(); }
    
    public String getClassName() {
        return _scope.getParam().className;
    }
    
    /**
     * Parameters to the constructor. Array of Aliases.
     */
    private Alias[] _constructorParams;
    
    public Alias[] getConstructorParams() { return _constructorParams; }
    
    
    private class AlphabetIterator extends SelectiveIterator {
        AlphabetIterator( Iterator base, int typeMask ) {
            super(base);
            _typeMask = typeMask;
        }
        private final int _typeMask;
        protected boolean filter( Object o ) {
            return (((Alphabet)o).getType() & _typeMask)!=0;
        }
    }
    

//    /**
//     * Fixes the attribute handlers so that a transition by
//     * an attribute will always return to the same state that
//     * it started.
//     */
//    public void copyAttributeHandlers() {
//        State[] states = (State[]) _allStates.toArray(new State[_allStates.size()]);
//        for( int i=0; i<states.length; i++ ) {
//            State st = states[i];
//            
//            Vector transitions = new Vector();
//            Iterator itr = st.iterateTransitions(Alphabet.ENTER_ATTRIBUTE);
//            while(itr.hasNext()) {
//                Transition t = (Transition)itr.next();
//                if(!t.getAlphabet().asEnterAttribute().workaroundSignificant)
//                    transitions.add(t);
//            }
//                
//            for( int j=0; j<transitions.size(); j++ ) {
//                // replace this transition by a cloned transition.
//                Transition t = (Transition)transitions.get(j);
//                Transition t2 = cloneAttributeTransition(t,st);
//                st.removeTransition(t);
//                st.addTransition(t2);
//            }
//        }
//    }
//    
//    /**
//     * Clones a sub-automaton that starts from t and ends with
//     * leaveAttribute. The destination state of such a leaveAttribute
//     * event would be re-written to the 'dest' state.
//     */
//    private Transition cloneAttributeTransition( Transition t, State dest ) {
//        return cloneAttributeTransition(t, dest, new Hashtable());
//    }
//    
//    /**
//     * @param m
//     *      map from the original state to the cloned state.
//     */
//    private Transition cloneAttributeTransition( Transition t, State dest, Map m ) {
//            
//        if(t.getAlphabet().isLeaveAttribute())
//            // this is the leave attribute transition. So go back to
//            // the 'dest' state.
//            return t.clone(dest);
//        
//        State orig = t.nextState();
//        
//        State st = (State)m.get(orig);
//        if(st==null) {
//            // we need to clone the state
//            st = new State(
//                this,
//                getStateCount(),
//                orig._locationHint);
//            addState(st);
//            m.put(orig,st);
//            
//            st.setAcceptable(orig.isAcceptable());
//            
//            // clone transitions
//            Iterator itr = orig.iterateTransitions();
//            while(itr.hasNext())
//                st.addTransition(cloneAttributeTransition(
//                    (Transition)itr.next(), dest, m));
//        }
//        
//        return t.clone(st);
//    }
    
    /**
     * Makes the automaton smaller.
     * 
     * In actuality, this method only removes unreachable states.
     */
    public void minimizeStates() {
        Stack queue = new Stack();
        Set reachable = new HashSet();
        
        queue.push(getInitialState());
        reachable.add(getInitialState());
        
        while(!queue.isEmpty()) {
            State s = (State)queue.pop();
            Iterator itr = s.iterateTransitions();
            while(itr.hasNext()) {
                Transition t = (Transition)itr.next();
                
                if(t.getAlphabet() instanceof Alphabet.Fork) {
                    State[] inits = ((Alphabet.Fork)t.getAlphabet())._subAutomata;
                    for( int i=0; i<inits.length; i++ )
                        if(reachable.add(inits[i]))
                            queue.push(inits[i]);
                }
                
                if(reachable.add(t.nextState()))
                    queue.push(t.nextState());
            }
        }
        
        _allStates.retainAll(reachable);
    }
    
    //about header and body
    private String _headerSection = "";
    public void appendHeaderSection(String c) { _headerSection += c; }
    public String getHeaderSection() { return _headerSection; }
    
    /** Name of fields defined in &lt;cc:java-body>. */
    private final Set _userDefinedFields = new HashSet();
    public boolean isUserDefinedField( String name ) {
        return _userDefinedFields.contains(name);
    }
    
    //type usage information. these flags affect the output of import statements
    private boolean _usingBigInteger;
    private boolean _usingCalendar;
    
    
    /** All _actions in this _scope. */
    private final Vector _actions = new Vector();
    public Iterator iterateActions() { return _actions.iterator(); }
    
    /** Creates a new Action object inside this _scope. */
    public Action createAction( String code ) {
        Action a = new Action(code);
        _actions.add(a);
        return a;
    }
    public Action createAction( StringBuffer code ) {
        return createAction(code.toString());
    }
    
    /**
     * User-defined code fragment.
     */
    public final class Action {
        private Action( String codeFragment ) {
            _codeFragment = codeFragment;
            _uniqueId = _actionIdGen++;
        }
        
        /** A code fragment that the user wrote. */
        private final String _codeFragment;
        public String getCodeFragment() { return _codeFragment; }
        
        /** Gets the code to invoke this action. */
        public CDStatement invoke() {
            return new CDMethodInvokeExpression("action"+_uniqueId).asStatement();
        }
        
        /** ID number that uniquely identifies this fragment. */
        private final int _uniqueId;
        public int getUniqueId() { return _uniqueId; }
        
        /** Generates the action function. */
        void generate( CDClass classdef ) {
            CDMethod method = new CDMethod(
                new CDLanguageSpecificString("private"),
                CDType.VOID,
                "action"+_uniqueId,
                new CDLanguageSpecificString("throws SAXException") );
            
            method.body().add(new CDLanguageSpecificString(_codeFragment));
            
            classdef.addMethod(method);
        }
    }
    
    /** used to generate unique IDs for Actions. */
    private int _actionIdGen = 0;
    
    
    /** All the aliases indexed by their names. */
    private final Map _aliases = new Hashtable();
    /** Iterate all the aliases. */
    public final Iterator iterateAliases() { return _aliases.entrySet().iterator(); }
    
    public ScopeInfo(NGCCGrammar g, Scope scope) {
        _grammar = g;
        _scope = scope;
        _allStates = new HashSet();
        _NSURItoStringConstant = new HashMap();

        Vector vec = new Vector();
        // parse constructor parameters
        if(_scope.getParam().params!=null) {
            StringTokenizer tokens = new StringTokenizer(_scope.getParam().params,",");
            while(tokens.hasMoreTokens()) {
                // (type,name) pair.
                String pair = tokens.nextToken().trim();
                int idx = pair.indexOf(' ');
                String vartype = pair.substring(0,idx).trim();
                String varname = pair.substring(idx+1).trim();
                
                vec.add(
                    addAlias(new CDType(vartype), varname ));
            }
        }
        _constructorParams = (Alias[])vec.toArray(new Alias[vec.size()]);
        
        
        if(_scope.getBody()!=null) {
            Reader reader = new StringReader(_scope.getBody());
            JavaBodyParser p = new JavaBodyParser(reader);
            
            try {
                p.JavaBody();       // parse the text
            } catch( Throwable e ) {
                // TODO: report error location and such.
                System.err.println("[Warning] unable to parse <java-body>");
                System.err.println("   "+e.getMessage());
            }
            
            _userDefinedFields.addAll(p.fields);
        }
    }
    
    public void simplifyAutomaton() {
        //copyAttributeHandlers();
        minimizeStates();
        
    }
    
    public void addNSURI(String nsuri)
    {
        if(_NSURItoStringConstant.containsKey(nsuri)) return;
        
        String result = "";
        if(nsuri.length()==0)
            result = "DEFAULT_NSURI";
        else {
            StringTokenizer tok = new StringTokenizer(nsuri, ":./%-~"); //loose check
            while(tok.hasMoreTokens()) {
                String part = tok.nextToken();
                if(result.length()>0) result+="_"; //delimiter
                result += part.toUpperCase();
            }
        }
        _NSURItoStringConstant.put(nsuri, result);
    }
    
    public String getNSStringConstant(String uri) {
        Object o = _NSURItoStringConstant.get(uri);
        return (String)o;
    }
    
    /**
     * Iterates states that have transitions with one of specified
     * alphabets.
     */
    public Iterator iterateStatesHaving( final int alphabetTypes ) {
        return new SelectiveIterator(iterateAllStates()) {
            protected boolean filter( Object o ) {
                return ((State)o).hasTransition(alphabetTypes);
            }
        };
    }
    
    public Iterator iterateAcceptableStates() {
        return new SelectiveIterator(_allStates.iterator()) {
            protected boolean filter( Object o ) {
                return ((State)o).isAcceptable();
            }
        };
    }
    public Iterator iterateAllStates() {
        return _allStates.iterator();
    }
    
    
    
    public void addState(State state) {
        _allStates.add(state);
    }
    
    public Alias addAlias(CDType type, String name) {
        Alias a = new Alias(type, name);
        _aliases.put(name,a);
        return a;
    }
    
    /** Returns true if this is the start pattern. */
    public boolean isRoot() { return _scope.name==null; }

    

    

    public void dump(PrintStream strm)
    {
        strm.println("Scope " + _scope.name);
        strm.print("HEAD: ");
        for (Iterator itr = head().iterator(); itr.hasNext();) {
            Alphabet a = (Alphabet) itr.next();
            strm.print(a);
            strm.print(", ");
        }
        strm.println();
    }
    
    
    
    
    
    /** Gets the display name of a state. */
    private String getStateName( State s ) {
        StringBuffer buf = new StringBuffer();
        buf.append('"');
        if(s==getInitialState()) {
            buf.append("init(");
            buf.append(s.getIndex());
            buf.append(")");
        } else {
            buf.append("s");
            buf.append(s.getIndex());
        }
            
        buf.append(buildActionList('+',s.getActionsOnExit()));
        buf.append('"');
        return buf.toString();
    }
    
    /** Gets the hue of the color for an alphabet. */
    private static String getColor( Alphabet a ) {
        // H S V
        switch(a.getType()) {
        case Alphabet.ENTER_ELEMENT:     return "0";
        case Alphabet.LEAVE_ELEMENT:     return "0.125";
        case Alphabet.ENTER_ATTRIBUTE:   return "0.25";
        case Alphabet.LEAVE_ATTRIBUTE:   return "0.375";
        case Alphabet.REF_BLOCK:         return "0.5";
        case Alphabet.DATA_TEXT:
        case Alphabet.VALUE_TEXT:        return "0.625";
        case Alphabet.FORK:              return "0.75";
        case Alphabet.FOR_ACTION:        return "0.875";
        default:
            throw new IllegalArgumentException("unexpected alphabet type "+a.getType()); // assertion failed
        }
    }
    
    /**
     * Writes the automaton by using
     * <a href="http://www.research.att.com/sw/tools/graphviz/">GraphViz</a>.
     */
    public void dumpAutomaton( File target ) throws IOException, InterruptedException {
        
        System.err.println("generating a graph to "+target.getPath());
        
        Process proc = Runtime.getRuntime().exec(
            new String[]{"dot","-Tgif","-o",target.getPath()});
        PrintWriter out = new PrintWriter(
            new BufferedOutputStream(proc.getOutputStream()));
//        PrintWriter out = new PrintWriter(System.out); // if you want to debug the input to GraphViz.
    
        out.println("digraph G {");
        out.println("node [shape=\"circle\"];");

        Iterator itr = iterateAllStates();
        while( itr.hasNext() ) {
            State s = (State)itr.next();
            if(s.isAcceptable())
                out.println(getStateName(s)+" [shape=\"doublecircle\"];");
            
            Iterator jtr = s.iterateTransitions();
            while(jtr.hasNext() ) {
                Transition t = (Transition)jtr.next();
                
                String str = MessageFormat.format(
                        "{0} -> {1} [ label=\"{2}{3}{4}\",color=\"{5} 1 .5\",fontcolor=\"{5} 1 .3\" ];",
                        new Object[]{
                            getStateName(s),
                            getStateName(t.nextState()),
                            t.getAlphabet().toString(),
                            buildActionList('^',t.getPrologueActions()),
                            buildActionList('_',t.getEpilogueActions()),
                            getColor(t.getAlphabet()) });
                out.println(str);
            }
        }
        
        out.println("}");
        out.flush();
        out.close();
        
        BufferedReader in = new BufferedReader(
            new InputStreamReader(proc.getInputStream()));
        while(true) {
            String s = in.readLine();
            if(s==null)     break;
            System.out.println(s);
        }
        in.close();
        
        proc.waitFor();
    }
    
    /** concatanates all action names (so that it can be printed out.) */
    private static String buildActionList( char head, Action[] actions ) {
        if(actions.length==0)   return "";
        
        StringBuffer label = new StringBuffer();
        label.append(head);
        for( int i=0; i<actions.length; i++ ) {
            if(i!=0)    label.append(',');
            label.append(actions[i].getUniqueId());
        }
        return label.toString();
    }
    
    private Set _cachedHEAD = null;
    
    /**
     * Computes the HEAD set of this _scope (that doesn't include
     * EVERYTHING_ELSE token.)
     * 
     * See {@link Head} for the definition.
     */
    public void head( Set result ) {
        // to speed up computation, we will cache the computed value.
        if(_cachedHEAD==null)
            _cachedHEAD = getInitialState().head(false);
        result.addAll(_cachedHEAD);
    }
    
    /**
     * Computes the HEAD set of this _scope (that doesn't include
     * EVERYTHING_ELSE token) and returns them in a new set.
     */
    public Set head() {
        Set s = new HashSet();
        head(s);
        return s;
    }
    
    /**
     * Computes the AFOLLOW set of this _scope
     */
    public void calcAFOLLOW() {
        Iterator it = iterateAllStates();
        while(it.hasNext()) {
            State s = (State)it.next();
            s.calcAFOLLOW();
        }
    }
        
}
