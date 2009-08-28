/*
 * State.java
 *
 * Created on 2001/08/04, 22:02
 */

package relaxngcc.automaton;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;

import org.xml.sax.Locator;

import relaxngcc.builder.ScopeInfo;
import relaxngcc.grammar.Pattern;
import relaxngcc.util.SelectiveIterator;
import relaxngcc.codedom.CDBlock;

/**
 * A State object has zero or more Transition objects
 */
public final class State implements Comparable
{
    private Set _allTransitions;
    
    //acceptable or not
    private boolean _acceptable;
    public void setAcceptable(boolean newvalue) { _acceptable=newvalue; }
    public boolean isAcceptable() { return _acceptable; }
    
//    // joinable or not.
//    // TODO: probably we don't need to distinguish final states and join states.
//    private boolean _join=false;
//    public void markAsJoin() { _join=true; }
//    public boolean isJoinState() { return _join; }
    
    /** Actions that get executed when the execution leaves this state. */
    private final Vector _actionsOnExit = new Vector();
    
    public ScopeInfo.Action[] getActionsOnExit() {
        return (ScopeInfo.Action[])_actionsOnExit.toArray(new ScopeInfo.Action[_actionsOnExit.size()]);
    }
    /** Gets the code to invoke exit-actions. */
    public void outputActionsOnExit(CDBlock sv) {
        for( int i=0; i<_actionsOnExit.size(); i++ )
            sv.add(((ScopeInfo.Action)_actionsOnExit.get(i)).invoke());
    }
    
    public void addActionOnExit(ScopeInfo.Action act) {
        _actionsOnExit.add(0,act);
    }
    public void addActionsOnExit(ScopeInfo.Action[] act) {
        for( int i=act.length-1; i>=0; i-- )
            addActionOnExit(act[i]);
    }
    
    /** ScopeInfo that owns this state. */
    private final ScopeInfo _container;
    public ScopeInfo getContainer() { return _container; }
    
    //index identifies this state in a scope as an integer
    public int getIndex() { return _index; }
    private int _index;
    
    /** Pattern from which this state was created. */
    public final Pattern _locationHint;
    
    /**
     * 
     * @param location
     *      Indicates the pattern object from which this state is created.
     */
    public State(ScopeInfo container, int index, Pattern location )
    {
        _container = container;
        _allTransitions = new TreeSet(WithOrder.orderComparator);
        
        _acceptable = false;
        _index = index;
        _locationHint = location;
    }

    public void addTransition(Transition t) {
        _allTransitions.add(t);
    }

    public void removeTransition( Transition t) {
        _allTransitions.remove(t);
    }
    
    private class TypeIterator extends SelectiveIterator {
        TypeIterator( int typeMask ) {
            super(_allTransitions.iterator());
            _typeMask = typeMask;
        }
        private final int _typeMask;
        protected boolean filter( Object o ) {
            return (((Transition)o).getAlphabet().getType()&_typeMask)!=0;
        }
    }

    public Iterator iterateTransitions() { return _allTransitions.iterator(); }

    /**
     * Checks if this state has transitions with
     * at least one of given types of alphabets.
     * 
     * @param alphabetTypes
     *      OR-ed combination of alphabet types you want to iterate.
     */
    public boolean hasTransition( int alphabetTypes ) {
        return new TypeIterator(alphabetTypes).hasNext();
    }
    
    /**
     * Iterate transitions with specified alphabets.
     * 
     * @param alphabetTypes
     *      OR-ed combination of alphabet types you want to iterate.
     */
    public Iterator iterateTransitions( int alphabetTypes ) {
        return new TypeIterator(alphabetTypes);
    }
        
    public int compareTo(Object obj) {
        if(!(obj instanceof State)) throw new ClassCastException("not State object");
        
        return _index-((State)obj)._index;
    }
    
    public void mergeTransitions(State s) {
        if(this==s)
            // this causes ConcurrentModificationException.
            // so we need to treat this as a special case.
            // 
            // merging a state to itself without any action
            // is a no-operation. so we can just return.
            return;
        mergeTransitions(s, null);
    }
    
    /**
     * For all the transitions leaving from the specified state,
     * add it to this state by appending the specified action
     * (possibly null) at the head of its prologue actions.
     */
    public void mergeTransitions(State s, ScopeInfo.Action action) {
        Iterator itr = s.iterateTransitions();
        while(itr.hasNext())
            // TODO: why there needs to be two methods "addTransitionWithCheck" and "addTransition"?
            addTransitionWithCheck( (Transition)itr.next(), action );
    }
    
    /**
     * finds a transition invoked by the passed alphabet
     */
    public Transition findTransition(Alphabet a) {
        Iterator it = iterateTransitions();
        while(it.hasNext()) {
            Transition t = (Transition)it.next();
            if(a.equals(t.getAlphabet())) return t;
        }
        return null;
    }
    
    //reports if this state has ambiguous transitions. [target] is a set of Transitions.
    /**
     * Adds the specified transition to this state,
     * and reports any ambiguity error if detected.
     */
    private void addTransitionWithCheck(
        Transition newtransition, ScopeInfo.Action action)
    {
        Alphabet a = newtransition.getAlphabet();
        
        Iterator it = iterateTransitions();
        while(it.hasNext()) {
            Transition tr = (Transition)it.next();
            Alphabet existing_alphabet = tr.getAlphabet();
            
            if(existing_alphabet.equals(a)) {
                if(tr.nextState()==newtransition.nextState()) {
                    if(action==null)
                        return; // trying to add the same transition. no-op.
                    else
                        // the same transition is being added but with an action.
                        // I guess this is ambiguous, but not sure. - Kohsuke
                        printAmbiguousTransitionsWarning(tr, newtransition);
                } else {
                    if(!newtransition.hasAction() && !tr.hasAction()) {
                        // only if both of them have no action, we can merge them.
                        tr.nextState().mergeTransitions(newtransition.nextState());
                        return; //ignores newtransition
                    } else
                        printAmbiguousTransitionsWarning(tr, newtransition);
                }
            }
        }
        
        // always make a copy, because we might modify actions later.
        // in general, it is dangerous to share transitions.
        newtransition = (Transition)newtransition.clone();
        
        if(action!=null)
            newtransition.insertPrologueAction(action);
        
        _allTransitions.add(newtransition);
    }

    private void printAmbiguousTransitionsWarning(Transition a, Transition b) {
        PrintStream s = System.err;
        printStateWarningHeader(s);
        s.print(" has ambiguous transitions: ");
        s.print(a.getAlphabet().toString());
        s.print("(to #");
        s.print(a.nextState().getIndex());
        s.print(") and ");
        s.print(b.getAlphabet().toString());
        s.print("(to #");
        s.print(b.nextState().getIndex());
        s.println(".)");
        a.getAlphabet().printLocator(s);
        b.getAlphabet().printLocator(s);
    }
    
    private void printStateWarningHeader(PrintStream s) {
        s.print("[Warning] ");

        s.print("State #");
        s.print(_index);
        s.print(" of ");
        s.print(_container._scope.name);
        // TODO: location
//        s.print(_Container.getLocation());
    }

    /**
     * Gets all the states reachable from this state.
     */
    public State[] getReachableStates() {
        HashSet s = new HashSet();
        getReachableStates(s);
        return (State[]) s.toArray(new State[s.size()]);
    }
    
    private void getReachableStates( Set r ) {
        r.add(this);
        for (Iterator itr = iterateTransitions(); itr.hasNext();) {
            Transition t = (Transition) itr.next();
            if(r.add(t.nextState()))
                t.nextState().getReachableStates(r);
        }
    }
    
    
    /**
     * Computes HEAD set of this state.
     * 
     * See {@link Head} for the definition.
     */
    public Set head( boolean includeEE ) {
        Set s = new HashSet();
        head(s, new HashSet(), includeEE);
        return s;
    }
    
    /**
     * Internal function to compute HEAD.
     */
    void head( Set result, Set checked_state, boolean includeEE ) {

		if(checked_state.contains(this)) return;
		checked_state.add(this);
        
        if(isAcceptable() && includeEE )
            result.add(Head.EVERYTHING_ELSE);
        
        Iterator itr = iterateTransitions();
        while(itr.hasNext()) {
            Transition t = (Transition)itr.next();
            t.head( result, checked_state, includeEE );
        }
    }
    
    /**
     * Computes ATTHEAD set of this state and returns them
     * in a sorted order.
     * 
     * See {@link HEAD} for the definition.
     */
    public Set attHead() {
        Set r = new HashSet();
        attHead(r);
        return r;
    }
    
    // internal-version
    private void attHead( Set result ) {
        Iterator itr = iterateTransitions();
        while(itr.hasNext()) {
            Transition t = (Transition)itr.next();
            Alphabet a = t.getAlphabet();
            
            if(a.isEnterAttribute())
                result.add(a);
            else if(a.isRef()) {
                // ref[X] itself will be included in ATTHEAD
                result.add(a);
                if(a.asRef().getTargetScope().isNullable())
                    t.nextState().attHead(result);
            }
            else if(a.isFork()) {
                result.add(a);
                if(a.asFork().isNullable())
                    t.nextState().attHead(result);
            }
        }
    }

    private Set _cachedAFollow;
    
    public Set AFollow() {
        return _cachedAFollow;
    }
    
    /**
     * Computes AFOLLOW from this state.
     */
    public void calcAFOLLOW() {
        HashSet t = new HashSet();
        HashSet c = new HashSet();
        AFollow(t, c);
        _cachedAFollow = t;
    }
    
    /**
     * The actual meta of AFOLLOW computation. AFOLLOW(s) is a set
     * of enterElement/leaveElement/text alphabets that can be
     * reached directly if one "collapses" attribute transitions.
     * 
     * @param   result
     *      This set will receive AFOLLOW.
     * @param   checked_state
     *      Used to detect cycles.
     */
    private void AFollow(Set result, Set checked_state) {
    	if(checked_state.contains(this)) return;
    	checked_state.add(this);
    	
        Iterator itr = iterateTransitions();
        while(itr.hasNext()) {
            Transition t = (Transition)itr.next();
            Alphabet a = t.getAlphabet();
            
            if(a.isEnterElement() || a.isLeaveElement() || a.isText())
                result.add(a);
            else if(a.isEnterAttribute()) {
                a.asEnterAttribute().leaveState.AFollow(result,checked_state);
            } else if(a.isRef()) {
                ScopeInfo si = a.asRef().getTargetScope();
                si.getInitialState().AFollow(result, checked_state);
                if(si.isNullable())
                    t.nextState().AFollow(result, checked_state);
            }
            else if(a.isFork()) {
                Alphabet.Fork fork = a.asFork();
                for( int i=0; i<fork._subAutomata.length; i++ )
                    fork._subAutomata[i].AFollow(result, checked_state);
                if(a.asFork().isNullable())
                    t.nextState().AFollow(result, checked_state);
            }
            else if(a.isForAction()) {
                t.nextState().AFollow(result,checked_state);
            }
        }
        
        if(isAcceptable()) result.add(Head.EVERYTHING_ELSE);
    }
}
