/*
 * Transition.java
 *
 * Created on 2001/08/04, 22:05
 */

package relaxngcc.automaton;

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

import relaxngcc.builder.ScopeInfo;
import relaxngcc.codedom.CDBlock;

/**
 * A Trnasition is a tuple of an Alphabet, a next state, and user-defined action.
 */
public final class Transition implements WithOrder
{
    private Alphabet _alphabet;
    private State _nextState;
    private final int _order;
    
    public int getOrder() {
        return _order;
    }
    
    /** value that uniquely identifies a transition. */
    private final int _uniqueId;
    
    /** Creates Transition with no action. */
    public Transition(Alphabet a, State n, int o) {
        this(a, n, new Vector(), new Vector(), o);
    }
    
    public static Transition createActionOnlyTransition(State next, ScopeInfo.Action act) {
        Transition t = new Transition(new Alphabet.ForAction(), next, Integer.MAX_VALUE); //_order of ActionOnlyTransition must be biggest
        if(act!=null) t.insertEpilogueAction(act);
        return t;
    }

    private static Vector createVector( ScopeInfo.Action a ) {
        Vector vec = new Vector();
        vec.add(a);
        return vec;
    }
    
    private Transition(Alphabet a, State n, Vector pro, Vector epi, int o) {
        _alphabet = a;
        _nextState = n;
        _prologueActions = pro;
        _epilogueActions = epi;
        _uniqueId = iotaGen++;
        _order = o;
    }

    /**
     * Actions to be executed immediately
     * before this transition is performed.
     */
    private final Vector _prologueActions;
    
    /**
     * Actions to be executed immediately
     * after this transition is performed.
     * 
     * Note that the difference between prologue
     * and epilogue is significant only for REF-type alphabets.
     */
    private final Vector _epilogueActions;
    
    /** Adds a new action at head of the prologue actions. */
    public void insertPrologueAction(ScopeInfo.Action newAction) {
        _prologueActions.add(0,newAction);
    }
    /** Adds a new action at head of the epilogue actions. */
    public void insertEpilogueAction(ScopeInfo.Action newAction) {
        _epilogueActions.add(0,newAction);
    }
    public void insertEpilogueActions(ScopeInfo.Action[] newActions) {
        for( int i=newActions.length-1; i>=0; i-- )
            insertEpilogueAction(newActions[i]);
    }
    
    /** Gets all prologue actions. */
    public ScopeInfo.Action[] getPrologueActions() {
        return toActionArray(_prologueActions);
    }
    /** Gets all epilogue actions. */
    public ScopeInfo.Action[] getEpilogueActions() {
        return toActionArray(_epilogueActions);
    }
    private static ScopeInfo.Action[] toActionArray(Vector vec) {
        return (ScopeInfo.Action[])vec.toArray(new ScopeInfo.Action[vec.size()]);
    }
    
    /** Gets the code to invoke all the prologue actions. */
    public CDBlock invokePrologueActions() {
        return invokeActions(_prologueActions);
    }
    /** Gets the code to invoke all the epilogue actions. */
    public CDBlock invokeEpilogueActions() {
        return invokeActions(_epilogueActions);
    }
    private static CDBlock invokeActions(Vector vec) {
        CDBlock sv = new CDBlock();
        for( int i=0; i<vec.size(); i++ )
            sv.add(((ScopeInfo.Action)vec.get(i)).invoke());
        return sv;
    }
    /** Returns true if this transition has any associated action. */
    public boolean hasAction() {
        return !_prologueActions.isEmpty() || !_epilogueActions.isEmpty();
    }
    
    public Object clone() {
        return clone(_nextState);
    }
    
    public Transition clone( State next ) {
        return new Transition(_alphabet, next,
            (Vector)_prologueActions.clone(), (Vector)_epilogueActions.clone(), _order);
    }

    public Alphabet getAlphabet() { return _alphabet; }
    public State nextState() { return _nextState; }
    
    public int getUniqueId() { return _uniqueId; }

    public void changeDestination(State s) { _nextState=s; }
    

    /**
     * Computes HEAD set of this transition.
     * 
     * See {@link Head} for the definition.
     */
    public Set head( boolean includeEE ) {
        Set s = new HashSet();
        head(s, new HashSet(), includeEE);
        return s;
    }
    
    /**
     * Internal function to compute HEAD(t)
     * 
     * @param includeEE
     *      If true, the return set will include EVERYTHING_ELSE
     *      when appropriate.
     */
    void head( Set result, Set checked_state, boolean includeEE ) {
        Alphabet a = getAlphabet();
        if(a.isFork()) {
            Alphabet.Fork fork = a.asFork();
            for( int i=0; i<fork._subAutomata.length; i++ )
                fork._subAutomata[i].head( result, checked_state, false );
            if(fork.isNullable())
                nextState().head( result, checked_state, includeEE );
        } else if(a.isRef()) {
            ScopeInfo target = a.asRef().getTargetScope();
            target.head( result );
            
            if( target.isNullable() )
                nextState().head( result, checked_state, includeEE );
        } else if(a.isForAction()) {
        	nextState().head(result, checked_state, includeEE);
        } else {
            result.add(a);
        }
    }        
    
    
    /** used to produce unique IDs. */
    private static int iotaGen=1;
}
