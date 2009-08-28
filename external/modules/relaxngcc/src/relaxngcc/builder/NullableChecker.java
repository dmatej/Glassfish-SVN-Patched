package relaxngcc.builder;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import relaxngcc.NGCCGrammar;
import relaxngcc.automaton.Alphabet;
import relaxngcc.automaton.State;
import relaxngcc.automaton.Transition;

/**
 * Computes the nullability of scopes.
 * 
 * <p>
 * A state s is said to be "reachable by epsilon" if
 *
 * <ol>
 * <li>it is an initial state, or
 * <li>a state s' is nullable, and there is a transition from s' to s by ref[X],
 *      and X is nullable.
 * </ol>
 * 
 * <p>
 * We say an automaton X is nullable if one of its final state is
 * "reachable by epsilon." Informally, this means X can accept
 * the empty tree.
 *
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class NullableChecker {
    
    public static void computeNullability( NGCCGrammar gram ) {
        // states determined to be reachable by epsilon
        Set erStates = new HashSet();
        
        // ScopeInfos that are determined to be nullable
        Set nullableScopes = new HashSet();
        
        
        // start by all initial states
        Iterator itr = gram.iterateScopeInfos();
        while(itr.hasNext()) {
            State s = ((ScopeInfo)itr.next()).getInitialState();
            erStates.add(s);
            if(s.isAcceptable())
                nullableScopes.add(s.getContainer());
        }
        
        int oldSize;
        do {
            oldSize = erStates.size();
            
            // check all the transitions from erStates
            State[] ss = (State[]) erStates.toArray(new State[erStates.size()]);
            for( int i=ss.length-1; i>=0; i-- ) {
                Iterator jtr = ss[i].iterateTransitions(Alphabet.REF_BLOCK);
                while(jtr.hasNext()) {
                    Transition t = (Transition)jtr.next();
                    ScopeInfo target = t.getAlphabet().asRef().getTargetScope();
                    if(nullableScopes.contains(target)) {
                        // t.nextState is reachable from another epsilon-reachable
                        // state by ref[X] transition.
                        State ns = t.nextState();
                        erStates.add(ns);
                        if(ns.isAcceptable())
                            nullableScopes.add(ns.getContainer());
                    }
                }
                
                jtr = ss[i].iterateTransitions(Alphabet.FOR_ACTION);
                while(jtr.hasNext()) {
                	State ns = ((Transition)jtr.next()).nextState();
                	erStates.add(ns);
                	if(ns.isAcceptable())
                        nullableScopes.add(ns.getContainer());
                }
                	
            }
            // repeat this process while erStates grow.
        } while( oldSize != erStates.size() );
        
        
        // set the nullability to each Scope
        ScopeInfo[] si = (ScopeInfo[])nullableScopes.toArray(
            new ScopeInfo[nullableScopes.size()]);
        
        for( int i=0; i<si.length; i++ ) {
            si[i].setNullable(true);
        }
    }
}

