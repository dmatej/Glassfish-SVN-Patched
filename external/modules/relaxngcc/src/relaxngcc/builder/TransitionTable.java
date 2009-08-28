package relaxngcc.builder;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Arrays;
import java.util.Vector;
import java.util.Comparator;
import java.text.MessageFormat;

import relaxngcc.automaton.State;
import relaxngcc.automaton.Alphabet;
import relaxngcc.automaton.Transition;
import relaxngcc.automaton.WithOrder;

public class TransitionTable {
	
	public static class Entry {
		public Transition transition;
		public Vector alphabets;
		
		public Entry(Transition tr, Alphabet a) {
			transition = tr;
			alphabets = new Vector();
			alphabets.add(a);
		}
		public void addAlphabet(Alphabet a) {
			alphabets.add(a);
		}
	}
	
    private final Map _table = new HashMap();
    
    public void add( State s, Alphabet alphabet, Transition action ) {
        Map m = (Map)_table.get(s);
        if(m==null)
            _table.put(s, m = new HashMap());
        
        if(m.containsKey(alphabet)) {
            // TODO: proper error report
            String scopename = s.getContainer()._scope.name;
            if(scopename==null) scopename = "<start>"; //the ScopeInfo for <start> block has a null as its name
            System.err.println(MessageFormat.format(
                "State #{0}  of \"{1}\" has a conflict by {2}",
                new Object[]{
                    Integer.toString(s.getIndex()),
                    scopename,
                    alphabet } ));
            alphabet.printLocator(System.out);
        }
        m.put(alphabet,action);
    }
    
    /**
     * If EVERYTHING_ELSE is added to a transition table,
     * we will store that information here.
     */
    private final Map _eeAction = new HashMap();
    
    public void addEverythingElse( State s, Transition action ) {
        _eeAction.put(s,action);
    }
    
    /**
     * Gets the transition associated to EVERYTHING_ELSE alphabet
     * in the given state if any. Or null.
     */
    public Transition getEverythingElse( State s ) {
        return (Transition)_eeAction.get(s);
    }
    
    /**
     * Lists all entries of the transition table with
     * the specified state in terms of TrnasitionTable.Entry.
     * The resulting array is sorted in the order of Transition.
     */
    public Entry[] list( State s ) {
        Map m = (Map)_table.get(s);
        if(m==null)
            return new Entry[0];
            /*
            return new Iterator() {
                public boolean hasNext() { return false; }
                public Object next() { return null; }
                public void remove() { throw new UnsupportedOperationException(); }
            };
            */
        else {
            Map.Entry[] a = (Map.Entry[])m.entrySet().toArray(new Map.Entry[m.size()]);
            Arrays.sort(a, new Comparator() {
                public int compare(Object t1, Object t2) {
                    Object o1 = ((Map.Entry)t1).getValue();
                    Object o2 = ((Map.Entry)t2).getValue();
                    return ((WithOrder)o2).getOrder()-((WithOrder)o1).getOrder();
                }
            } );
            //making groups by transition
            Vector result = new Vector();
            Entry e = new Entry((Transition)a[0].getValue(), (Alphabet)a[0].getKey());
            result.add(e);
            for(int i=1; i<a.length; i++) {
            	if(a[i].getValue()==a[i-1].getValue()) //identical transition
            		e.addAlphabet((Alphabet)a[i].getKey());
            	else {
		            e = new Entry((Transition)a[i].getValue(), (Alphabet)a[i].getKey());
		            result.add(e);
            	}
            }            	
            	
            return (Entry[])result.toArray(new Entry[result.size()]);
            
        }
    }

}
