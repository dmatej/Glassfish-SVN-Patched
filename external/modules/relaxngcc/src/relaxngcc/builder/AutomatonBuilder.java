/*
 * ScopeBuilder.java
 *
 * Created on 2001/08/04, 22:15
 */

package relaxngcc.builder;
import java.util.Iterator;

import relaxngcc.MetaDataType;
import relaxngcc.NGCCGrammar;
import relaxngcc.automaton.Alphabet;
import relaxngcc.automaton.State;
import relaxngcc.automaton.Transition;
import relaxngcc.codedom.CDType;
import relaxngcc.grammar.AttributePattern;
import relaxngcc.grammar.ChoicePattern;
import relaxngcc.grammar.DataPattern;
import relaxngcc.grammar.ElementPattern;
import relaxngcc.grammar.EmptyPattern;
import relaxngcc.grammar.GroupPattern;
import relaxngcc.grammar.InterleavePattern;
import relaxngcc.grammar.JavaBlock;
import relaxngcc.grammar.ListPattern;
import relaxngcc.grammar.NameClass;
import relaxngcc.grammar.NotAllowedPattern;
import relaxngcc.grammar.OneOrMorePattern;
import relaxngcc.grammar.Pattern;
import relaxngcc.grammar.PatternFunction;
import relaxngcc.grammar.RefPattern;
import relaxngcc.grammar.Scope;
import relaxngcc.grammar.ValuePattern;

/**
 * Builds an automaton from {@link Scope} object.
 * 
 * <p>
 * This function returns {@link String}.
 * 
 * @author Daisuke Okajima
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class AutomatonBuilder implements PatternFunction
{
    private static final String NEWLINE = System.getProperty("line.separator");
    
    /**
     * Used to give order numbers to EnterAttribute alphabets.
     */
    private int _orderCounter;
    
    /**
     * Actions are added to this buffer until it is processed.
     * Note that we can't use StringBuffer bcause we need to
     * add *in front*, not at the end.
     */
    private String _preservedAction="";

    
    
    /** Builds ScopeInfo. */
    public static void build( NGCCGrammar grammar, ScopeInfo scope ) {
        new AutomatonBuilder(grammar,scope).build();
    }
    
    private AutomatonBuilder( NGCCGrammar grammar, ScopeInfo scope ) {
        _grammar = grammar;
        _scopeInfo = scope;
    }
    
    private final NGCCGrammar _grammar;
    private final ScopeInfo _scopeInfo;

    private State _destination;
    
    public void build() {
        //starts from final state
        _destination = createState(null);
        _destination.setAcceptable(true);

        State initial = (State)_scopeInfo._scope.getPattern().apply(this);
        initial = addAction(initial,true);
                
        _scopeInfo.setInitialState(initial);
        
    }
    
    
    public Object element( ElementPattern pattern ) {
        NameClass nc = pattern.name;
        
        State tail = createState(pattern);
        Transition te = createTransition(
            new Alphabet.LeaveElement(nc,pattern.startLocator), _destination);
        addAction(te,false);
        tail.addTransition(te);
        
        // process descendants
        _destination = tail;
        State middle = (State)pattern.body.apply(this);
        
        State head = createState(pattern);
        Transition ts = createTransition(
            new Alphabet.EnterElement(nc,pattern.endLocator), middle);
        addAction(ts,true);
        head.addTransition(ts);
        
        return head;
    }


    public Object attribute( AttributePattern pattern ) {
        NameClass nc = pattern.name;
        State orgdest = _destination;

        State tail = createState(pattern);
        Transition te = createTransition(
            new Alphabet.LeaveAttribute(nc,pattern.startLocator),
            _destination);
        addAction(te,false);
        tail.addTransition(te);
  
        _destination = tail;
        State middle = (State)pattern.body.apply(this);      
        
        Alphabet.EnterAttribute ea = new Alphabet.EnterAttribute(
            nc,pattern.endLocator,orgdest);
        
        Transition ts = createTransition(
            ea,
            middle);
        addAction(ts,true);
        
        // if a special flag is specified by the user,
        // do NOT treat it as an optional attribute
        State head = createState(pattern);
        //State head = orgdest;
        head.addTransition(ts);
        _destination = orgdest;
        return head;
    }

    public Object data( DataPattern pattern ) {
        State result = createState(pattern);
        if(pattern.alias!=null)
            _scopeInfo.addAlias( CDType.STRING, pattern.alias );
        
        Transition t = createTransition(
            new Alphabet.DataText(pattern.type,pattern.alias,pattern.locator),
            _destination);
        addAction(t, false);
        result.addTransition(t);
        return result;
    }
    
    public Object empty( EmptyPattern pattern ) {
        return _destination;
    }
    public Object notAllowed( NotAllowedPattern pattern ) {
        // return a non-reachable state
        return createState(pattern);
    }

    public Object value( ValuePattern pattern ) {
        if(pattern.alias!=null)
            _scopeInfo.addAlias( CDType.STRING, pattern.alias );
        
        State result = createState(pattern);
        Transition t = createTransition(
            new Alphabet.ValueText(pattern.value, pattern.alias, pattern.locator),
            _destination);
        addAction(t,false);
        result.addTransition(t);
        return result;
    }


    public Object list( ListPattern pattern ) {
        if(pattern.alias!=null) {
            // don't treat this list as a structured text.
            _scopeInfo.addAlias(CDType.STRING, pattern.alias );
            
            State result = createState(pattern);
            Transition t = createTransition(
                new Alphabet.DataText(
                    new MetaDataType("string"),
                    pattern.alias,pattern.locator),
                _destination);
            addAction(t,false);
            result.addTransition(t);
            return result;
        } else {
            // treat this list as a structured text
            State head = (State)pattern.p.apply(this);
            
            // then append the "header" transition that tokenizes the text.
            _scopeInfo.addAlias(CDType.STRING, "__text" );
            Transition tr = new Transition(
                new Alphabet.DataText(
                    new MetaDataType("string"), "__text", pattern.locator ),
                head,
                _orderCounter++ );
            tr.insertEpilogueAction(_scopeInfo.createAction(
                "$runtime.processList(__text);"));
            addAction(tr,false);
            // add user-defined action before the processList method,
            // so that those are executed before <list> is processed.
            
            State top = createState(pattern);
            top.addTransition(tr);
            
            return top;
        }
    }
    

    public Object javaBlock( JavaBlock block ) {
        // because we traverse the _grammar backward, we need
        // to add new code in front of the existing ones.
        // it's also safe to add NL because the new code might
        // contain a comment at its end.
        _preservedAction = block.code+NEWLINE+_preservedAction;
        return _destination;
    }
    
    public Object group( GroupPattern pattern ) {
        // build automaton in a reverse order.
        // TODO: how about actions?
        _destination = (State)pattern.p2.apply(this);
        return               pattern.p1.apply(this);
    }
    
    public Object choice( ChoicePattern pattern ) {
        
        ScopeInfo.Action act = collectAction();

        State tail = _destination;
        if(act!=null) {
            tail = createState(_destination._locationHint);
            if(_destination.isAcceptable()) tail.setAcceptable(true);
            tail.addTransition(Transition.createActionOnlyTransition(_destination, act));
        }
        
        // a branch could be empty, in that case head could be returned
        // as the head of a branch. This would cause a weird effect.
        // so we should better create a new state.
        
        State head;
        /* if this choice pattern originates in an <optional> element, 
         * the use of ACTION_ONLY transition reduces the size of automaton rather than the merge of transition.
         */
        if(pattern.p1 instanceof EmptyPattern) {
        	head = createState(pattern);
	        _destination = tail;
	        processChoiceBranch(head,pattern.p2);
	        head.addTransition(Transition.createActionOnlyTransition(tail, null));
	        if(tail.isAcceptable()) head.setAcceptable(true);
        }
        else if(pattern.p2 instanceof EmptyPattern) {
        	head = createState(pattern);
	        _destination = tail;
	        processChoiceBranch(head,pattern.p1);
	        head.addTransition(Transition.createActionOnlyTransition(tail, null));
	        if(tail.isAcceptable()) head.setAcceptable(true);
        }
        else {
        	head = createState(pattern);
        
	        _destination = tail;
	        processChoiceBranch(head,pattern.p2);
	        _destination = tail;
	        processChoiceBranch(head,pattern.p1);
		}        
        return head;
    }
    
    private void processChoiceBranch( State head, Pattern pattern ) {
        
        State member = (State)pattern.apply(this);
        member = addAction(member,true);
        
        head.mergeTransitions(member);
        
        if(member.isAcceptable()) {
            // TODO: we need to copy exit actions from the member state
            // to the head state, but what if there already are some exit
            // actions?
            // this would happen for cases like
            // <choice>
            //   <group>
            //     <optional>...</optoinal>
            //     <cc:java> AAA </cc:java>
            //   </group>
            //   <group>
            //     <optional>...</optoinal>
            //     <cc:java> BBB </cc:java>
            //   </group>
            // </choice>
            //
            // this is a variation of ambiguity which we need to
            // detect.
            head.setAcceptable(true);
            head.addActionsOnExit(member.getActionsOnExit());
        }
    }
    

    public Object interleave( InterleavePattern pattern ) {
        
        State join = _destination;
        
        // add pending actions to a dummy transition so that
        // we can add these actions later as the epilogue action
        // of the join.
        Transition dummy = new Transition(null,null,0);
        addAction(dummy,false);
        
        Pattern[] children = pattern.getChildPatterns();
        State[] subAutomata = new State[children.length];
        NameClass[] elemNC = new NameClass[children.length];
        NameClass[] attNC = new NameClass[children.length];
        boolean[] text = new boolean[children.length];
        
        for( int i=children.length-1; i>=0; i-- ) {
            // create sub-automaton for each branch
            _destination = createState(pattern); 
            _destination.setAcceptable(true); // mark as the join state
            State member = (State)children[i].apply(this);
            member = addAction(member,true);
            
            subAutomata[i] = member;
            elemNC[i] = ElementNameCollector.collect(children[i]);
            attNC[i]  = AttributeNameCollector.collect(children[i]);
            text[i]   = TextCollector.collect(children[i]);
        }
        
        State head = createState(pattern);
        Transition forkTr = new Transition(
            new Alphabet.Fork(subAutomata,elemNC,attNC,text,
                null/*TODO:locator*/),
            join,
            _orderCounter++ );
        head.addTransition(forkTr);
        
        forkTr.insertEpilogueActions(dummy.getEpilogueActions());
        
        return head;
    }


    public Object oneOrMore(OneOrMorePattern pattern) {
        ScopeInfo.Action act = collectAction();

        State tail = _destination;
        if(act!=null) {
            tail = createState(_destination._locationHint);
            if(_destination.isAcceptable()) tail.setAcceptable(true);
            tail.addTransition(Transition.createActionOnlyTransition(_destination, act));
        }
        
        _destination = tail;
        State head = (State)pattern.p.apply(this);
        head = addAction(head,true); //addAction must be before mergeTransition
        
        tail.mergeTransitions(head);
        return head;
    }

    public Object ref( RefPattern pattern ) {
        
        State head = createState(pattern);
        
        ScopeInfo targetScope = _grammar.getScopeInfo(pattern.target);
        
        String alias = pattern.param.getAlias();
        if(alias!=null)
            _scopeInfo.addAlias(
                targetScope._scope.getParam().returnType, alias);
        
        Transition t = createTransition(new Alphabet.Ref(
            targetScope, alias,
            pattern.param.getWithParams(),
            pattern.locator),
            _destination);
        head.addTransition(t);

        // add action as epilogue because code should be executed
        // *after* the transition is performed.
        addAction(t,false);
        
        return head;
    }
    
    public Object scope( Scope scope ) {
        // we don't cross <ref> boundary, so this shouldn't be executed at all.
        throw new InternalError();
    }
    
    
    private State createState(Pattern source) {
        State s = new State(_scopeInfo, _scopeInfo.getStateCount(), source);
        _scopeInfo.addState(s);
        return s;
    }
    
    private Transition createTransition(Alphabet key, State destination) {
        Transition t = new Transition(key, destination, _orderCounter++);
        return t;
    }
    
    private void addAction(Transition t, boolean prologue) {
        ScopeInfo.Action action = collectAction();
        if(action!=null) {
            if(prologue)    t.insertPrologueAction(action);
            else            t.insertEpilogueAction(action);
        }
    }
    
    /**
     * Adds the specified action as a prologue/epilogue action
     * to all the transitions that leave the given state.
     * 
     * <p>
     * To avoid causing unexpected modification, a State object
     * will be copied and the new state will be returned
     * 
     * <p>
     * Consider a process of building (A|(B,cc:java)) where
     * A and B are elements and cc:java is an associated java action,
     * if we don't copy the state before adding actions to it, then
     * we end up creating the following automaton:
     * 
     * <pre><xmp>
     * s1 --- A ---> s2+action (final state)
     *  |             ^
     *  +---- B -----+
     * </xmp></pre>
     * 
     * which is incorrect, because we don't want cc:java to be executed
     * when we see A.
     * 
     * <p>
     * Copying a state will prevent this side-effect.
     */
    private State addAction(State s, boolean prologue) {
        
        ScopeInfo.Action act = collectAction();
        if(act==null) return s;
        
        State ss = createState(s._locationHint);
        ss.mergeTransitions(s);
        if(s.isAcceptable()) {
            ss.setAcceptable(true);
            ss.addActionsOnExit(s.getActionsOnExit());
        }
        
        Iterator it = ss.iterateTransitions();
        while(it.hasNext()) {
            Transition t = (Transition)it.next();
            if(prologue)    t.insertPrologueAction(act);
            else            t.insertEpilogueAction(act);
        }

        //probably actionOnExit is no longer necessary
        //ss.addActionOnExit(act);
        return ss;
    }
    
    /**
     * Builds an action object from accumulated unprocessed &lt;cc:java>.
     */
    private ScopeInfo.Action collectAction() {
        ScopeInfo.Action act = (_preservedAction.length()==0)? null : _scopeInfo.createAction(_preservedAction);
        _preservedAction = "";
        return act;
    }
}
