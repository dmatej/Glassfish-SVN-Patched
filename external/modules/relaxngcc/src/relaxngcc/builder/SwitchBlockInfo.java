package relaxngcc.builder;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;

import relaxngcc.automaton.State;
import relaxngcc.codedom.*;

/**
 * Generates code in the following format:
 * 
 * <pre>
 * switch(state) {
 * case state #1:
 *     === prologue code ===
 *     
 *     if( conditional #1 ) {
 *         statement #1;
 *     } else
 *     if( conditional #2 ) {
 *         statement #2;
 *     } else {
 *     if ...
 *  
 *     } else {
 *         === else code ===
 *     }
 *     
 *     === epilogue code ===
 *     break;
 * case state #n:
 *     ...
 *     break;
 * }
 * </pre>
 */
class SwitchBlockInfo
{

    //utility classes: for switch-case-if structure of each handler
    private class CodeAboutState
    {
        public CDBlock prologue;
        public CDBlock epilogue;
        public CDIfStatement conditionalCodes,top;
        public CDBlock elsecode;
        
        public void addConditionalCode(CDExpression cond, CDBlock code) {
            if(conditionalCodes==null)
                conditionalCodes = top = new CDIfStatement(cond);
            else
                // add another "if" statement.
                top = top._else()._if(cond);
            
            top.setThenBlock(code);
        }
        
        public CDBlock output(CDStatement errorHandleMethod) {
            CDBlock sv = new CDBlock();
            
            if(prologue!=null) sv.add(prologue);
            
            //elsecode, null‚È‚çerrorHandleMethod‚Å•Â‚¶‚é
            
            CDBlock terminal = elsecode;
            if(terminal==null && errorHandleMethod!=null)
                terminal = new CDBlock(errorHandleMethod);
    
            if(conditionalCodes!=null) {
                if(terminal!=null)
                        top.setElseBlock(terminal);
                sv.add(conditionalCodes);
            } else {
                if(terminal!=null)
                    sv.add(terminal);
            }
            
            if(epilogue!=null)
                sv.add(epilogue);
                
            return sv;
        }
    }

    public Map state2CodeFragment = new HashMap();
    
    private int _type; //one of the constants in Alphabet class
    public int getType() { return _type; }
    public SwitchBlockInfo(int type) {
        _type = type;
    }
    
    private CodeAboutState getCAS( State state ) {
        CodeAboutState cas = (CodeAboutState)state2CodeFragment.get(state);
        if(cas==null) {
            cas = new CodeAboutState();
            state2CodeFragment.put(state, cas);
        }
        return cas;
    }
    
    //if "cond" is "", "code" is put with no if-else clause. this behavior is not smart...
    public void addConditionalCode(State state, CDExpression cond, CDBlock code) {
        getCAS(state).addConditionalCode(cond,code);
    }
    
    public void addElseCode(State state, CDBlock code) {
        CodeAboutState cas = getCAS(state);
        
        if(cas.elsecode==null)
            cas.elsecode = code;
        else
            cas.elsecode.add(code);
    }
    
    public void addPrologue(State state, CDStatement code) {
        CodeAboutState cas = getCAS(state);
        if(cas.prologue==null)
            cas.prologue = new CDBlock(code);
        else
            cas.prologue.add(code);
    }
    
    public void addEpilogue(State state, CDStatement code) {
        CodeAboutState cas = getCAS(state);
        if(cas.epilogue==null)
            cas.epilogue = new CDBlock(code);
        else
            cas.epilogue.add(code);
    }

    CDBlock output(CDVariable $state, CDStatement errorHandleMethod) {
        CDBlock sv = new CDBlock();
        CDSwitchStatement switchBlock = null;
        Iterator i = state2CodeFragment.entrySet().iterator();
        while(i.hasNext())
        {
            Map.Entry e = (Map.Entry)i.next();
            State st = (State)e.getKey();
            
            CDExpression condition = CDOp.EQ($state,
                 new CDConstant(st.getIndex()));
                
            CDBlock whentrue = ((CodeAboutState)e.getValue()).output(errorHandleMethod);
            
            if(switchBlock==null)
                switchBlock = new CDSwitchStatement($state);
            
            switchBlock.addCase(new CDConstant(st.getIndex()), whentrue );
        }
        
        if(switchBlock!=null) {
            if(errorHandleMethod!=null)
                switchBlock.defaultCase().add(errorHandleMethod);
            sv.add(switchBlock);
        }
        return sv;
    }
}
