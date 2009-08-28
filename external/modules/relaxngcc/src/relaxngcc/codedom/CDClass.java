package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;

/**
 */
public class CDClass extends CDType {

    private CDLanguageSpecificString[] _precedingDeclarations;
    private CDLanguageSpecificString _preModifier;
    private CDLanguageSpecificString _postModifier;
    private String _className;
    
    private final ArrayList _members = new ArrayList();
    private final ArrayList _methods = new ArrayList();
    
    private final ArrayList _additionalBody = new ArrayList();
    
    private final ArrayList _innerClasses = new ArrayList();

    public CDClass( String className ) {
        this(null,null,className,null);
    }

    public CDClass(
        CDLanguageSpecificString[] declarations, CDLanguageSpecificString fs,
        String name, CDLanguageSpecificString bs) {
        
        super(name);
        _precedingDeclarations = declarations;
        _preModifier = fs;
        _className = name;
        _postModifier = bs;
    }
    

    /** Adds a new member declaration. */
    public CDVariable addMember(
        CDLanguageSpecificString modifier,
        CDType type, String name, CDExpression initialValue) {
            
        CDVariable var = new CDVariable(modifier,type,name,initialValue);
        _members.add(var);
        return var;
    }

    public CDVariable addMember(
        CDLanguageSpecificString modifier, CDType type, String name) {
        
        return addMember(modifier,type,name,null);
    }
    
    public void addMethod(CDMethod methoddef) {
        _methods.add(methoddef);
    }
    public void addLanguageSpecificString(CDLanguageSpecificString content) {
        _additionalBody.add(content);
    }
    
    /**
     * Adds a new inner class.
     */
    public void addInnerClass( CDClass innerClass ) {
        _innerClasses.add(innerClass);
    }

    public void writeType( CDFormatter f ) throws IOException {
        f.p(_className);
    }

    public void writeTo( CDFormatter f ) throws IOException {

        if(_precedingDeclarations!=null) {
            for(int i=0; i<_precedingDeclarations.length; i++)
                f.write(_precedingDeclarations[i]).nl();
        }
        if(_preModifier!=null)
            f.write(_preModifier);
        
        f.p("class").p(_className);

        if(_postModifier!=null)
            f.write(_postModifier);
        
        f.p('{');
        f.in();
        f.nl();
        
        for(int i=0; i<_members.size(); i++)
            ((CDVariable)_members.get(i)).state(f);
            
        f.nl();
        
        for(int i=0; i<_methods.size(); i++)
            ((CDMethod)_methods.get(i)).writeTo(f);
        
        for(int i=0; i<_additionalBody.size(); i++)
            f.write((CDLanguageSpecificString)_additionalBody.get(i));
        
        for(int i=0; i<_innerClasses.size(); i++) {
            f.nl();
            ((CDClass)_innerClasses.get(i)).writeTo(f);
            f.nl();
        }
        
        f.out();
        f.nl().p('}').nl().nl();
    }

}
