package org.kohsuke.rngom.dump;

import org.kohsuke.rngom.ast.builder.Annotations;
import org.kohsuke.rngom.ast.builder.CommentList;
import org.kohsuke.rngom.ast.builder.DataPatternBuilder;
import org.kohsuke.rngom.ast.builder.Div;
import org.kohsuke.rngom.ast.builder.ElementAnnotationBuilder;
import org.kohsuke.rngom.ast.builder.Grammar;
import org.kohsuke.rngom.ast.builder.Include;
import org.kohsuke.rngom.ast.builder.NameClassBuilder;
import org.kohsuke.rngom.ast.om.Location;
import org.kohsuke.rngom.ast.om.ParsedElementAnnotation;
import org.kohsuke.rngom.ast.om.ParsedNameClass;
import org.kohsuke.rngom.ast.om.ParsedPattern;

/**
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class Factory {
    private int id=0;
    
    private class Ref implements ParsedPattern,Location,ParsedElementAnnotation,ParsedNameClass {
        private final int i = id++;
        private final String prefix;
        
        Ref( String prefix ) {
            this.prefix = prefix;
        }
        
        public String toString() {
            return prefix+i;
        }
    }

    public ParsedPattern createPattern() {
        return new Ref("p");
    }

    public Location createLocation() {
        return new Ref("loc");
    }

    public ParsedElementAnnotation createParsedElementAnnotation() {
        return new Ref("ea");
    }

    public ParsedNameClass createNameClass() {
        return new Ref("n");
    }

    public NameClassBuilder createNameClassBuilder(Printer p) {
        return new NameClassBuilderImpl(this,p);
    }
    
    public Annotations createAnnotations(Printer p) {
        return new AnnotationsImpl(this,p,id++);
    }

    public ElementAnnotationBuilder createElementAnnotationBuilder(Printer p) {
        return new ElementAnnotationBuilderImpl(this,p,id++);
    }

    public CommentList createCommentList(Printer p) {
        return new CommentListImpl(this,p,id++);
    }

    public DataPatternBuilder createDataPatternBuilder(Printer p) {
        return new DataPatternBuilderImpl(this,p,id++);
    }

    public Grammar createGrammar(Printer p) {
        return new GrammarImpl(this,p,id++);
    }

    public Div createDiv(Printer p) {
        return new GrammarImpl(this,p,id++);
    }

    public Include createInclude(Printer p) {
        return new GrammarImpl(this,p,id++);
    }
}
