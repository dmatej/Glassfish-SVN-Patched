package org.kohsuke.rngom.dump;

import org.kohsuke.rngom.ast.builder.Annotations;
import org.kohsuke.rngom.ast.builder.BuildException;
import org.kohsuke.rngom.ast.builder.CommentList;
import org.kohsuke.rngom.ast.builder.NameClassBuilder;
import org.kohsuke.rngom.ast.om.Location;
import org.kohsuke.rngom.ast.om.ParsedElementAnnotation;
import org.kohsuke.rngom.ast.om.ParsedNameClass;

/**
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class NameClassBuilderImpl implements NameClassBuilder {
    private final Factory factory;
    private final Printer printer;
    
    public NameClassBuilderImpl(Factory f,Printer p) {
        factory = f;
        printer = p;
    }

    public ParsedNameClass annotate(ParsedNameClass nc, Annotations anno) throws BuildException {
        printer.name("annotate").param(nc).param(anno);
        return printer.result(factory.createNameClass());
    }

    public ParsedNameClass annotateAfter(ParsedNameClass nc, ParsedElementAnnotation e) throws BuildException {
        printer.name("annotateAfter").param(nc).param(e);
        return printer.result(factory.createNameClass());
    }

    public ParsedNameClass commentAfter(ParsedNameClass nc, CommentList comments) throws BuildException {
        printer.name("commentAfter").param(nc).param(comments);
        return printer.result(factory.createNameClass());
    }

    public ParsedNameClass makeChoice(ParsedNameClass[] nameClasses, int nNameClasses, Location loc, Annotations anno) {
        printer.name("makeChoice");
        for( int i=0; i<nNameClasses; i++ )
            printer.param(nameClasses[i]);
        printer.param(loc).param(anno);
        return printer.result(factory.createNameClass());
    }

    public ParsedNameClass makeName(String ns, String localName, String prefix, Location loc, Annotations anno) {
        printer.name("makeName").param(ns).param(localName).param(prefix).param(loc).param(anno);
        return printer.result(factory.createNameClass());
    }

    public ParsedNameClass makeNsName(String ns, Location loc, Annotations anno) {
        printer.name("makeNsName").param(ns).param(loc).param(anno);
        return printer.result(factory.createNameClass());
    }

    public ParsedNameClass makeNsName(String ns, ParsedNameClass except, Location loc, Annotations anno) {
        printer.name("makeNsName").param(ns).param(except).param(loc).param(anno);
        return printer.result(factory.createNameClass());
    }

    public ParsedNameClass makeAnyName(Location loc, Annotations anno) {
        printer.name("makeAnyName").param(loc).param(anno);
        return printer.result(factory.createNameClass());
    }

    public ParsedNameClass makeAnyName(ParsedNameClass except, Location loc, Annotations anno) {
        printer.name("makeAnyName").param(except).param(loc).param(anno);
        return printer.result(factory.createNameClass());
    }

    public ParsedNameClass makeErrorNameClass() {
        printer.name("makeErrorNameClass");
        return printer.result(factory.createNameClass());
    }
}
