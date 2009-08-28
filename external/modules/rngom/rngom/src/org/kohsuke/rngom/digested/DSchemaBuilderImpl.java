package org.kohsuke.rngom.digested;

import org.kohsuke.rngom.ast.builder.Annotations;
import org.kohsuke.rngom.ast.builder.BuildException;
import org.kohsuke.rngom.ast.builder.CommentList;
import org.kohsuke.rngom.ast.builder.DataPatternBuilder;
import org.kohsuke.rngom.ast.builder.ElementAnnotationBuilder;
import org.kohsuke.rngom.ast.builder.Grammar;
import org.kohsuke.rngom.ast.builder.NameClassBuilder;
import org.kohsuke.rngom.ast.builder.SchemaBuilder;
import org.kohsuke.rngom.ast.builder.Scope;
import org.kohsuke.rngom.ast.om.Location;
import org.kohsuke.rngom.ast.om.ParsedElementAnnotation;
import org.kohsuke.rngom.ast.om.ParsedNameClass;
import org.kohsuke.rngom.ast.om.ParsedPattern;
import org.kohsuke.rngom.ast.util.LocatorImpl;
import org.kohsuke.rngom.nc.NameClass;
import org.kohsuke.rngom.nc.NameClassBuilderImpl;
import org.kohsuke.rngom.parse.Context;
import org.kohsuke.rngom.parse.IllegalSchemaException;
import org.kohsuke.rngom.parse.Parseable;
import org.xml.sax.Locator;

import java.util.List;

/**
 * Parses as {@link Parseable} into a {@link DPattern}.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class DSchemaBuilderImpl implements SchemaBuilder {

    private final NameClassBuilder ncb = new NameClassBuilderImpl();

    public NameClassBuilder getNameClassBuilder() throws BuildException {
        return ncb;
    }

    static  DPattern wrap( DPattern p, Location loc, Annotations anno ) {
        p.location = (Locator)loc;
        if(anno!=null)
            p.annotation = ((Annotation)anno).getResult();
        return p;
    }

    static DContainerPattern addAll( DContainerPattern parent, List children) {
        for( int i=0; i<children.size(); i++ )
            parent.add( (DPattern)children.get(i) );
        return parent;
    }

    static  DUnaryPattern addBody( DUnaryPattern parent, ParsedPattern _body, Location loc ) {
        parent.setChild( (DPattern)_body );
        return parent;
    }

    public ParsedPattern makeChoice(List patterns, Location loc, Annotations anno) throws BuildException {
        return wrap(addAll(new DChoicePattern(),patterns),loc,anno);
    }

    public ParsedPattern makeInterleave(List patterns, Location loc, Annotations anno) throws BuildException {
        return wrap(addAll(new DInterleavePattern(),patterns),loc,anno);
    }

    public ParsedPattern makeGroup(List patterns, Location loc, Annotations anno) throws BuildException {
        return wrap(addAll(new DGroupPattern(),patterns),loc,anno);
    }

    public ParsedPattern makeOneOrMore(ParsedPattern p, Location loc, Annotations anno) throws BuildException {
        return wrap(addBody(new DOneOrMorePattern(),p,loc),loc,anno);
    }

    public ParsedPattern makeZeroOrMore(ParsedPattern p, Location loc, Annotations anno) throws BuildException {
        return wrap(addBody(new DZeroOrMorePattern(),p,loc),loc,anno);
    }

    public ParsedPattern makeOptional(ParsedPattern p, Location loc, Annotations anno) throws BuildException {
        return wrap(addBody(new DOptionalPattern(),p,loc),loc,anno);
    }

    public ParsedPattern makeList(ParsedPattern p, Location loc, Annotations anno) throws BuildException {
        return wrap(addBody(new DListPattern(),p,loc),loc,anno);
    }

    public ParsedPattern makeMixed(ParsedPattern p, Location loc, Annotations anno) throws BuildException {
        return wrap(addBody(new DMixedPattern(),p,loc),loc,anno);
    }

    public ParsedPattern makeEmpty(Location loc, Annotations anno) {
        return wrap(new DEmptyPattern(),loc,anno);
    }

    public ParsedPattern makeNotAllowed(Location loc, Annotations anno) {
        return wrap(new DNotAllowedPattern(),loc,anno);
    }

    public ParsedPattern makeText(Location loc, Annotations anno) {
        return wrap(new DTextPattern(),loc,anno);
    }

    public ParsedPattern makeAttribute(ParsedNameClass nc, ParsedPattern p, Location loc, Annotations anno) throws BuildException {
        return wrap(addBody(new DAttributePattern((NameClass)nc),p,loc),loc,anno);
    }

    public ParsedPattern makeElement(ParsedNameClass nc, ParsedPattern p, Location loc, Annotations anno) throws BuildException {
        return wrap(addBody(new DElementPattern((NameClass)nc),p,loc),loc,anno);
    }

    public DataPatternBuilder makeDataPatternBuilder(String datatypeLibrary, String type, Location loc) throws BuildException {
        return new DataPatternBuilderImpl(datatypeLibrary,type,loc);
    }

    public ParsedPattern makeValue(String datatypeLibrary, String type, String value, Context c, String ns, Location loc, Annotations anno) throws BuildException {
        return wrap(new DValuePattern(datatypeLibrary,type,value,c.copy(),ns),loc,anno);
    }

    public Grammar makeGrammar(Scope parent) {
        return new GrammarBuilderImpl(new DGrammarPattern(),parent,this);
    }

    public ParsedPattern annotate(ParsedPattern p, Annotations anno) throws BuildException {
        // TODO: not sure when this is used
        return p;
    }

    public ParsedPattern annotateAfter(ParsedPattern p, ParsedElementAnnotation e) throws BuildException {
        // TODO
        return p;
    }

    public ParsedPattern commentAfter(ParsedPattern p, CommentList comments) throws BuildException {
        // TODO
        return p;
    }

    public ParsedPattern makeExternalRef(Parseable current, String uri, String ns, Scope scope, Location loc, Annotations anno) throws BuildException, IllegalSchemaException {
        // TODO
        return null;
    }

    public Location makeLocation(String systemId, int lineNumber, int columnNumber) {
        return new LocatorImpl(systemId,lineNumber,columnNumber);
    }

    public Annotations makeAnnotations(CommentList comments, Context context) {
        return new Annotation();
    }

    public ElementAnnotationBuilder makeElementAnnotationBuilder(String ns, String localName, String prefix, Location loc, CommentList comments, Context context) {
        return new ElementAnnotationBuilderImpl();
    }

    public CommentList makeCommentList() {
        return null;
    }

    public ParsedPattern makeErrorPattern() {
        return new DNotAllowedPattern();
    }

    public boolean usesComments() {
        return false;
    }

    public ParsedPattern expandPattern(ParsedPattern p) throws BuildException, IllegalSchemaException {
        return p;
    }
}
