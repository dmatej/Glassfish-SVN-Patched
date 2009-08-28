package org.kohsuke.rngom.dump;

import org.kohsuke.rngom.ast.builder.BuildException;
import org.kohsuke.rngom.ast.builder.CommentList;
import org.kohsuke.rngom.ast.builder.ElementAnnotationBuilder;
import org.kohsuke.rngom.ast.om.Location;
import org.kohsuke.rngom.ast.om.ParsedElementAnnotation;

/**
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class ElementAnnotationBuilderImpl extends AnnotationsImpl implements ElementAnnotationBuilder {

    public ElementAnnotationBuilderImpl(Factory f, Printer p, int id) {
        super(f, p, id);
    }

    public void addText(String value, Location loc, CommentList comments) throws BuildException {
        out("addText").param(value).param(loc).param(comments);
        
    }

    public ParsedElementAnnotation makeElementAnnotation() throws BuildException {
        out("makeElementAnnotation");
        return printer.result(factory.createParsedElementAnnotation());
    }
}
