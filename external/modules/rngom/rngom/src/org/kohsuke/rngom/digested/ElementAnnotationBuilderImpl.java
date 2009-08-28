package org.kohsuke.rngom.digested;

import org.kohsuke.rngom.ast.builder.BuildException;
import org.kohsuke.rngom.ast.builder.CommentList;
import org.kohsuke.rngom.ast.builder.ElementAnnotationBuilder;
import org.kohsuke.rngom.ast.om.Location;
import org.kohsuke.rngom.ast.om.ParsedElementAnnotation;

/**
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
class ElementAnnotationBuilderImpl implements ElementAnnotationBuilder {
    public void addText(String value, Location loc, CommentList comments) throws BuildException {
        // TODO
    }

    public ParsedElementAnnotation makeElementAnnotation() throws BuildException {
        // TODO
        return null;
    }

    public void addAttribute(String ns, String localName, String prefix, String value, Location loc) throws BuildException {
        // TODO

    }

    public void addElement(ParsedElementAnnotation ea) throws BuildException {
        // TODO

    }

    public void addComment(CommentList comments) throws BuildException {
        // TODO

    }

    public void addLeadingComment(CommentList comments) throws BuildException {
        // TODO

    }

}
