package org.kohsuke.rngom.ast.builder;

import org.kohsuke.rngom.ast.om.Location;
import org.kohsuke.rngom.ast.om.ParsedElementAnnotation;
import org.kohsuke.rngom.ast.om.ParsedPattern;


public interface ElementAnnotationBuilder<
    P extends ParsedPattern,
    E extends ParsedElementAnnotation,
    L extends Location,
    A extends Annotations<E,L,CL>,
    CL extends CommentList<L>> extends Annotations<E,L,CL> {

    void addText(String value, L loc, CL comments) throws BuildException;
    E makeElementAnnotation() throws BuildException;
}
