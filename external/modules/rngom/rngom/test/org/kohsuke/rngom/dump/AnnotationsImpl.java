package org.kohsuke.rngom.dump;

import org.kohsuke.rngom.ast.builder.Annotations;
import org.kohsuke.rngom.ast.builder.BuildException;
import org.kohsuke.rngom.ast.builder.CommentList;
import org.kohsuke.rngom.ast.om.Location;
import org.kohsuke.rngom.ast.om.ParsedElementAnnotation;

/**
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class AnnotationsImpl extends Base implements Annotations {
    public AnnotationsImpl( Factory f, Printer p, int id ) {
        super(f,p,id);
    }

    public void addAttribute(String ns, String localName, String prefix, String value, Location loc) throws BuildException {
        out("addAttribute").param(ns).param(localName).param(prefix).param(value).param(loc).result();
    }

    public void addElement(ParsedElementAnnotation ea) throws BuildException {
        out("addElement").param(ea).result();
    }

    public void addComment(CommentList comments) throws BuildException {
        out("addComment").param(comments).result();
    }

    public void addLeadingComment(CommentList comments) throws BuildException {
        out("addLeadingComment").param(comments).result();
    }
    
    public String prefix() {
        return "a";
    }
}
