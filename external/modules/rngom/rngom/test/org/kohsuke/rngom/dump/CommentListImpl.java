package org.kohsuke.rngom.dump;

import org.kohsuke.rngom.ast.builder.BuildException;
import org.kohsuke.rngom.ast.builder.CommentList;
import org.kohsuke.rngom.ast.om.Location;

/**
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class CommentListImpl extends Base implements CommentList {
    public CommentListImpl(Factory f, Printer p, int id) {
        super(f,p,id);
    }

    public void addComment(String value, Location loc) throws BuildException {
        out("addComment").param(value).param(loc).result();
    }

    protected String prefix() {
        return "c";
    }
}
