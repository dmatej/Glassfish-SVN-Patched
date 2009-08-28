package org.kohsuke.rngom.dump;

import org.kohsuke.rngom.ast.builder.Annotations;
import org.kohsuke.rngom.ast.builder.BuildException;
import org.kohsuke.rngom.ast.builder.DataPatternBuilder;
import org.kohsuke.rngom.ast.om.Location;
import org.kohsuke.rngom.ast.om.ParsedElementAnnotation;
import org.kohsuke.rngom.ast.om.ParsedPattern;
import org.kohsuke.rngom.parse.Context;

/**
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class DataPatternBuilderImpl extends Base implements DataPatternBuilder {
    public DataPatternBuilderImpl(Factory f, Printer p, int id) {
        super(f, p, id);
    }

    protected String prefix() {
        return "dtb";
    }

    public void addParam(String name, String value, Context context, String ns, Location loc, Annotations anno) throws BuildException {
        out("addParam").param(name).param(value).param(ns).param(loc).param(anno).result();
    }

    public void annotation(ParsedElementAnnotation ea) {
        out("annotation").param(ea).result();
    }

    public ParsedPattern makePattern(Location loc, Annotations anno) throws BuildException {
        out("makePattern").param(loc).param(anno);
        return printer.result(factory.createPattern());
    }

    public ParsedPattern makePattern(ParsedPattern except, Location loc, Annotations anno) throws BuildException {
        out("makePattern").param(except).param(loc).param(anno);
        return printer.result(factory.createPattern());
    }
}
