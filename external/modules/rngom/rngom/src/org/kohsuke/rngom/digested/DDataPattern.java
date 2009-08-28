package org.kohsuke.rngom.digested;

import org.kohsuke.rngom.ast.om.Location;
import org.kohsuke.rngom.parse.Context;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class DDataPattern extends DPattern {
    DPattern except;

    String datatypeLibrary;
    String type;

    List params = new ArrayList();

    /**
     * Parameter to a data pattern.
     */
    public class Param {
        String name;
        String value;
        Context context;
        String ns;
        Location loc;
        Annotation anno;

        public Param(String name, String value, Context context, String ns, Location loc, Annotation anno) {
            this.name = name;
            this.value = value;
            this.context = context;
            this.ns = ns;
            this.loc = loc;
            this.anno = anno;
        }

        public String getName() {
            return name;
        }

        public String getValue() {
            return value;
        }

        public Context getContext() {
            return context;
        }

        public String getNs() {
            return ns;
        }

        public Location getLoc() {
            return loc;
        }

        public Annotation getAnno() {
            return anno;
        }
    }

    public String getDatatypeLibrary() {
        return datatypeLibrary;
    }

    public String getType() {
        return type;
    }

    public List getParams() {
        return params;
    }

    public DPattern getExcept() {
        return except;
    }

    public boolean isNullable() {
        return false;
    }

    public Object accept( DPatternVisitor visitor ) {
        return visitor.onData(this);
    }
}
