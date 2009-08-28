package org.kohsuke.rngom.nc;

import javax.xml.namespace.QName;

public interface NameClassVisitor<V> {
    V visitChoice(NameClass nc1, NameClass nc2);
    V visitNsName(String ns);
    V visitNsNameExcept(String ns, NameClass nc);
    V visitAnyName();
    V visitAnyNameExcept(NameClass nc);
    V visitName(QName name);
    V visitNull();
    V visitError();
}
