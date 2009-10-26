/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class QNameHolder implements Holder {
    public javax.xml.namespace.QName value;
    
    public QNameHolder() {
    }
    
    public QNameHolder(javax.xml.namespace.QName myQName) {
        this.value = myQName;
    }
}
