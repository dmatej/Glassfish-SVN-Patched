/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class ObjectHolder implements Holder {
    public java.lang.Object value;
    
    public ObjectHolder() {
    }
    
    public ObjectHolder(java.lang.Object value) {
        this.value = value;
    }
}
