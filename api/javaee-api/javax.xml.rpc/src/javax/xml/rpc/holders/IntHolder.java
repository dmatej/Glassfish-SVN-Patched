/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class IntHolder implements Holder {
    public int value;
    
    public IntHolder() {
    }
    
    public IntHolder(int myint) {
        this.value = myint;
    }
}
