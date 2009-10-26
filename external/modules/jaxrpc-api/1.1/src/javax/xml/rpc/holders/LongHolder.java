/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class LongHolder implements Holder {
    public long value;
    
    public LongHolder() {
    }
    
    public LongHolder(long mylong) {
        this.value = mylong;
    }
}
