/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class LongWrapperHolder implements Holder {
    public Long value;
    
    public LongWrapperHolder() {
    }
    
    public LongWrapperHolder(Long mylong) {
        this.value = mylong;
    }
}
