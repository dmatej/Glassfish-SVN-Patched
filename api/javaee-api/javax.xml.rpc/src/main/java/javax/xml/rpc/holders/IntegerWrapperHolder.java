/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class IntegerWrapperHolder implements Holder {
    public Integer value;
    
    public IntegerWrapperHolder() {
    }
    
    public IntegerWrapperHolder(Integer myint) {
        this.value = myint;
    }
}
