/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class FloatWrapperHolder implements Holder {
    public Float value;
    
    public FloatWrapperHolder() {
    }
    
    public FloatWrapperHolder(Float myfloat) {
        this.value = myfloat;
    }
}
