/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class FloatHolder implements Holder {
    public float value;
    
    public FloatHolder() {
    }
    
    public FloatHolder(float myfloat) {
        this.value = myfloat;
    }
}
