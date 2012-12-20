/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class DoubleHolder implements Holder {
    public double value;
    
    public DoubleHolder() {
    }
    
    public DoubleHolder(double mydouble) {
        this.value = mydouble;
    }
}
