/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class DoubleWrapperHolder implements Holder {
    public Double value;
    
    public DoubleWrapperHolder() {
    }
    
    public DoubleWrapperHolder(Double mydouble) {
        this.value = mydouble;
    }
}
