/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class BigIntegerHolder implements Holder {
    public java.math.BigInteger value;
    
    public BigIntegerHolder() {
    }
    
    public BigIntegerHolder(java.math.BigInteger myBigInteger) {
        this.value = myBigInteger;
    }
}
