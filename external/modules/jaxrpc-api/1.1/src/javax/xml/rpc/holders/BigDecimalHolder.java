/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class BigDecimalHolder implements Holder {
    public java.math.BigDecimal value;
    
    public BigDecimalHolder() {
    }
    
    public BigDecimalHolder(java.math.BigDecimal myBigDecimal) {
        this.value = myBigDecimal;
    }
}
