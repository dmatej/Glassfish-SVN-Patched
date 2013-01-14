/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class ShortHolder implements Holder {
    public short value;
    
    public ShortHolder() {
    }
    
    public ShortHolder(short myshort) {
        this.value = myshort;
    }
}
