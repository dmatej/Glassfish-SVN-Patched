/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class ShortWrapperHolder implements Holder {
    public Short value;
    
    public ShortWrapperHolder() {
    }
    
    public ShortWrapperHolder(Short myshort) {
        this.value = myshort;
    }
}
