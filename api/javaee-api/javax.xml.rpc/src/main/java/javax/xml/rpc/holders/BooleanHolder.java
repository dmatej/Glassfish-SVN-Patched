/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class BooleanHolder implements Holder {
    public boolean value;
    
    public BooleanHolder() {
    }
    
    public BooleanHolder(boolean myboolean) {
        this.value = myboolean;
    }
}
