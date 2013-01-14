/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class BooleanWrapperHolder implements Holder {
    public Boolean value;
    
    public BooleanWrapperHolder() {
    }
    
    public BooleanWrapperHolder(Boolean myboolean) {
        this.value = myboolean;
    }
}
