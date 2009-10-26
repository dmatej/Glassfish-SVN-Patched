/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class StringHolder implements Holder {
    public java.lang.String value;
    
    public StringHolder() {
    }
    
    public StringHolder(java.lang.String myString) {
        this.value = myString;
    }
}
