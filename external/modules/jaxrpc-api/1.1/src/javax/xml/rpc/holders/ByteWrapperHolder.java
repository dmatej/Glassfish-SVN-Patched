/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class ByteWrapperHolder implements Holder {
    public Byte value;
    
    public ByteWrapperHolder() {
    }
    
    public ByteWrapperHolder(Byte mybyte) {
        this.value = mybyte;
    }
}
