/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class ByteHolder implements Holder {
    public byte value;
    
    public ByteHolder() {
    }
    
    public ByteHolder(byte mybyte) {
        this.value = mybyte;
    }
}
