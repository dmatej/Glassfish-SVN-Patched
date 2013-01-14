/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class ByteArrayHolder implements Holder {
    public byte[] value;
    
    public ByteArrayHolder() {
    }
    
    public ByteArrayHolder(byte[] mybyteArray) {
        this.value = mybyteArray;
    }
}
