/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

package org.jvnet.staxex;

/**
 * @author Kohsuke Kawaguchi
 */
class Base64Encoder {
    private static final char[] encodeMap = initEncodeMap();

    private static char[] initEncodeMap() {
        char[] map = new char[64];
        int i;
        for( i= 0; i<26; i++ )        map[i] = (char)('A'+i);
        for( i=26; i<52; i++ )        map[i] = (char)('a'+(i-26));
        for( i=52; i<62; i++ )        map[i] = (char)('0'+(i-52));
        map[62] = '+';
        map[63] = '/';

        return map;
    }

    public static char encode( int i ) {
        return encodeMap[i&0x3F];
    }

    public static byte encodeByte( int i ) {
        return (byte)encodeMap[i&0x3F];
    }

    public static String print(byte[] input, int offset, int len) {
        char[] buf = new char[((len+2)/3)*4];
        int ptr = print(input,offset,len,buf,0);
        assert ptr==buf.length;
        return new String(buf);
    }

    /**
     * Encodes a byte array into a char array by doing base64 encoding.
     *
     * The caller must supply a big enough buffer.
     *
     * @return
     *      the value of {@code ptr+((len+2)/3)*4}, which is the new offset
     *      in the output buffer where the further bytes should be placed.
     */
    public static int print(byte[] input, int offset, int len, char[] buf, int ptr) {
        for( int i=offset; i<len; i+=3 ) {
            switch( len-i ) {
            case 1:
                buf[ptr++] = encode(input[i]>>2);
                buf[ptr++] = encode(((input[i])&0x3)<<4);
                buf[ptr++] = '=';
                buf[ptr++] = '=';
                break;
            case 2:
                buf[ptr++] = encode(input[i]>>2);
                buf[ptr++] = encode(
                            ((input[i]&0x3)<<4) |
                            ((input[i+1]>>4)&0xF));
                buf[ptr++] = encode((input[i+1]&0xF)<<2);
                buf[ptr++] = '=';
                break;
            default:
                buf[ptr++] = encode(input[i]>>2);
                buf[ptr++] = encode(
                            ((input[i]&0x3)<<4) |
                            ((input[i+1]>>4)&0xF));
                buf[ptr++] = encode(
                            ((input[i+1]&0xF)<<2)|
                            ((input[i+2]>>6)&0x3));
                buf[ptr++] = encode(input[i+2]&0x3F);
                break;
            }
        }
        return ptr;
    }
}
