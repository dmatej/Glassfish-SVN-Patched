/* 
 * @(#)file      UnsignedInt.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.5
 * @(#)lastedit  07/03/08
 * 
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright (c) 2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU General
 * Public License Version 2 only ("GPL") or the Common Development and
 * Distribution License("CDDL")(collectively, the "License"). You may not use
 * this file except in compliance with the License. You can obtain a copy of the
 * License at http://opendmk.dev.java.net/legal_notices/licenses.txt or in the 
 * LEGAL_NOTICES folder that accompanied this code. See the License for the 
 * specific language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file found at
 *     http://opendmk.dev.java.net/legal_notices/licenses.txt
 * or in the LEGAL_NOTICES folder that accompanied this code.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.
 * 
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * 
 *       "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding
 * 
 *       "[Contributor] elects to include this software in this distribution
 *        under the [CDDL or GPL Version 2] license."
 * 
 * If you don't indicate a single choice of license, a recipient has the option
 * to distribute your version of this file under either the CDDL or the GPL
 * Version 2, or to extend the choice of license to its licensees as provided
 * above. However, if you add GPL Version 2 code and therefore, elected the
 * GPL Version 2 license, then the option applies only if the new code is made
 * subject to such option by the copyright holder.
 * 
 */ 



package com.sun.jdmk;

/** <p>An unsigned integer, between 0 and 2<sup>32</sup> - 1.  Objects of
    this class and its subclasses are immutable.</p>  
    @since Java DMK 5.1
*/
class UnsignedInt extends UnsignedLong {
    private static final long serialVersionUID = 1079398472598502169L;
    /** The largest representable <code>UnsignedLong</code> value,
	equal to 2<sup>32</sup> - 1.  */
    public static final UnsignedInt MAX_VALUE = new UnsignedInt(-1);

    UnsignedInt(int x) {
	super(x & 0xffffffffL);
    }

    /** Make an <code>UnsignedInt</code> object representing a value
	between 0 and 2<sup>32</sup> that is equal mod
	2<sup>32</sup> to <code>x</code>.  The actual type of
	the returned value will be the most specific possible of
	<code>UnsignedByte</code>, <code>UnsignedShort</code>, or
	<code>UnsignedInt</code>.  */
    public static UnsignedInt make(int x) {
	return (UnsignedInt) UnsignedLong.make(x & 0xffffffffL);
    }

    /** Same as <code>make</code> except that if the resultant object would
	be equal to <code>this</code>, <code>this</code> is returned.  This
	allows us to avoid fabricating an object to represent, e.g., the
	result of adding 0 to <code>this</code>.  */
    UnsignedInt makeIfDifferent(int x) {
	if ((int) this.value == x)
	    return this;
	return make(x);
    }

    /** Return an <code>UnsignedInt</code> that is equal mod
        2<sup>32</sup> to <code>this + x</code>.  The actual type of
        the returned value will be the most specific possible of
        <code>UnsignedByte</code>, <code>UnsignedShort</code>, or
        <code>UnsignedInt</code>.  */
    public UnsignedInt add(UnsignedInt x) {
	return makeIfDifferent((int) (this.value + x.value));
    }

    /** Return an <code>UnsignedInt</code> that is equal mod
        2<sup>32</sup> to <code>this - x</code>.  The actual type of
        the returned value will be the most specific possible of
        <code>UnsignedByte</code>, <code>UnsignedShort</code>, or
        <code>UnsignedInt</code>.  */
    public UnsignedInt subtract(UnsignedInt x) {
	return makeIfDifferent((int) (this.value - x.value));
    }

    /** Return an <code>UnsignedInt</code> that is equal mod
        2<sup>32</sup> to <code>this * x</code>.  The actual type of
        the returned value will be the most specific possible of
        <code>UnsignedByte</code>, <code>UnsignedShort</code>, or
        <code>UnsignedInt</code>.  */
    public UnsignedInt multiply(UnsignedInt x) {
	return makeIfDifferent((int) (this.value * x.value));
    }

    /** Return an <code>UnsignedInt</code> that is equal to
        <code>[this / x]</code>, where <code>[x]</code> is the floor
        function.  The actual type of the returned value will be the
        most specific possible of <code>UnsignedByte</code>,
        <code>UnsignedShort</code>, or <code>UnsignedInt</code>.
    
	@exception ArithmeticException if <code>x</code> is zero.  */
    public UnsignedInt divide(UnsignedInt x) throws ArithmeticException {
	return makeIfDifferent((int) (this.value / x.value));
    }

    /** Return an <code>UnsignedInt</code> that is equal to
	<code>(this mod x)</code>.  The actual type of the returned
	value will be the most specific possible of
	<code>UnsignedByte</code>, <code>UnsignedShort</code>, or
	<code>UnsignedInt</code>.
    
	@exception ArithmeticException if <code>x</code> is zero.  */
    public UnsignedInt mod(UnsignedInt x) throws ArithmeticException {
	return makeIfDifferent((int) (this.value % x.value));
    }

    /* We can't override shiftLeft or shiftRight since they still have
       an int parameter.  */

    /** Return an <code>UnsignedInt</code> that is the bitwise
	<code>and</code> of <code>this</code> and <code>x</code>.
	The actual type of the returned value will be the most
	specific possible of <code>UnsignedByte</code>,
	<code>UnsignedShort</code>, or <code>UnsignedInt</code>.  */
    public UnsignedInt and(UnsignedInt x) {
	return makeIfDifferent((int) (this.value & x.value));
    }

    /** Return an <code>UnsignedInt</code> that is the bitwise
	<code>or</code> of <code>this</code> and <code>x</code>.
	The actual type of the returned value will be the most
	specific possible of <code>UnsignedByte</code>,
	<code>UnsignedShort</code>, or <code>UnsignedInt</code>.  */
    public UnsignedInt or(UnsignedInt x) {
	return makeIfDifferent((int) (this.value | x.value));
    }

    /** Return an <code>UnsignedInt</code> that is the bitwise
	<code>xor</code> of <code>this</code> and <code>x</code>.
	The actual type of the returned value will be the most
	specific possible of <code>UnsignedByte</code>,
	<code>UnsignedShort</code>, or <code>UnsignedInt</code>.  */
    public UnsignedInt xor(UnsignedInt x) {
	return makeIfDifferent((int) (this.value ^ x.value));
    }

    /* We can't override not, testBit, setBit, or clearBit.  */

    /** Return the smaller of <code>this</code> and <code>x</code>.  */
    public UnsignedInt min(UnsignedInt x) {
	if (unsignedCompare(this.value, x.value) < 0)
	    return this;
	else
	    return x;
    }

    /** Return the larger of <code>this</code> and <code>x</code>.  */
    public UnsignedInt max(UnsignedInt x) {
	if (unsignedCompare(this.value, x.value) > 0)
	    return this;
	else
	    return x;
    }

}
