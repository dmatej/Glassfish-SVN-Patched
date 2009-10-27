/* 
 * @(#)file      UnsignedLong.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.8
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

import java.lang.Number;
import java.lang.Comparable;

/** <p>An unsigned integer, between 0 and 2<sup>64</sup> - 1.  Objects of
    this class and its subclasses are immutable.</p>  
    @since Java DMK 5.1
*/
public class UnsignedLong extends Number implements Comparable {
    private static final long serialVersionUID = 862366467859516825L;
    /** The largest representable <code>UnsignedLong</code> value,
	equal to 2<sup>64</sup> - 1.  */
    public static final UnsignedLong MAX_VALUE = new UnsignedLong(-1L);

    /** The value 0.  */
    static final UnsignedByte ZERO = new UnsignedByte((byte) 0);

    /** The value 1.  */
    static final UnsignedByte ONE = new UnsignedByte((byte) 1);

    /** The value 2.  */
    static final UnsignedByte TWO = new UnsignedByte((byte) 2);

    /** A long whose 64 bits are the same as the 64 bits of the
	unsigned value represented by this object.  If X is the number
	represented by this object and x is <code>value</code>, then X
	= x + (s * 2<sup>64</sup>), where s is 0 if x &gt;= 0 or 1 if
	x &lt; 0.  */
    long value;

    /** An unsigned value between 0 and 2<sup>64</sup> that is equal
        mod 2<sup>64</sup> to <code>value</code>.  
	@deprecated Use <CODE> make(long) </CODE> instead.
    */
    public UnsignedLong(long value) {
	this.value = value;
    }

    /** Make an <code>UnsignedLong</code> object representing a value
	between 0 and 2<sup>64</sup> that is equal mod
	2<sup>64</sup> to <code>x</code>.*/
    public static UnsignedLong make(long x) {
	if (x >= 0) {
	    if (x < 0x100) {
		if (x == 0)
		    return ZERO;
		else if (x == 1)
		    return ONE;
		else
		    return new UnsignedByte((byte) x);
	    }
	    if (x < 0x10000)
		return new UnsignedShort((short) x);
	    if (x < 0x100000000L)
		return new UnsignedInt((int) x);
	}
	return new UnsignedLong(x);
    }

    /** Same as <code>make</code> except that if the resultant object would
	be equal to <code>this</code>, <code>this</code> is returned.  This
	allows us to avoid fabricating an object to represent, e.g., the
	result of adding 0 to <code>this</code>.  */
    UnsignedLong makeIfDifferent(long x) {
	if (this.value == x)
	    return this;
	return make(x);
    }

    /** Return an <code>int</code> that is equal mod 2<sup>32</sup> to
	this unsigned number.  */
    public int intValue() {
	return (int) value;
    }

    /** Return a <code>long</code> that is equal mod 2<sup>64</sup> to
	this unsigned number.  */
    public long longValue() {
	return value;
    }

    /** Return the representable <code>double</code> that is nearest
        to this unsigned number.  */
    public double doubleValue() {
	if (value >= 0)
	    return (double) value;
	else
	    return (double) value - 2.0 * Long.MIN_VALUE;
    }

    /** Return the representable <code>float</code> that is nearest
        to this unsigned number.  */
    public float floatValue() {
	if (value >= 0)
	    return (float) value;
	else
	    return (float) value - 2.0F * Long.MIN_VALUE;
    }

    public int compareTo(Object o) {
	UnsignedLong u = (UnsignedLong) o;
	return unsignedCompare(this.value, u.value);
    }

    /** Returns a value that is negative, zero, or positive according as
	the value represented by <code>this</code> is respectively less
	than, equal to, or greater to <code>x</code>.  */
    public int compareTo(long x) {
	/* If x < 0 then it is less than any unsigned value.
	   If this.value < 0 then it represents a positive number larger
	   than any value representable in a long.  */
	if (x < 0 || this.value < 0 || this.value > x)
	    return +1;
	if (this.value == x)
	    return 0;
	return -1;  /* 0 <= this.value < x */
    }

    public boolean equals(Object o) {
	return (o instanceof UnsignedLong &&
		this.value == ((UnsignedLong) o).value);
    }

    public int hashCode() {
	return (int) ((value >>> 32) ^ value);
    }

    /** Return a String that is the decimal representation of this unsigned
	number.  */
    public String toString() {
	return unsignedToString(value);
    }

    /** Return a String that is the hexadecimal representation of this
        unsigned number, without extra leading zeroes.  Lowercase letters
	are used for the digits <code>a</code> to <code>f</code>.
	@see java.lang.Long#toHexString  */
    public static String toHexString(UnsignedLong u) {
	return Long.toHexString(u.value);
    }

    /** Return a String that is the octal representation of this
        unsigned number, without extra leading zeroes.
	@see java.lang.Long#toOctalString  */
    public static String toOctalString(UnsignedLong u) {
	return Long.toOctalString(u.value);
    }

    /** Return a String that is the binary representation of this
	unsigned number, without extra leading zeroes.
	@see java.lang.Long#toBinaryString  */
    public static String toBinaryString(UnsignedLong u) {
	return Long.toBinaryString(u.value);
    }

    /** Return an <code>UnsignedLong</code> whose value is specified by the
	given String in decimal.  Equivalent to <code>valueOf(s, 10)</code>  */
    public static UnsignedLong valueOf(String s) throws NumberFormatException {
	return valueOf(s, 10);
    }

    /** Return an <code>UnsignedLong</code> whose value is specified by the
	given String in the given radix.

	@param s a String representing an unsigned integer in the
	given radix.  The characters in <code>s</code> must all be
	digits of the specified radix (as determined by whether
	<code>Character.digit</code> returns a nonnegative value).

	@param radix the radix in which the digits in <code>s</code> are
	expressed.

	@exception NumberFormatException if <code>s</code> is empty,
	or contains characters that are not digits of the specified
	radix, or specifies a value that is &gt;= 2<sup>64</sup>, or
	if the radix is not between <code>Character.MIN_RADIX</code>
	and <code>Character.MAX_RADIX</code>.
    */
    public static UnsignedLong valueOf(String s, int radix)
	    throws NumberFormatException {
	return make(parseUnsignedLong(s, radix));
    }

    /** Return an <code>UnsignedLong</code> whose value is specified by the
	given String, including a possible prefix indicating the radix.
	If the String begins with <code>0x</code> or <code>#</code>, the
	remainder of the String specifies a hexadecimal number.  If the
	String begins with <code>0</code> followed by other digits, the
	remainder of the String specifies an octal number.  Otherwise,
	the String specifies a decimal number.

	@exception NumberFormatException if <code>s</code> is empty,
	or contains no characters after the prefix, or contains
	characters that are not digits of the specified radix, or
	specifies a value that is &gt;= 2<sup>64</sup>.
    */
    public static UnsignedLong decode(String s) throws NumberFormatException {
	int radix;
	if (s.startsWith("#")) {
	    radix = 16;
	    s = s.substring(1);
	} else if (s.startsWith("0x")) {
	    radix = 16;
	    s = s.substring(2);
	} else if (s.startsWith("0") && s.length() > 1) {
	    radix = 8;
	    s = s.substring(1);
	} else {
	    radix = 10;
	}
	return valueOf(s, radix);
    }

    /** Return an <code>UnsignedLong</code> that is equal mod
        2<sup>64</sup> to <code>this + x</code>.*/
    public UnsignedLong add(UnsignedLong x) {
	/* If the represented values are T and X, and writing t and x
	   for this.value and x.value,
	   t + x = T - s1 * 2^64 + X - s2 * 2^64
	   which is equal mod 2^64 to T + X.  */
	return makeIfDifferent(this.value + x.value);
    }

    /** Return an <code>UnsignedLong</code> that is equal mod
        2<sup>64</sup> to <code>this - x</code>.*/
    public UnsignedLong subtract(UnsignedLong x) {
	/* The reasoning is the same as for add.  */
	return makeIfDifferent(this.value - x.value);
    }

    /** Return an <code>UnsignedLong</code> that is equal mod
        2<sup>64</sup> to <code>this * x</code>.*/
    public UnsignedLong multiply(UnsignedLong x) {
	/* If the represented values are T and X, and writing t and x
	   for this.value and x.value,
	   t * x = (T + s1 * 2^64) + (X + s2 * 2^64)
	     = (T * X) + (s1 * 2^64 * X) + (s2 * 2^64 * T) + (s1 * s2 * 2^128)
	   which is equal mod 2^64 to T * X.  */
	return makeIfDifferent(this.value * x.value);
    }

    /** Return an <code>UnsignedLong</code> that is equal to
	<code>[this / x]</code>, where <code>[x]</code> is the floor
	function. 
    
	@exception ArithmeticException if <code>x</code> is zero.  */
    public UnsignedLong divide(UnsignedLong x) throws ArithmeticException {
	return makeIfDifferent(unsignedDivide(this.value, x.value));
    }

    /** Return an <code>UnsignedLong</code> that is equal to
	<code>this mod x</code>.
    
	@exception ArithmeticException if <code>x</code> is zero.  */
    public UnsignedLong mod(UnsignedLong x) throws ArithmeticException {
	return makeIfDifferent(unsignedMod(this.value, x.value));
    }

    /** Return an <code>UnsignedLong</code> that is equal mod
	2<sup>64</sup> to <code>[this * 2<sup>n</sup>]</code>, where
	<code>[x]</code> is the floor function.  <code>n</code> may
	be negative. */
    public UnsignedLong shiftLeft(int n) {
	if (n < 0)
	    return shiftRight(-n);
	if (n >= 64)
	    return ZERO;
	return makeIfDifferent(this.value << n);
    }

    /** Return an <code>UnsignedLong</code> that is equal mod
	2<sup>64</sup> to <code>[this / 2<sup>n</sup>]</code>, where
	<code>[x]</code> is the floor function.  <code>n</code> may
	be negative. 
    */
    public UnsignedLong shiftRight(int n) {
	if (n < 0)
	    return shiftLeft(-n);
	if (n >= 64)
	    return ZERO;
	return makeIfDifferent(this.value >>> n);
    }

    /** Return an <code>UnsignedLong</code> that is the bitwise
	<code>and</code> of <code>this</code> and <code>x</code>.
    */
    public UnsignedLong and(UnsignedLong x) {
	return makeIfDifferent(this.value & x.value);
    }

    /** Return an <code>UnsignedLong</code> that is the bitwise
	<code>or</code> of <code>this</code> and <code>x</code>.
    */
    public UnsignedLong or(UnsignedLong x) {
	return makeIfDifferent(this.value | x.value);
    }

    /** Return an <code>UnsignedLong</code> that is the bitwise
	<code>xor</code> of <code>this</code> and <code>x</code>.
    */
    public UnsignedLong xor(UnsignedLong x) {
	return makeIfDifferent(this.value ^ x.value);
    }

    /** Return an <code>UnsignedLong</code> that is the bitwise
	<code>not</code> of <code>this</code>, equal to
	<code>2<sup>64</sup> - 1 - this</code>.
    */
    public UnsignedLong not() {
	/* No point in using makeIfDifferent - it's necessarily different.  */
	return make(~this.value);
    }

    /** Return true if and only if the bit representing
        <code>2<sup>n</sup></code> is set in <code>this</code>.  */
    public boolean testBit(int n) {
	if (n < 0 || n >= 64)
	    return false;
	return ((this.value & (1L << n)) != 0);
    }

    /** Return an <code>UnsignedLong</code> that has the designated
	bit set. 
	This means that the bitwise <code>or</code> of
	<code>this</code> and <code>2<sup>n</sup></code>.  
    */
    public UnsignedLong setBit(int n) {
	if (n < 0 || n >= 64)
	    return this;
	return makeIfDifferent(this.value | (1 << n));
    }

    /** Return an <code>UnsignedLong</code> that has the designated
	bit clear.
	This means the bitwise <code>and</code> of
	<code>this</code> and <code>2<sup>64</sup> - 1 - n</code>.
    */
    public UnsignedLong clearBit(int n) {
	if (n < 0 || n >= 64)
	    return this;
	return makeIfDifferent(this.value & ~(1 << n));
    }

    /** Return the smaller of <code>this</code> and <code>x</code>.  */
    public UnsignedLong min(UnsignedLong x) {
	if (unsignedCompare(this.value, x.value) < 0)
	    return this;
	else
	    return x;
    }

    /** Return the larger of <code>this</code> and <code>x</code>.  */
    public UnsignedLong max(UnsignedLong x) {
	if (unsignedCompare(this.value, x.value) > 0)
	    return this;
	else
	    return x;
    }

    static int unsignedCompare(long x, long y) {
	if (x == y)
	    return 0;
	else if (x + Long.MIN_VALUE < y + Long.MIN_VALUE)
	    return -1;
	else
	    return +1;
    }

    static long unsignedMultiply(long x, long y) {
	return x * y;
    }

    static long unsignedDivide(long x, long y) throws ArithmeticException {
	/* There might be a simpler way to do this division, but I'm
	   not sure what it is.  The semantics of Java integer division
	   when the operands are negative are unhelpful, so we avoid
	   that.  Instead, we break down each of the four cases for x and
	   y positive and negative, and do the right thing for each one.
	   In the comments that follow, X and Y are the unsigned numbers
	   represented by the signed values x and y.  [X] is the floor
	   function.  */
	if (x >= 0) {
	    /* Easy case: if y is positive we can safely divide; if y
               is negative, Y >= 2^63, and X < 2^63, so [X/Y] = 0.  */
            if (y >= 0)
		return x / y;
	    else
		return 0;
	} else if (y < 0) {
	    /* Even easier case: x < 0 and y < 0 => 2^63 <= X,Y < 2^64.
               [X/Y] is 1 if X >= Y.  Since both x and y are negative,
	       comparison between x and y is the same as between X and Y.  */
	    if (x < y)
		return 0;
	    else
		return 1;
	} else {
	    /* Hard case: x < 0 and y >= 0.  If y is even, [X/Y] =
	       [[X/2]/[Y/2]].  Otherwise, write K = 2^63, and let
	       X = K + m.  m and y are representable in a long.  Noting
	       that a/b = [a/b] + (a%b)/b, we have X/Y = (K + m)/y =
	       K/y + m/y = [K/y] + (K%y)/y + [m/y] + (m%y)/y.  Unless
	       y is a power of 2, [K/y] = [(K-1)/y] and
	       K%y = ((K - 1) % y) + 1.
	       Therefore, [X/Y] = [K/y] + [m/y] + [((K%y) + (m%y)) / y].
	       We can avoid the last division by noting
	       that [((K%y) + (m%y)) / y] is either 0 or 1.  If y is a
	       power of 2, it must be 1, since we have already dealt with
	       even numbers.  */
	    if ((y & 1) == 0) {  /* y even */
		final long X_div_2 = x >>> 1;
		final long y_div_2 = y >>> 1;
		return X_div_2 / y_div_2;  /* divide by zero if y = 0 */
	    } else if (y == 1) {
		return x;
	    } else {
		final long m = x & Long.MAX_VALUE;
		final long m_div_y = m / y;
		final long m_mod_y = m % y;
		final long K_div_y = Long.MAX_VALUE / y;
		final long K_mod_y = (Long.MAX_VALUE % y) + 1;
		long z = K_div_y + m_div_y;
		if (K_mod_y + m_mod_y >= y)
		    z++;
		return z;
	    }
	}
    }

    static long unsignedMod(long x, long y) {
	/* We could optimize the logic a bit here, but this is simpler.  */
	return x - y * unsignedDivide(x, y);
    }

    static String unsignedToString(long x) {
	if (x >= 0)
	    return Long.toString(x);
	/* x is negative, representing an unsigned number X >= 2^63.
           Hand-code divide and mod by 10.  */
	final long xmod2 = x & 1;
	final long xdiv2 = x >>> 1;
	final long xmod10 = (xdiv2 % 5) * 2 + xmod2;
	final long xdiv10 = xdiv2 / 5;
	return Long.toString(xdiv10) + Long.toString(xmod10);
    }

    static long parseUnsignedLong(String s, int radix)
	    throws NumberFormatException {
        if (s.startsWith("-"))
	    throw new NumberFormatException("negative unsigned");
	/* First attempt to parse the nonnegative number as a long.
           This will work for all values except those that are
           representable as an UnsignedLong but not as a long, namely
           values between 2^63 and 2^64 - 1.  */
	try {
	    return Long.parseLong(s, radix);
	} catch (NumberFormatException e) {
	    /* Exception could be because s is too big to be represented
	       as a long, or it could be because s is garbage.  Separately
	       parse, first, all the characters of s except the last, then,
	       the last character of s.  If there is a garbage character,
	       one parse or the other will fail.  If even the shortened s
	       is too big for a long, then s is too big for an UnsignedLong,
	       even if the radix is only 2.  */
	    final int len = s.length();
	    final long left = Long.parseLong(s.substring(0, len - 1), radix);
	    /* Check that the multiplication left * radix will not overflow.
	       -1 represents 2^64 - 1, the largest representable unsigned long.
	       Call it K.  left * radix > K iff left > K / radix.  (radix > 0.)
	       left is an integer and there is no integer between [K / radix]
	       and K / radix, so left > K / radix iff left > [K / radix].  */
	    if (left > unsignedDivide(-1, radix))
		throw new NumberFormatException("unrepresentable unsigned");
	    final long leftShifted = left * radix;
	    final long right = Long.parseLong(s.substring(len - 1), radix);
	    final long u = leftShifted + right;
	    if (u >= 0)
		throw new NumberFormatException("unrepresentable unsigned");
	    return u;
	}
    }

    /*
    public static void main(String[] args) {
	UnsignedLong a = valueOf(args[0]);
	UnsignedLong b = valueOf(args[1]);
	System.out.println("a = " + a + " [" + a.getClass().getName() + "]");
	System.out.println("b = " + b + " [" + b.getClass().getName() + "]");
	System.out.println("a / b = " + a.divide(b));
    }
    */
}
