/*
 * @(#)CramMD5.java	1.11 01/03/22
 *
 * Copyright 1999-2000 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 */

package com.sun.security.sasl.misc; 

import com.sun.security.sasl.preview.*;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

// For HMAC_MD5
import java.security.NoSuchAlgorithmException;
import java.security.MessageDigest;

/**
  * Implements the CRAM-MD5 SASL mechanism. 
  * (<A HREF="ftp://ftp.isi.edu/in-notes/rfc2195.txt">RFC 2195</A>).
  * CRAM-MD5 has no initial response. It receives bytes from
  * the server as a challenge, which it hashes by using MD5 and the password.
  * It concatenates the authentication ID with this result and returns it
  * as the response to the challenge. At that point, the exchange is complete.
  *
  * @author Vincent Ryan
  * @author Rosanna Lee
  */
public class CramMD5 implements SaslClient {
    private boolean completed = false;
    private byte[] pw;
    private String username;

    /**
     * Creates a SASL mechanism with client credentials that it needs 
     * to participate in CRAM-MD5 authentication exchange with the server.
     *
     * @param authID A  non-null string representing the principal 
     * being authenticated.
     *
     * @param pw A non-null String or byte[]
     * containing the password. If it is an array, it is first cloned.
     */
    public CramMD5(String authID, byte[] pw) throws SaslException {
	if (authID == null || pw == null) {
	    throw new SaslException(
		"CRAM-MD5: authentication ID and password must be specified");
	}
	
	username = authID;
	this.pw = pw;  // caller should have already cloned
    }

    /**
     * Retrieves this mechanism's name for to initiate the CRAM-MD5 protocol
     * exchange.
     *
     * @return  The string "CRAM-MD5".
     */
    public String getMechanismName() {
	return "CRAM-MD5";
    }

    /**
     * CRAM-MD5 has no initial response.
     */
    public boolean hasInitialResponse() {
	return false;
    }

    public void dispose() throws SaslException {
    }

    /**
     * Processes the challenge data.
     * 
     * The server sends a challenge data using which the client must
     * compute an MD5-digest with its password as the key.
     *
     * @param challengeData A non-null byte array containing the challenge 
     * 	      data from the server.
     * @return A non-null byte array containing the response to be sent to 
     * 	      the server.
     * @throws SaslException If platform does not have MD5 support or if
     * this method is invoked more than once.
     */
    public byte[] evaluateChallenge(byte[] challengeData) 
	throws SaslException {

        // See if we've been here before
	if (completed) {
	    throw new SaslException("Already completed");
	}
	completed = true;

	// generate a keyed-MD5 digest from the user's password and challenge.
        try {
	    String digest = HMAC_MD5(pw, challengeData);

	    // clear it when we no longer need it
	    clearPassword();

	    // response is username + " " + digest
	    String resp = username + " " + digest;

	    return resp.getBytes("UTF8");
	} catch (java.security.NoSuchAlgorithmException e) {
	    throw new SaslException("MD5 algorithm not available on platform", e);
	} catch (java.io.UnsupportedEncodingException e) {
	    throw new SaslException("UTF8 not available on platform", e);
	}
    }

    /**
     * Determines whether this mechanism has completed.
     * CRAM-MD5 completes after processing one challenge from the server.
     *
     * @return true if has completed; false otherwise;
     */
    public boolean isComplete() {
	return completed;
    }

    /**
      * Unwraps the incoming buffer.
      *
      * @throws SaslException Not applicable to this mechanism.
      */
    public byte[] unwrap(byte[] incoming, int offset, int len)
	throws SaslException {
	if (completed) {
	    throw new SaslException(
		"CRAM-MD5 supports neither integrity nor privacy");
	} else {
	    throw new SaslException("Not completed");
	}
    }

    /**
      * Wraps the outgoing buffer.
      *
      * @throws SaslException Not applicable to this mechanism.
      */
    public byte[] wrap(byte[] outgoing, int offset, int len) throws SaslException {
	if (completed) {
	    throw new SaslException(
		"CRAM-MD5  supports neither integrity nor privacy");
	} else {
	    throw new SaslException("Not completed");
	}
    }

    /**
     * Retrieves the negotiated property.
     * This method can be called only after the authentication exchange has
     * completed (i.e., when <tt>isComplete()</tt> returns true); otherwise, a
     * <tt>SaslException</tt> is thrown.
     * 
     * @return value of property; only QOP is applicable to CRAM-MD5.
     * @exception SaslException if this authentication exchange has not completed
     */
    public String getNegotiatedProperty(String propName) throws SaslException {
	if (completed) {
	    if (propName.equals(Sasl.QOP)) {
		return "auth";
	    } else {
		return null;
	    }
	} else {
	    throw new SaslException("Not completed");
	}
    }

    private void clearPassword() {
	if (pw != null) {
	    // zero out password
	    for (int i = 0; i < pw.length; i++) {
		pw[i] = (byte)0;
	    }
	    pw = null;
	}
    }

    protected void finalize() {
	clearPassword();
    }

    static private final int MD5_BLOCKSIZE = 64;
    /**
     * Hashes its input arguments according to HMAC-MD5 (RFC 2104)
     * and returns the resulting digest in its ASCII representation.
     *
     * HMAC-MD5 function is described as follows:
     *
     *	     MD5(key XOR opad, MD5(key XOR ipad, text))
     *
     * where key  is an n byte key
     *       ipad is the byte 0x36 repeated 64 times
     *       opad is the byte 0x5c repeated 64 times
     *       text is the data to be protected
     */
    private final static String HMAC_MD5(byte[] key, byte[] text) 
	throws NoSuchAlgorithmException {

	MessageDigest md5 = MessageDigest.getInstance("MD5");

	/* digest the key if longer than 64 bytes */
	if (key.length > 64) {
	    key = md5.digest(key);
	}

	byte[] ipad = new byte[MD5_BLOCKSIZE];	/* inner padding */
	byte[] opad = new byte[MD5_BLOCKSIZE];	/* outer padding */
	byte[] digest;
	int i;

	/* store key in pads */
	for (i = 0; i < MD5_BLOCKSIZE; i++) {
	    for ( ; i < key.length; i++) {
		ipad[i] = key[i];
		opad[i] = key[i];
	    }
	    ipad[i] = 0x00;
	    opad[i] = 0x00;
	}

	/* XOR key with pads */
	for (i = 0; i < MD5_BLOCKSIZE; i++) {
	    ipad[i] ^= 0x36;
	    opad[i] ^= 0x5c;
	}

	/* inner MD5 */
	md5.update(ipad);
	md5.update(text);
	digest = md5.digest();

	/* outer MD5 */
	md5.update(opad);
	md5.update(digest);
	digest = md5.digest();

	// Get character representation of digest
	StringBuffer digestString = new StringBuffer();

	for (i = 0; i < digest.length; i++) {
	    if ((digest[i] & 0x000000ff) < 0x10) {
		digestString.append("0" +
		    Integer.toHexString(digest[i] & 0x000000ff));
	    } else {
		digestString.append(
		    Integer.toHexString(digest[i] & 0x000000ff));
	    }
	}

	return (digestString.toString());
    }
}
