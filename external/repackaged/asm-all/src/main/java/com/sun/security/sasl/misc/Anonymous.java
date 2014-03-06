/*
 * @(#)Anonymous.java	1.6 01/03/22
 *
 * Copyright 1999-2000 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 */

package com.sun.security.sasl.misc; 

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import com.sun.security.sasl.preview.*;

/**
  * Implements the ANONYMOUS SASL mechanism. 
  * (<A HREF="ftp://ftp.isi.edu/in-notes/rfc2245.txt">RFC 2245</A>).
  * The Anonymous mechanism sends the trace information that it is given
  * (such as an email or distinguished name) as the initial response
  * and then it is complete.
  *
  * @author Rosanna Lee
  */
public class Anonymous implements SaslClient {
    private byte[] trace;
    private boolean completed = false;

    /**
     * Constructs an Anonymous mechanism with optional trace information.
     * 
     * @param traceInfo If non-null, the string will be used for trace information.
     * @exception SaslException If cannot encode traceInfo in UTF-8
     */
    public Anonymous(String traceInfo) throws SaslException {
	if (traceInfo instanceof String) {
	    try {
		trace = ((String)traceInfo).getBytes("UTF8");
	    } catch (java.io.UnsupportedEncodingException e) {
		throw new SaslException("Cannot encode trace info in UTF-8", e);
	    }
	} else {
	    trace = new byte[0];
	}
    }

    /**
     * Retrieves this mechanim's name for initiating the Anonymous protocol
     * exchange.
     *
     * @return  The string "ANONYMOUS".
     */
    public String getMechanismName() {
	return "ANONYMOUS";
    }

    public boolean hasInitialResponse() {
	return true;
    }

    public void dispose() throws SaslException {
    }

    /**
     * Processes the challenge data. 
     * It returns the ANONYMOUS mechanism's initial response, 
     * which is the trace information encoded in UTF-8.
     * This is the optional information that is sent along with the SASL command.
     * After this method is called, isComplete() returns true.
     * 
     * @param challengeData Ignored.
     * @return The possibly empty initial response (username) 
     * @throws SaslException If authentication already been complete.
     */
    public byte[] evaluateChallenge(byte[] challengeData) throws SaslException {
	if (completed) {
	    throw new SaslException("Already completed");
	}
	completed = true;
	return trace;
    }

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
	    throw new SaslException("ANONYMOUS has no supported QOP");
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
	    throw new SaslException("ANONYMOUS has no supported QOP");
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
     * @return null No property is applicable to this mechanism.
     * @exception SaslException if this authentication exchange has not completed
     */
    public String getNegotiatedProperty(String propName) throws SaslException {
	if (completed) {
	    return null;
	} else {
	    throw new SaslException("Not completed");
	}
    }
}
