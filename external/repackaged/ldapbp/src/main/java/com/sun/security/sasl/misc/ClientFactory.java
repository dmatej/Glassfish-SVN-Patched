/*
 * @(#)ClientFactory.java	1.3 01/06/27
 *
 * Copyright 1999-2000 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 */

package com.sun.security.sasl.misc; 

import com.sun.security.sasl.preview.*;
import com.sun.security.sasl.util.Policy;

import java.util.Map;
import java.io.IOException;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/**
  * Client factory for CRAM-MD5, ANONYMOUS, PLAIN.
  *
  * @author Rosanna Lee
  */
public class ClientFactory implements SaslClientFactory {
    private static final String myMechs[] = {
	"CRAM-MD5", // 
	"PLAIN",    // noplaintext
	"ANONYMOUS",// noplaintext
    };

    private static final int mechPolicies[] = {
	Policy.NOPLAINTEXT|Policy.NOANONYMOUS,      // CRAM-MD5
	Policy.NOANONYMOUS, 			    // PLAIN
	Policy.NOPLAINTEXT		            // ANONYMOUS
    };

    private static final int CRAMMD5 = 0;
    private static final int PLAIN = 1;
    private static final int ANONYMOUS = 2;

    private byte[] bytepw;
    private String authId;

    public ClientFactory() {
    }

    public SaslClient createSaslClient(String[] mechs,
	String authorizationId,
	String protocol,
	String serverName,
	Map props,
	CallbackHandler cbh) throws SaslException {

	    for (int i = 0; i < mechs.length; i++) {
		if (mechs[i].equals(myMechs[CRAMMD5])
		    && Policy.checkPolicy(mechPolicies[CRAMMD5], props)) {
		    getUserInfo("CRAM-MD5", authorizationId, cbh);

		    // Callee responsible for clearing bytepw
		    return new CramMD5(authId, bytepw);

		} else if (mechs[i].equals(myMechs[PLAIN])
		    && Policy.checkPolicy(mechPolicies[PLAIN], props)) {
		    getUserInfo("PLAIN", authorizationId, cbh);

		    // Callee responsible for clearing bytepw
		    return new Plain(authorizationId, authId, bytepw);

		} else if (mechs[i].equals(myMechs[ANONYMOUS])
		    && Policy.checkPolicy(mechPolicies[ANONYMOUS], props)) {
		    return new Anonymous(authorizationId);
		}
	    }
	    return null;
    };

    public String[] getMechanismNames(Map props) {
	return Policy.filterMechs(myMechs, mechPolicies, props);
    }

    /**
     * Gets the authentication id and password. The
     * password is converted to bytes using UTF-8 and stored in bytepw.
     * The authentication id is stored in authId.
     *
     * @param prefix The non-null prefix to use for the prompts (e.g., mechanism
     *  name)
     * @param authorizationId The possibly null authorization id. This is used
     * as a default for the NameCallback. If null, no prefix.
     * @param cbh The non-null callback handler to use.
     */
    private void getUserInfo(String prefix, String authorizationId, 
	CallbackHandler cbh) throws SaslException {
	try {
	    String userPrompt = prefix + " authentication id: ";
	    String passwdPrompt = prefix + " password: ";

	    NameCallback ncb = authorizationId == null? 
		new NameCallback(userPrompt) :
		new NameCallback(userPrompt, authorizationId);

	    PasswordCallback pcb = new PasswordCallback(passwdPrompt, false);

	    cbh.handle(new Callback[]{ncb,pcb});

	    char[] pw = pcb.getPassword();

	    if (pw != null) {
		bytepw = new String(pw).getBytes("UTF8");
		pcb.clearPassword();
	    } else {
		bytepw = null;
	    }

	    authId = ncb.getName();

	} catch (IOException e) {
	    throw new SaslException("Cannot get password", e);
	} catch (UnsupportedCallbackException e) {
	    throw new SaslException("Cannot get userid/password", e);
	}
    }
}
