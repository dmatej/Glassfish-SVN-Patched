/*
 * @(#)file      CommunicationException.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.19
 * @(#)lastedit      07/03/08
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
 *
 */

package com.sun.jdmk.comm;

// java import
//
import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * Represents exceptions raised due to communications problems,
 * for example when a managed object server is out of reach.<p>
 *
 */

public class CommunicationException extends javax.management.JMRuntimeException {

    /* Serial version */
    private static final long serialVersionUID = -2499186113233316177L;

    /**
     * Constructs a CommunicationException with a target exception.
     */
    public CommunicationException(Throwable target) {
	super(target.getMessage());
	this.target = target;
    }

    /**
     * Constructs a CommunicationException with a target exception
     * and a detail message.
     */
    public CommunicationException(Throwable target, String msg) {
	super(msg);
	this.target = target;
    }

    /**
     * Constructs a CommunicationException with a detail message.
     */
    public CommunicationException(String msg) {
	super(msg);
    }

    /**
     * Get the thrown target exception.
     */
    public Throwable getTargetException() {
	return target;
    }

    /**
     * Prints the stack trace of the thrown target exception.
     *
     */
    public void printStackTrace() {
	printStackTrace(System.err);
    }

    /**
     * Prints the stack trace of the thrown target exception to the specified
     * print stream.
     */
    public void printStackTrace(PrintStream ps) {
	synchronized (ps) {
	    if (target != null) {
		ps.print("com.sun.jdmk.comm.CommunicationException: ");
		target.printStackTrace(ps);
	    } else {
		super.printStackTrace(ps);
	    }
	}
    }

    /**
     * Prints the stack trace of the thrown target exception to the
     * specified print writer.
     */
    public void printStackTrace(PrintWriter pw) {
	synchronized (pw) {
	    if (target != null) {
		pw.print("com.sun.jdmk.comm.CommunicationException: ");
		target.printStackTrace(pw);
	    } else {
		super.printStackTrace(pw);
	    }
	}
    }

    /**
     * @serial This field holds the target if the
     *         CommunicationException(Throwable target) 
     *         constructor was used to instantiate the object.
     */
    private Throwable target;
}
