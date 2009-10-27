/* 
 * @(#)ProxyRef.java	1.3
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

package com.sun.jmx.remote.opt.internal;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.lang.reflect.Method;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.Operation;
import java.rmi.server.RemoteCall;
import java.rmi.server.RemoteObject;
import java.rmi.server.RemoteRef;

public class ProxyRef implements RemoteRef {
    public ProxyRef(RemoteRef ref) {
	this.ref = ref;
    }

    public void readExternal(ObjectInput in)
	    throws IOException, ClassNotFoundException {
	ref.readExternal(in);
    }

    public void writeExternal(ObjectOutput out) throws IOException {
	ref.writeExternal(out);
    }

    public void invoke(RemoteCall call) throws Exception {
	ref.invoke(call);
    }

    public Object invoke(Remote obj, Method method, Object[] params,
			 long opnum) throws Exception {
	return ref.invoke(obj, method, params, opnum);
    }

    public void done(RemoteCall call) throws RemoteException {
	ref.done(call);
    }

    public String getRefClass(ObjectOutput out) {
	return ref.getRefClass(out);
    }

    public RemoteCall newCall(RemoteObject obj, Operation[] op, int opnum,
			      long hash) throws RemoteException {
	return ref.newCall(obj, op, opnum, hash);
    }

    public boolean remoteEquals(RemoteRef obj) {
        return ref.remoteEquals(obj);
    }

    public int remoteHashCode() {
        return ref.remoteHashCode();
    }

    public String remoteToString() {
        return ref.remoteToString();
    }

    protected RemoteRef ref;
}
    
