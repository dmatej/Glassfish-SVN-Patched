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

/*
 * ProcessInstance.java
 *
 * Created on October 23, 2003, 2:25 PM
 */

package com.sun.enterprise.ee.nodeagent;

import java.util.Observable;
import java.util.Observer;
import java.util.Properties;
import java.util.Vector;
import java.util.Enumeration;
import java.util.Iterator;
import java.lang.Process;
import java.io.InputStreamReader;
import java.io.BufferedReader;

import com.sun.enterprise.util.ProcessExecutor;

/**
 *
 * @author  basler
 */
public class ProcessInstanceExternal extends AbstractProcessInstance {
    

    public void startInstance() throws ProcessManagerException {
        try {
            ProcessExecutor pe=new ProcessExecutor(getStartCommandAsArray());
            pe.execute(false, false);
            setProcess(pe.getSubProcess());
        } catch (Exception e) {
            throw new ProcessManagerException(e);
        }
    }

   
    public void stopInstance() throws ProcessManagerException {
        try {
            ProcessExecutor pe=new ProcessExecutor(getStopCommandAsArray());
            pe.execute(false, false);
        } catch (Exception e) {
            throw new ProcessManagerException(e);
        }
    }

    public void stopInstance(int timeout) throws ProcessManagerException {
        throw new UnsupportedOperationException("Not Supported");
    }    
}
