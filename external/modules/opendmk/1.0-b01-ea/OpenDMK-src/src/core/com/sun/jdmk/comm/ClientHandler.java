/*
 * @(#)file      ClientHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.23
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
import java.io.*;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;

// jmx RI import
//
import com.sun.jdmk.internal.ClassLogger;


/**
 * The <CODE>ClientHandler</CODE> class is the base class of each
 * JDMK adaptors.<p>
 */

abstract class ClientHandler implements Runnable {

    public ClientHandler(CommunicatorServer server, int id, MBeanServer f, ObjectName n) {
        adaptorServer = server ;
        requestId = id ;
        mbs = f ;
        objectName = n ;
        interruptCalled = false ;
        dbgTag = makeDebugTag() ;
	logger = makeLogger(dbgTag);
	//if (mbs == null ){
	//thread = new Thread (this) ;
	thread =  createThread(this);

	//} else {
	//thread = mbs.getThreadAllocatorSrvIf().obtainThread(objectName,this) ;
	//}
        // Note: the thread will be started by the subclass.
    }

    // thread service
    Thread createThread(Runnable r) {
	return new Thread(this);
    }

    public void interrupt() {
        if (logger.finerOn()) {
            logger.finer("interrupt","start") ;
        }
        interruptCalled = true ;
	if (thread != null) {
            thread.interrupt() ;
	}
        if (logger.finerOn()) {
            logger.finer("interrupt","end") ;
        }
    }
  
  
    public void join() {
	if (thread != null) {
        try {
            thread.join() ;
        }
        catch(InterruptedException x) {
        }
	}
    }
  
    public void run() {

        try {
            //
            // Notify the server we are now active
            //
            adaptorServer.notifyClientHandlerCreated(this) ;

            //
            // Call protocol specific sequence
            //
            doRun() ;
        }
        finally {
            //
            // Now notify the adaptor server that the handler is terminating.
            // This is important because the server may be blocked waiting for
            // a handler to terminate.
            //
            adaptorServer.notifyClientHandlerDeleted(this) ;
        }
    }  
  
    //
    // The protocol-dependent part of the request
    //
    public abstract void doRun() ;  
  
    protected CommunicatorServer adaptorServer = null ;
    protected int requestId = -1 ;
    protected MBeanServer mbs = null ;
    protected ObjectName objectName = null ;
    protected Thread thread = null ;
    protected boolean interruptCalled = false ;
    protected String dbgTag = null ;

    protected String makeDebugTag() {
        return "ClientHandler[" + adaptorServer.getProtocol() + ":" + 
	    adaptorServer.getPort() + "][" + requestId + "]";
    }

    ClassLogger makeLogger(String dbgTag) {
	return new ClassLogger(ClassLogger.LOGGER_ADAPTOR_HTML, dbgTag);
    }

    final ClassLogger logger;

}
