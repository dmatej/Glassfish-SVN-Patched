/*
 * @(#)file      TimedURLConnection.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.6
 * @(#)date      07/04/04
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

package com.sun.jdmk.comm;

import com.sun.jdmk.internal.ClassLogger;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;

/**
 * This class defines  methods to make a socket object with a specified timeout.
 * NPCTE fix for escalation 535848, bug 4653805, SD, 02 May 02
 */
public class TimedURLConnection  {

    private static final int MAKE_URLCONNECTION_OPS = 1;
    private static final int GET_OUTPUTSTREAM_OPS = 2;
    private static final int GET_INPUTSTREAM_OPS = 3;
    private static final int GET_RESPONSECODE_OPS = 4;
    private static final int GET_ERRORSTREAM_OPS = 5;
    private static final String urlConnTimeoutString = new String("operation timed out for URLConnection");

    private long timeOut;
    private URLConnection urlConn = null;

    public TimedURLConnection(URL url, long timeout) throws IOException, SecurityException  {
	if (logger.traceOn()) {
            logger.trace("TimedURLConnection","new TimedURLConnection with timeout: " + timeout);
	}
	timeOut = timeout;
	urlConn = make(url);
    }

    public URLConnection getURLConnection() {
	return  urlConn;
    }


    private class TimeoutOperation extends Thread  {

	private int currentOperartion; // 0: start; 1: stop
	private Object returnObject = null;
	private boolean gotResult = false;
	private URL url = null;
	private URLConnection tmpUrlConn = null;
	private boolean resultGiven = false;
	private boolean timeoutOccurred = false;

	public TimeoutOperation(int operation, URL url) {
	    // should testsomehow timeout is > 0	    
		currentOperartion = operation;	
		this.url = url;
	    }

	public TimeoutOperation(int operation, URLConnection urlConn) {
	    // should testsomehow timeout is > 0	    
		currentOperartion = operation;	
		tmpUrlConn = urlConn;
	    }

	public void run() {
	    switch (currentOperartion) {
	    case MAKE_URLCONNECTION_OPS:
		gotResult = false;
		if (url != null) {
		    try {
			returnObject = (URLConnection) url.openConnection();
			gotResult = true;
		    } catch (InterruptedIOException ie) {
			checkTimeout(ie, "creating URLConnection", "MAKE_URLCONNECTION_OPS");
		    } catch (Exception e) {
			if (logger.debugOn()) {
			    logger.debug("TimeoutOperation:run", "MAKE_URLCONNECTION_OPS, got Exception");
			    e.printStackTrace();
			} 
		    }
		}
		break;
	    case GET_INPUTSTREAM_OPS:	       
		gotResult = false;
		if (urlConn != null) {
		    try {
			returnObject = (InputStream) urlConn.getInputStream() ;
			gotResult = true;
		    } catch (InterruptedIOException ie) {
			checkTimeout(ie, "getting InputStream of an URLConnection", "GET_INPUTSTREAM_OPS");
		    } catch (Exception e) {
			if (logger.debugOn()) {
			    logger.debug("TimeoutOperation:run", "GET_INPUTSTREAM_OPS, got Exception");
			    e.printStackTrace();
			} 
		    }
		}
		break;
	    case GET_OUTPUTSTREAM_OPS:	       
		gotResult = false;
		if (urlConn != null) {
		    try {
			returnObject = (OutputStream) urlConn.getOutputStream() ;
			gotResult = true;
		    } catch (InterruptedIOException ie) {
			checkTimeout(ie, "getting OutputStream of an URLConnection", "GET_OUTPUTSTREAM_OPS");
		    } catch (Exception e) {
			if (logger.debugOn()) {
			    logger.debug("TimeoutOperation:run", "GET_OUTPUTSTREAM_OPS, got Exception");
			    e.printStackTrace();
			} 
		    }
		}
		break;
	    case GET_RESPONSECODE_OPS:	       
		gotResult = false;
		if (urlConn != null) {
		    try {
			int tmpInt = 0;
			tmpInt = ((HttpURLConnection)urlConn).getResponseCode() ;
			returnObject = new Integer(tmpInt);
			gotResult = true;
		    } catch (InterruptedIOException ie) {
			checkTimeout(ie, "getting RespondCode of an Http", "GET_RESPONSECODE_OPS");
		    } catch (IOException ioe) {
			// SD, hack ? set the return code to -1
			returnObject = new Integer(-1);
			gotResult = true;			
			if (logger.debugOn()) {
			    logger.debug("TimeoutOperation:run", "GET_RESPONSECODE_OPS, got IOException, setting dummy response code");
			    ioe.printStackTrace();
			} 
		    } catch (Exception e) {
			if (logger.debugOn()) {
			    logger.debug("TimeoutOperation:run", "GET_RESPONSECODE_OPS, got Exception");
			    e.printStackTrace();
			} 
		    }
		}
		break;
	    case GET_ERRORSTREAM_OPS:	       
		gotResult = false;
		if (urlConn != null) {
		    try {
			returnObject = ((HttpURLConnection)urlConn).getErrorStream() ;
			gotResult = true;
		    } catch (Exception e) {
			if (logger.debugOn()) {
			    logger.debug("TimeoutOperation:run", "GET_ERRORSTREAM_OPS, got Exception");
			    e.printStackTrace();
			} 
		    }
		}
		break;
	    default:
		break;
	    }
	    while (resultGiven == false) {
		try {
		    Thread.currentThread().sleep(100); // sleep for 100 ms
		} catch (Exception e) {
		    if (logger.debugOn()) {
			logger.debug("TimeoutOperation:run", "got Exception while sleeping");
		    } 
		    throw new CommunicationException("run TimeoutOperation, sleep failed");
		}
	    }
	}

	public boolean gotResult() {
	    return gotResult;
	}

	public Object getReturnObject() {
	    resultGiven = true;
	    return returnObject;
	}

	public boolean hasTimeoutOccurred() {
	    return timeoutOccurred;
	}

	public void setTimeoutOccurred(boolean occurred) {
	    timeoutOccurred = occurred;
	}

	private void checkTimeout(Exception ie, String msg1, String msg2) {
	    if (hasTimeoutOccurred()) {
		logger.trace("TimeoutOperation:run", "Timeout while " + msg1);
		setTimeoutOccurred(false);
	    } 
	    if (logger.debugOn()) {
		logger.debug("TimeoutOperation:run", msg2 + ", got Exception");
		ie.printStackTrace();
	    }
	}
    }

    private Object runTimeoutOperation (TimeoutOperation curThread, long timeOut) throws IOException {
	long endTime = 0;
	long curTime = 0;
	boolean threadFinished = false;
	Object retObject = null;

	if (timeOut > 0) {
	    endTime = System.currentTimeMillis() + timeOut;
	} else {
	    if (logger.debugOn()) {
		logger.debug("runTimeoutOperation","bad timeout");
	    }
            throw new CommunicationException("bad balue for timeout");
	}

	curThread.start();	
	curTime = System.currentTimeMillis();
	while (curTime < endTime) {
	    if (curThread.isAlive()) {
		try {
		    Thread.currentThread().sleep(100); // sleep for 100 ms
		    if (curThread.gotResult()) {
			break;
		    }
		} catch (Exception e) {
		    if (logger.debugOn()) {
			logger.debug("runTimeoutOperation", "got Exception while sleeping");
		    } 
		    throw new CommunicationException("run runTimeoutOperation, sleep failed");
		}
		curTime = System.currentTimeMillis();
	    } else {
		// startThread finished
		threadFinished = true;
		break;
	    }
	}

	// we are here, either because we know already get the result,
	// or because timeout has elapsed; in this case, we need to check if we have the
	// result; do it anyway
	if (curThread.gotResult()) {
	    retObject = curThread.getReturnObject();
	} else 	if (threadFinished == false) {	    
	    if (logger.debugOn()) {
		logger.debug("runTimeoutOperation","openConnection timed out");
	    }
	    curThread.setTimeoutOccurred(true);
	    curThread.interrupt();
	    throw new CommunicationException(urlConnTimeoutString);
	}
	return retObject;
    }



    public OutputStream getOutputStream () throws IOException {
       OutputStream tempOut = null;

	if (logger.debugOn()) {
	    logger.debug("getOutputStream","Entering");
	}
       if (urlConn != null) {
	   TimeoutOperation getOutputThread = new TimeoutOperation(GET_OUTPUTSTREAM_OPS, urlConn );
	   tempOut = (OutputStream) runTimeoutOperation(getOutputThread, timeOut);	
       } else {
	    if (logger.debugOn()) {
		logger.debug("getOutputThread","urlConn null !");
	    } 
	    throw new CommunicationException("Failed to get inputStream, no connection");
       }

	if (logger.debugOn()) {
	    logger.debug("getOutputStream","returning" + tempOut);
	}
       return tempOut;	
    }


   public InputStream getInputStream () throws IOException {
       InputStream tempIn = null;

	if (logger.debugOn()) {
	    logger.debug("getInputStream","Entering");
	}
       if (urlConn != null) {
	   TimeoutOperation getInputThread = new TimeoutOperation(GET_INPUTSTREAM_OPS, urlConn );
	   tempIn = (InputStream) runTimeoutOperation(getInputThread, timeOut);	
       } else {
	    if (logger.debugOn()) {
		logger.debug("getInputThread","urlConn null !");
	    } 
	    throw new CommunicationException("Failed to get inputStream, no connection");
       }

	if (logger.debugOn()) {
	    logger.debug("getInputStream","returning: " + tempIn);
	}
       return tempIn;	
    }

   public int getResponseCode () throws IOException {
       Integer tempResponseCode = null;

	if (logger.debugOn()) {
	    logger.debug("getResponseCode","Entering");
	}
       if (urlConn != null) {
	   TimeoutOperation getResponseCodeThread = new TimeoutOperation(GET_RESPONSECODE_OPS, urlConn );
	   tempResponseCode = (Integer) runTimeoutOperation(getResponseCodeThread, timeOut);	
       } else {
	    if (logger.debugOn()) {
		logger.debug("getResponseCodeThread","urlConn null !");
	    } 
	    throw new CommunicationException("Failed to get RespondCode, no connection");
       }

	if (logger.debugOn()) {
	    logger.debug("getResponseCodeThread","returning: " + tempResponseCode.intValue());
	}
       return tempResponseCode.intValue();	
    }

   public InputStream getErrorStream () throws IOException {
       InputStream tempErr = null;

	if (logger.debugOn()) {
	    logger.debug("getErrorStream","Entering");
	}
       if (urlConn != null) {
	   TimeoutOperation getErrorThread = new TimeoutOperation(GET_ERRORSTREAM_OPS, urlConn );
	   tempErr = (InputStream) runTimeoutOperation(getErrorThread, timeOut);	
       } else {
	    if (logger.debugOn()) {
		logger.debug("getErrorThread","urlConn null !");
	    } 
	    throw new CommunicationException("Failed to get errorStream, no connection");
       }

	if (logger.debugOn()) {
	    logger.debug("getErrorStream","returning: " + tempErr);
	}
       return tempErr;	
    }

   // private stuff
    private URLConnection make(URL url) throws IOException {
	URLConnection tempUrlConn = null; 

	if (logger.debugOn()) {
	    logger.debug("make","Entering");
	}
	TimeoutOperation makeThread = new TimeoutOperation(MAKE_URLCONNECTION_OPS, url);	
	tempUrlConn = (URLConnection) runTimeoutOperation(makeThread, timeOut);

	if (logger.debugOn()) {
	    logger.debug("make","returning: " + tempUrlConn);
	}
	return tempUrlConn;
    }

    // trace and debug
    /** The name of this class to be used for tracing */
    private final static String dbgTag = "TimedURLConnection";
    
    final static private ClassLogger logger = 
        new ClassLogger(ClassLogger.LOGGER_CONNECTION_TIMER,dbgTag);


}
