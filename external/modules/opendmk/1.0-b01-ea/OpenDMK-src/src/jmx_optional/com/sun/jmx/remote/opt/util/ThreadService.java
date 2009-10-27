/*
 * @(#)file      ThreadService.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.9
 * @(#)lastedit  07/03/08
 * @(#)build     @BUILD_TAG_PLACEHOLDER@
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

package com.sun.jmx.remote.opt.util;

import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;

public class ThreadService {

    public ThreadService(int min, int max) {
	this(min, max, true);
    }

    public ThreadService(int min, int max, boolean simple) {
	if (min < 0) {
	    throw new IllegalArgumentException("Negative minimal thread number.");
	}

	if (max < min) {
	    throw new IllegalArgumentException("Maximum number less than minimal number.");
	}

	this.min = min;
	this.max = max;
	this.simple = simple;

	defaultPriority = Thread.currentThread().getPriority();
	defaultLoader = getContextClassLoader();

	if (min > 0) {
	    JobExecutor.handoff(new ThreadServiceJob());
	}
    }
 
    public void handoff(Runnable job) {
	isTerminated();

	if (job == null) {
	    throw new IllegalArgumentException("Null job.");
	}

	synchronized(lock) {
	    jobList.add(job);

	    // there are enough idle threads
	    if (jobList.size() <= idle) {
		lock.notify();
		return;
	    }

	    // should ask more threads immidiately
	    if (total < min || total == 0) {
		total++;
		JobExecutor.handoff(new ThreadServiceJob());

		return;
	    }

	    // no more thread can be asked
	    if (total == max) {
		return;
	    }
	}

	// wait a while for working threads become possibly idle
	Thread.yield();

	// see if ask more thread
	synchronized(lock) {
	    if (jobList.size() > idle && total < max) {
		total++;
		JobExecutor.handoff(new ThreadServiceJob());
	    }
	}
    }

    public void terminate() {
	synchronized(lock) {
	    if (terminated) {
		return;
	    }

	    terminated = true;

	    jobList.clear();
	    lock.notifyAll();
	
	    Thread ct = Thread.currentThread();

	    while (threadList.size() > 0) {
		Thread rt = (Thread)threadList.remove(0);
		if (ct != rt) {
		    rt.interrupt();
		}		    
	    }
	}		
    }

//     public static ThreadService getShared() {
// 	return shared;
//     }

// private stuff
    private class ThreadServiceJob implements Runnable {
        public ThreadServiceJob() {
        }

        public void run() {
	    Thread currentThread = Thread.currentThread();

	    synchronized(lock) {
		threadList.add(currentThread);
	    }

	    // init
	    currentThread.setPriority(defaultPriority);
	    currentThread.interrupted();
	    setContextClassLoader(currentThread, defaultLoader);

	    Runnable job = null;

	    while (!terminated) {
		synchronized(lock) {
		    if (jobList.size() == 0) {
			if (total > min) {
			    if (idle == 0) { // keep one with timeout
				idle++;

				// waiting with timeout
				long remainingTime = LEAVING_WAITING_TIME;
				final long startTime = System.currentTimeMillis();

				while(jobList.size() <= 0 && remainingTime > 0) {
				    try {
					lock.wait(remainingTime);
				    } catch (InterruptedException e) {
					// clean the flag, the thread will be reused.
					currentThread.interrupted();

					if (terminated) {
					    return;
					}
				    }

				    remainingTime = LEAVING_WAITING_TIME -
					(System.currentTimeMillis() - startTime);
				}

				idle--;

				if (jobList.size() > 0) {
				    job = (Runnable)jobList.remove(0);
				} else {
				    // still no job, leaving
				    // do here within synchronization
				    total--;
				    threadList.remove(currentThread);

				    break;
				}
			    } else {
				// outside of min and some other threads are idle, leave
				// do here within synchronization
				total--;
				threadList.remove(currentThread);

				break;
			    }
			} else { // staying within min
			    idle++;

			    while(!terminated && jobList.size() <= 0) {
				try {
				    lock.wait();			
				} catch (InterruptedException ire) {
				    // OK
				    // should not happen in this step
				}
			    }
			    
			    idle--;
			    
			    if (terminated) {
				break;
			    }

			    job = (Runnable)jobList.remove(0);
			}
		    } else {
			job = (Runnable)jobList.remove(0);
                    }
		}

		if (terminated) {
		    break;
		}

		try {
		    job.run();
		} catch (Exception e) {
		    if (logger.warningOn()) {
			logger.warning("run", "Got an unexpected exception.", e);
		    }
		} finally {
		    job = null;
		}

		currentThread.interrupted();

		if (!simple) {
		    // re-init
		    currentThread.setPriority(defaultPriority);
		    setContextClassLoader(currentThread, defaultLoader);
		}
	    }

	    // important!!!
	    // This thread will possibly go to another ThreadService,
	    // it is important to remove its interrupted flag (if set)
	    currentThread.interrupted();
	}
    }

    protected void finalize() {
	terminate();
    }

    private void isTerminated() throws IllegalStateException {
	if (terminated) {
	    throw new IllegalStateException("The Thread Service has been terminated.");
	}
    }

    private ClassLoader getContextClassLoader() {
	return (ClassLoader)
	    AccessController.doPrivileged(new PrivilegedAction() {
		    public Object run() {
			return Thread.currentThread().getContextClassLoader();
		    }
	    });
    }

    private void setContextClassLoader(final Thread currentThread,
				       final ClassLoader classloader) {
	AccessController.doPrivileged(new PrivilegedAction() {
		public Object run() {
		    currentThread.setContextClassLoader(classloader);
		    return null;
		}
	});
    }

// private stuff
    private int min;
    private int max;
    private boolean simple;

    private int total = 0;
    private int idle = 0;
    private boolean terminated = false;

    private ArrayList jobList = new ArrayList();
    private ArrayList threadList = new ArrayList();

    private static final ThreadService shared = new ThreadService(0, Integer.MAX_VALUE);

    private int defaultPriority;
    private ClassLoader defaultLoader;
    private int[] lock = new int[0];

    private static final int LEAVING_WAITING_TIME = 1000;

    private static final ClassLogger logger = 
	new ClassLogger("com.sun.jmx.remote.opt.util", 
			"ThreadService");
}
