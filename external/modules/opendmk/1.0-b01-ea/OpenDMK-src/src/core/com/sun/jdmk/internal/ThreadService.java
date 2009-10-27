/*
 * @(#)file      ThreadService.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.10
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
 *
 */

package com.sun.jdmk.internal;

import java.util.ArrayList;
import com.sun.jdmk.tasks.Task;
import com.sun.jdmk.tasks.TaskServer;

/**
 * This class implements a {@link com.sun.jdmk.tasks.TaskServer} over
 * a thread pool.
 **/
public class ThreadService implements TaskServer {

    public ThreadService(int threadNumber) {
	if (threadNumber <= 0) {
	    throw new IllegalArgumentException("The thread number should bigger than zero.");
	}

	minThreads = threadNumber;
	threadList = new ExecutorThread[threadNumber];

// 	for (int i=0; i<threadNumber; i++) {
// 	    threadList[i] = new ExecutorThread();
// 	    threadList[i].start();
// 	}

	priority = Thread.currentThread().getPriority();
	cloader = Thread.currentThread().getContextClassLoader();
    }

// public methods
// --------------

    /**
     * Submit a task to be executed.
     * Once a task is submitted, it is guaranteed that either
     * {@link com.sun.jdmk.tasks.Task#run() task.run()} or 
     * {@link com.sun.jdmk.tasks.Task#cancel() task.cancel()} will be called.
     * This implementation of TaskServer uses a thread pool to execute
     * the submitted tasks.
     * @param task The task to be executed.
     * @exception IllegalArgumentException if the submitted task is null.
     **/
    public void submitTask(Task task) throws IllegalArgumentException {
	submitTask((Runnable)task);
    }

    /**
     * Submit a task to be executed.
     * This implementation of TaskServer uses a thread pool to execute
     * the submitted tasks.
     * @param task The task to be executed.
     * @exception IllegalArgumentException if the submitted task is null.
     **/
    public void submitTask(Runnable task) throws IllegalArgumentException {
	stateCheck();

	if (task == null) {
	    throw new IllegalArgumentException("No task specified.");
	}

	synchronized(jobList) {
	    jobList.add(jobList.size(), task);
	    jobList.notify();
	}

	createThread();
    }

    public Runnable removeTask(Runnable task) {
	stateCheck();

	Runnable removed = null;
	synchronized(jobList) {
	    int lg = jobList.indexOf(task);
	    if (lg >= 0) {
		removed = (Runnable)jobList.remove(lg);
	    }
	}
	if (removed != null && removed instanceof Task) 
	    ((Task) removed).cancel();
	return removed;
    }

    public void removeAll() {
	stateCheck();
	
	final Object[] jobs;
	synchronized(jobList) {
	    jobs = jobList.toArray();
	    jobList.clear();
	}
	final int len = jobs.length;
	for (int i=0; i<len ; i++) {
	    final Object o = jobs[i];
	    if (o!= null && o instanceof Task) ((Task)o).cancel();
	}
    }

    // to terminate
    public void terminate() {

	if (terminated == true) {
	    return;
	}

	terminated = true;

	synchronized(jobList) {
	    jobList.notifyAll();
	}

	removeAll();

	for (int i=0; i<currThreds; i++) {
	    try {
		threadList[i].interrupt();
	    } catch (Exception e) {
		// TODO
	    }
	}

	threadList = null;
    }

// private classes
// ---------------

    // A thread used to execute jobs
    //
    private class ExecutorThread extends Thread {
	public ExecutorThread() {
	    super(threadGroup, "ThreadService-"+counter++);
	    setDaemon(true);

	    // init
	    this.setPriority(priority);
	    this.setContextClassLoader(cloader);
 
	    idle++;
	}

	public void run() {

	    while(!terminated) {
		Runnable job = null;

		synchronized(jobList) {
		    if (jobList.size() > 0) {
		        job = (Runnable)jobList.remove(0);
			if (jobList.size() > 0) {
			    jobList.notify();
			}
			
		    } else {
			try {
			    jobList.wait();
			} catch (InterruptedException ie) {
			    // terminated ?
			} 
			continue;
		    }
		}
		if (job != null) {
		    try {
			idle--;
			job.run();
		    } catch (Exception e) { 
			// TODO
			e.printStackTrace();
		    } finally {
			idle++;
		    }
		}

		// re-init
		this.setPriority(priority);
		this.interrupted();
		this.setContextClassLoader(cloader);
	    }
	}
    }

// private methods
    private void stateCheck() throws IllegalStateException {
	if (terminated) {
	    throw new IllegalStateException("The thread service has been terminated.");
	}
    }

    private void createThread() {
	if (idle < 1) {
	    synchronized(threadList) {
		if (jobList.size() > 0 && currThreds < minThreads) {
		    ExecutorThread et = new ExecutorThread();
		    et.start();
		    threadList[currThreds++] = et;
		}
	    }
	}
    }


// protected or private variables
// ------------------------------
    private ArrayList jobList = new ArrayList(0);

    private ExecutorThread[] threadList;
    private int minThreads = 1;
    private int currThreds = 0;
    private int idle = 0;

    private boolean terminated = false;
    private int priority;
    private ThreadGroup threadGroup = new ThreadGroup("ThreadService");
    private ClassLoader cloader;

    private static long counter = 0;

    private int addedJobs = 1;
    private int doneJobs = 1;
}
