/*
 * @(#)file      TimedExecutor.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.7
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

public class TimedExecutor implements TaskServer {

    public TimedExecutor() {
	this(2);
    }

    public TimedExecutor(int threads) {
	if (threads <= 0) {
	    throw  new IllegalArgumentException("Thread number should be bigger than zero.");
	}

	tService = new ThreadService(threads);

	exeThread = new ExecutorThread();
	exeThread.start();
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
	submitTask((Runnable)task,0);
    }

    /**
     * Submit a task to be executed after a given delay.
     * Once a task is submitted, it is guaranteed that either
     * {@link com.sun.jdmk.tasks.Task#run() task.run()} or 
     * {@link com.sun.jdmk.tasks.Task#cancel() task.cancel()} will be called.
     * This implementation of TaskServer uses a thread pool to execute
     * the submitted tasks.
     * @param task The task to be executed.
     * @exception IllegalArgumentException if the submitted task is null.
     **/
    public void submitTask(Task task, long delay) 
	throws IllegalArgumentException {
	submitTask((Runnable)task,delay);
    }

    /**
     * Submit a task to be executed after a given delay.
     * This implementation of TaskServer uses a thread pool to execute
     * the submitted tasks.
     * @param task The task to be executed.
     * @exception IllegalArgumentException if the submitted task is null.
     **/
    public void submitTask(Runnable task, long delay) 
	throws IllegalArgumentException {
	if (task == null) {
	    throw new IllegalArgumentException("No task specified.");
	}

// 	if (delay < 0) {
// 	    throw new IllegalArgumentException("Illegal delay is specified.");
// 	}

	// delay will be changed to absolute time!!!
	long startTime = delay + System.currentTimeMillis();

	JobInfo newJob = new JobInfo(task, startTime);
 
	// put the job at a right place
	boolean added = false;

	synchronized(jobInfoList) {
	    int lg = jobInfoList.size();
	    for (int i=lg-1; i>=0; i--) {
		JobInfo info = (JobInfo)jobInfoList.get(i);
		if (newJob.startTime >= info.startTime) {
		    jobInfoList.add(i+1, newJob);
		    added = true;
		    break;
		}
	    }
		
	    if (!added) {
		jobInfoList.add(0, newJob);
	    }
		
	    // wake up the executing thread. 
	    jobInfoList.notify();
	}
    }

    public Runnable removeTask(Runnable task) {
	Runnable removed = null;

	synchronized(jobInfoList) {
	    int lg = jobInfoList.size();

	    for (int i=0; i<lg; i++) {
		JobInfo info = (JobInfo)jobInfoList.get(i);
		if (task == info.job) {
		    removed = ((JobInfo)jobInfoList.remove(i)).job;

		    if (i == 0) {
			// wake up the executing thread.
			jobInfoList.notify();
		    }

		    break;
		}
	    }
	}

	if (removed != null && removed instanceof Task)
	    ((Task)removed).cancel();

	return removed;
    }

    public void removeAll() {
	final JobInfo[] jobs;
	synchronized(jobInfoList) {
	    jobs = new JobInfo[jobInfoList.size()];
	    jobInfoList.toArray(jobs);
	    jobInfoList.clear();
	    jobInfoList.notifyAll();
	}
	final int len = jobs.length;
	for (int i=0; i<len ; i++) {
	    final Object o = jobs[i].job;
	    if (o!=null && o instanceof Task) ((Task)o).cancel();
	}
    }	

    // to terminate
    public void terminate() {
	terminated = true;

	removeAll();

	// ...
	try {
	    Thread.sleep(100);
	    exeThread.interrupt();

	    // if the thread is executing ...
	    // TODO
	} catch (Exception e) {}
    }

// private classes
// ---------------

    // A thread used to execute jobs
    //
    private class ExecutorThread extends Thread {
	public ExecutorThread() {
	    super("ExecutorThread");
	    setDaemon(true);
	}

	public void run() {
	    while(!terminated) {
		JobInfo ji = getJobInfo();

		if (terminated) {	
		    break;
		}

		try {
		    tService.submitTask(ji.job);
		} catch (Exception e) {
		    // TODO
		    e.printStackTrace();
		}
	    }
	}

	private JobInfo getJobInfo() {
	    JobInfo ji = null;

	    synchronized(jobInfoList) {
		while (!terminated) {
		    if (jobInfoList.size() == 0) {
			try {
			    jobInfoList.wait();

			    continue;
			} catch (InterruptedException e) {}
		    }

		    ji = (JobInfo)jobInfoList.get(0);
		    long dt = ji.startTime - System.currentTimeMillis();
		    if (dt <= 0) {
			jobInfoList.remove(0);
			break;
		    } else {
			try {
			    jobInfoList.wait(dt);

			    continue;
			} catch (InterruptedException e) {}
		    }
		}
	    }

	    return ji;
	}
    }

    // used to save the info about a job
    //
    private class JobInfo {
	public Runnable job;
	public long startTime;

	public JobInfo(Runnable job,
		       long startTime) {
	    this.job = job;
	    this.startTime = startTime;
	}
    }


// protected or private variables
// ------------------------------
    private ThreadService tService;

    private ArrayList jobInfoList = new ArrayList();

    private ExecutorThread exeThread;

    private boolean terminated;

    private long doneJob = 0;
    private long addedJob=0;
}
