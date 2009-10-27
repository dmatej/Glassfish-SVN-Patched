/* 
 * @(#)file      DaemonTaskServer.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.17
 * @(#)lastedit  07/03/08
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

// NPCTE fix for bugId 4510777, esc 532372, MR October 2001 
// file DaemonTaskServer.java created for this bug fix

package com.sun.jdmk.tasks;

import com.sun.jdmk.internal.ClassLogger;
import java.util.LinkedList;
import java.util.NoSuchElementException;

/**
 * This class implements a Task Server that runs in its own 
 * thread. 
 * Objects can submit tasks to this object, and they will be executed
 * in background in this object daemon thread. The submitted tasks
 * should not block or last too long - because it will prevent following
 * tasks to be executed.
 * When a task producer calls the 
 * <code>submitTask(Task)</code> method, the task is put inside a FIFO 
 * list, and the Task Server Thread is waken up, if necessary.
 * After that, the method return.
 * <p>
 * The Task Server Thread then asynchronously takes the tasks
 * out of the FIFO list, and invokes the run() method.
 * <p>
 * This mechanism guarantee that the Task producer will spend
 * a minimum time invoking the tasks, and will not be blocked.
 * @see com.sun.jdmk.tasks.Task
 * @see com.sun.jdmk.tasks.TaskServer
 *
 * @since Java DMK 5.0
 **/
 
public class DaemonTaskServer  implements TaskServer {

    // As far as I know the Daemon Task Server is only used by SNMP.
    // It is likely to be deprecated in favor of JDK 5 concurent utilities
    // (Executors) - so the best for now would probably be to log in SNMP
    // logger. However since this is a general purpose class which could be
    // used elsewhere, it may be cleaner to log in LOGGER_MISC
    //
    private static final ClassLogger logger = 
            new ClassLogger(ClassLogger.LOGGER_MISC,DaemonTaskServer.class);

    private final class Daemon implements Runnable {
        public void run() {
            DaemonTaskServer.this.run();
        }
    }

    private final Daemon daemon = new Daemon();

    // State Flags:
    // ------------

    // start => on=true ; stop => on=false
    private transient boolean on = false;

    // run() started  => isRunning=true
    // run() finished => isRunning=false
    private transient boolean isRunning = false;

    // terminate => terminating=true
    private transient boolean terminating = false; 

    // Operating data:
    // ---------------

    // The FIFO list of tasks
    private transient LinkedList list = new LinkedList();

    // The Daemon Thread that eats the list
    private transient Thread thread = null;

    // The sequence number used to sequence tasks.
    private transient long sequenceNumber = 0;

    // Used to name the daemon thread.
    private static int count = 0;


    // Access to State Flags:
    // ----------------------

    // Set isRunning flag to true. Called at the very beginning of the
    // run() method.
    private synchronized void running() {
        isRunning=true;
    }

    // Set isRunning flag to false. Called at the very end of the
    // run() method.
    private synchronized void stopped() {
        isRunning=false;
        thread = null;
    }

    // Return the value of the isRunning flag: tell whether the run()
    // method is in progress.
    private synchronized boolean isRunning() {
        return isRunning;
    }

    // Tell whether start() or stop() was called:
    // on=true  => start() was called
    // on=false => stop() was called, and the daemon thread will
    //             terminate at its next iteration.
    private synchronized boolean isOn() {
        return on;
    }

    /**
     * Implements the Task Server Thread loop. 
     **/
    void run() {
        try {
            // We are in the run()!
            running();
            if (logger.finerOn())
                logger.finer("run","Task Server Thread starting...");

            // Loop until stop() is called.
            while (isOn()) {
                try {
                    
                    // Get next notif, wait() if the list is empty.
                    final Task t = nextTask();

                    // If the notif is not null, send it.
                    if (t != null) {
                        execute(t);
                    }
                } catch (InterruptedException i) {
                    if (logger.finerOn())
                        logger.finer("run","Task Server Thread interrupted...");
                } catch (Throwable t) {
                    // Rather unexpected: log message, proceed with 
                    // next notif...
                    if (logger.fineOn()) {
                        logger.fine("run",
                        "Unexpected Error/Exception in Task Server thread: "+t+
                                "\n\t Error/Exception ignored.");
                    }
                    if (logger.debugOn()) {
                        logger.debug("run",t);
                    }
                }
            }
        } finally {
            // We are going out of the run()!
            try {
                if (logger.finerOn())
                    logger.finer("run","Task Server Thread exiting...");
            } catch (Throwable t) { }
            stopped();
        }
    }

    // Get next task, wait if necessary.
    private final synchronized Task nextTask() 
        throws InterruptedException {
        // Try to get next task.
        try {
            synchronized (list) {
                return (Task) list.removeFirst();
            }
        } catch (NoSuchElementException x) {
        }
        
        // No task available: let's wait...
        //trace.send("Waiting for tasks...");
        wait();

        // We have been awaken: try to get next task.
        try {
            synchronized (list) {
                return (Task) list.removeFirst();
            }
        } catch (NoSuchElementException x) {
            // No task? return null, perhaps we need to stop.
            // If not, we will wait again at next iteration.
            return null;
        }
    }

    /**
     * Execute the given task.
     * This method simply calls 
     * {@link com.sun.jdmk.tasks.Task#run() task.run()}. It is provided
     * as a customization hook for subclasses.
     * @param task The task to execute.
     * @exception InterruptedException if the task is interrupted.
     **/
    protected void execute(Task task) 
        throws InterruptedException {
        task.run();
    }

    /**
     * Cancel the given task.
     * This method simply calls 
     * {@link com.sun.jdmk.tasks.Task#cancel() task.cancel()}. 
     * It is provided as a customization hook for subclasses.
     * @param task The task to cancel.
     * @exception InterruptedException if cancel is interrupted.
     **/
    protected void cancel(Task task) 
        throws InterruptedException {
        task.cancel();
    }

    /**
     * Submit a task.
     * The submitted task is put inside a FIFO list, and the Task
     * Server Thread is waken up, if necessary. After that, the method 
     * returns. The task will be executed asynchronously by the
     * Task Server Thread.
     * @param task The Task to execute.
     **/
    public synchronized void submitTask(Task task) {
        // If we are terminating then we don't accept new tasks...
        if (terminating) throw new 
            IllegalStateException("Task Server Terminated");

        // How can we execute a null task? nothing to do: return.
        if (task == null) return;
        //logger.finest("Adding task: " + task);

        // Add task at the end of the FIFO.
        list.addLast(task);
        
        // Wake up the daemon thread.
        wakethread();
    }

    
    // Wake up the daemon thread.
    private final synchronized void wakethread() {
        this.notifyAll();
    }

    /**
     * Stop the Task Server Thread. Wait for the thread to die.
     * @exception InterruptedException if stop is interrupted.
     **/
    public void stop() throws InterruptedException {
        final Thread t;
        //trace.send("Stopping...");
        synchronized (this) {
            if (isOn() == false && thread == null) return;
            t = thread;
            on = false;
            if (thread != null) thread.interrupt();
        }
        if ((t != null) && (Thread.currentThread() != t))
            t.join();
        //trace.send("Stopping Done...");
    }

    // Tell if the FIFO is empty
    private boolean isListEmpty() {
        synchronized(list) {
            return list.isEmpty();
        }
    }

    /**
     * Stop the Task Server Thread. Wait for the thread to die.
     * Then flush the Task FIFO, canceling any remaining 
     * task. During this time, the object will reject any call
     * to {@link #submitTask(com.sun.jdmk.tasks.Task) submitTask()}.
     * When terminated, the object can not be started again, unless
     * {@link com.sun.jdmk.tasks.DaemonTaskServer#reset()} is called.
     * @exception InterruptedException if cancel is interrupted.
     **/
    public void terminate() throws InterruptedException {
        //trace.send("Terminating...");

        // make sure we won't accept new tasks
        synchronized(this) { terminating = true; }

        // stop the daemon thread
        stop();
        //trace.send("Flushing...");

        // cancel all remaining tasks
        synchronized(this) {
            while (!isListEmpty()) {
                try {
                    final Task t;
                    synchronized(list) {
                        t = (Task) list.removeFirst();
                    }
                    cancel(t);
                } catch (NoSuchElementException x) {
                    break;
                }
            }
        }
        //trace.send("Terminated...");
    }

    /**
     * Reset the DaemonTaskServer. 
     * This method makes it possible to reuse the DaemonTaskServer after a
     * {@link com.sun.jdmk.tasks.DaemonTaskServer#terminate()}
     * @exception IllegalStateException if the DaemonTaskServer is running.
     **/
    public synchronized void reset() {
        if (isOn() || isRunning()) throw new 
            IllegalStateException("Must be stopped in order to reset");
        synchronized (list) {
            list.clear();
        }
        terminating = false;    
    }

    /**
     * Start the Task Server Thread.
     * @exception IllegalStateException if the DaemonTaskServer is terminated.
     **/
    public void start() {
        start(Thread.NORM_PRIORITY);
    }

    /**
     * Start the Task Server Thread. Assign the specified priority to the
     * internal daemon thread.
     * @param threadPriority Priority of the daemon thread see 
     *        {@link java.lang.Thread}.
     * @exception IllegalStateException if the DaemonTaskServer is terminated.
     **/
    public void start(int threadPriority) {
        final Thread t;
        //trace.send("Starting...");
        synchronized (this) {
            if (terminating == true) throw new 
                IllegalStateException("Task Server Terminated");
            if (isOn() == true) return;
            on = true;
            thread = new Thread(daemon,"DaemonTaskServer("+ count++ + ")");
            t = thread;
            t.setDaemon(true);
            t.setPriority(threadPriority);
        }
        t.start();
        //trace.send("Starting Done...");
    }

}
