/* 
 * @(#)file      Task.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.8
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
// file Task.java created for this bug fix
package com.sun.jdmk.tasks;

/**
 * This interface is implemented by objects that can be executed
 * by a {@link com.sun.jdmk.tasks.TaskServer}. 
 * <p>A <code>Task</code> object implements two methods:
 * <ul><li><code>public void run(): </code> from 
 *               {@link java.lang.Runnable}</li>
 * <ul>This method is called by the {@link com.sun.jdmk.tasks.TaskServer}
 *     when the task is executed.</ul>
 * <li><code>public void cancel(): </code></li>
 * <ul>This method is called by the {@link com.sun.jdmk.tasks.TaskServer}
 *     if the <code>TaskServer</code> is stopped before the 
 *     <code>Task</code> is executed.</ul>
 * </ul>
 * An implementation of {@link com.sun.jdmk.tasks.TaskServer} shall call
 * either <code>run()</code> or <code>cancel()</code>.
 * Whether the task is executed synchronously in the current
 * thread (when calling <code>TaskServer.submitTask()</code> or in a new
 * thread dedicated to the task, or in a daemon thread, depends on the
 * implementation of the <code>TaskServer</code> through which the task
 * is executed. 
 * <p>The implementation of <code>Task</code> must not make any
 * assumption on the implementation of the <code>TaskServer</code> through
 * which it will be executed.
 * @see com.sun.jdmk.tasks.TaskServer
 *
 * @since Java DMK 5.0
 **/
public interface Task extends Runnable {
    /**
     * Cancel the submitted task.
     * The implementation of this method is Task-implementation dependent.
     * It could involve some message logging, or even call the run() method.
     * Note that only one of run() or cancel() will be called - and exactly
     * one.
     **/
    public void cancel();
}
