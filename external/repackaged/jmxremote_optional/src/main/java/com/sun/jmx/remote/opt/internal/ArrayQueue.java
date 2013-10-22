/*
 * @(#)ArrayQueue.java	1.3
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

import java.util.AbstractList;
import java.util.Iterator;

public class ArrayQueue extends AbstractList {
    public ArrayQueue(int capacity) {
	this.capacity = capacity + 1;
	this.queue = new Object[capacity + 1];
	this.head = 0;
	this.tail = 0;
    }

    public void resize(int newcapacity) {
	int size = size();
	if (newcapacity < size)
	    throw new IndexOutOfBoundsException("Resizing would lose data");
	newcapacity++;
	if (newcapacity == this.capacity)
	    return;
	Object[] newqueue = new Object[newcapacity];
	for (int i = 0; i < size; i++)
	    newqueue[i] = get(i);
	this.capacity = newcapacity;
	this.queue = newqueue;
	this.head = 0;
	this.tail = size;
    }

    public boolean add(Object o) {
	queue[tail] = o;
	int newtail = (tail + 1) % capacity;
	if (newtail == head)
	    throw new IndexOutOfBoundsException("Queue full");
	tail = newtail;
	return true; // we did add something
    }

    public Object remove(int i) {
	if (i != 0)
	    throw new IllegalArgumentException("Can only remove head of queue");
	if (head == tail)
	    throw new IndexOutOfBoundsException("Queue empty");
	Object removed = queue[head];
	queue[head] = null;
	head = (head + 1) % capacity;
	return removed;
    }

    public Object get(int i) {
	int size = size();
	if (i < 0 || i >= size) {
	    final String msg = "Index " + i + ", queue size " + size;
	    throw new IndexOutOfBoundsException(msg);
	}
	int index = (head + i) % capacity;
	return queue[index];
    }

    public int size() {
	// Can't use % here because it's not mod: -3 % 2 is -1, not +1.
	int diff = tail - head;
	if (diff < 0)
	    diff += capacity;
	return diff;
    }

    private int capacity;
    private Object[] queue;
    private int head;
    private int tail;
}
