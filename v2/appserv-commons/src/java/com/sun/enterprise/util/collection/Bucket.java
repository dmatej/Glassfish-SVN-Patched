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

//NOTE: Tabs are used instead of spaces for indentation. 
//  Make sure that your editor does not replace tabs with spaces. 
//  Set the tab length using your favourite editor to your 
//  visual preference.

/*
 * Filename: Bucket.java	
 *
 * Copyright 2000-2001 by iPlanet/Sun Microsystems, Inc.,
 * 901 San Antonio Road, Palo Alto, California, 94303, U.S.A.
 * All rights reserved.
 * 
 * This software is the confidential and proprietary information
 * of iPlanet/Sun Microsystems, Inc. ("Confidential Information").
 * You shall not disclose such Confidential Information and shall
 * use it only in accordance with the terms of the license 
 * agreement you entered into with iPlanet/Sun Microsystems.
 */
 
/**
 * <BR> <I>$Source: /cvs/glassfish/appserv-commons/src/java/com/sun/enterprise/util/collection/Bucket.java,v $</I>
 * @author     $Author: tcfujii $
 * @version    $Revision: 1.3 $ $Date: 2005/12/25 04:12:10 $
 */
 
package com.sun.enterprise.util.collection;

import java.util.Iterator;
	
/**
 * A bucket is esentially a list of entries that can be accessed through a primitive typed key.
 * Buckets are used in building primitive typed HashMaps. See IntHashMap for more details.
 * 
 */
public interface Bucket {
	
	/**
	 * Put an object into the bucket using the key. The key can be used
	 *	to retrieve the object later.
	 * @param key The key (can be negative).
	 * @param object The object to be put in the bucket.
	 * @return Any old object that was associated using the same key or null otherwise.
	 */
	public Object put(int key, Object object);
	public Object put(long key, Object object);
	
	/**
	 * Get an object into the bucket using the key.
	 * @param key The key (can be negative) for retrieving object.
	 * @return The object that was associated using the same key or null otherwise.
	 */
	public Object get(int key);
	public Object get(long key);
	
	/**
	 * Remove an object from the bucket using the key.
	 * @param key The key (can be negative).
	 * @return The object that was associated using the same key or null otherwise.
	 */
	public Object remove(int key);
	public Object remove(long key);
	
	/**
	 * Check if the bucket contains the key.
	 * @param key The key (can be negative).
	 * @return true if the bucket contains the key or null otherwise.
	 */
	public boolean containsKey(int key);
	public boolean containsKey(long key);
	
	
	/**
	 * Return the size of the bucket.
	 * @return The number of items in the bucket
	 */
	public int size();
	
	/**
	 * Get an iterator for iteraing through all objects in the bucket.
	 * @return An iterator. iterator.next() returns an object.
	 */
	public Iterator iterator();
	
	/**
	 * Get an iterator for iteraing through all entries in the bucket.
	 * @return An iterator. iterator.next() returns an IntEntry.
	 */
	public Iterator entryIterator();
	
}
