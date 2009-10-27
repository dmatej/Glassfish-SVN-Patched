/* 
 * @(#)file      ClassLoaderRepositorySupport.java 
 * @(#)author    Sun Microsystems, Inc. 
 * @(#)version   1.14 
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

package com.sun.jdmk;


// JMX import
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.management.InstanceNotFoundException;
import javax.management.loading.ClassLoaderRepository;
import javax.management.loading.PrivateClassLoader;

import java.util.Vector;
import java.util.Hashtable;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

import com.sun.jdmk.internal.ClassLogger;

/**
 * This class keeps the list of Class Loaders registered in the MBean Server.
 * It provides the necessary methods to load classes using the 
 * registered Class Loaders. 
 * This class is provided uniquely for compatibility with Java DMK 5.0.
 *
 * @deprecated Use {@link MBeanServer#getClassLoaderRepository()} instead.
 *
 * @since Java DMK 5.0
 */
public final class ClassLoaderRepositorySupport 
    implements ModifiableClassLoaderRepository {

    /* We associate an optional ObjectName with each entry so that
       we can remove the correct entry when unregistering an MBean
       that is a ClassLoader.  The same object could be registered
       under two different names (even though this is not recommended)
       so if we did not do this we could disturb the defined
       semantics for the order of ClassLoaders in the repository.  */
    private static class LoaderEntry {
	ObjectName name; // can be null
	ClassLoader loader;

	LoaderEntry(ObjectName name,  ClassLoader loader) {
	    this.name = name;
	    this.loader = loader;
	}
    }

    private static final LoaderEntry[] EMPTY_LOADER_ARRAY = new LoaderEntry[0];

    /**
     * List of class loaders
     * Only read-only actions should be performed on this object.
     *
     * We do O(n) operations on this array, e.g. when removing
     * a ClassLoader.  The assumption is that the number of elements
     * is small, probably less than ten, and that the vast majority
     * of operations are searches (loadClass) which are by definition
     * linear.
     */ 
    private LoaderEntry[] loaders = EMPTY_LOADER_ARRAY;

    /**
     * Same behavior as {@link java.util.List#add(Object)}.
     * Replace the loader list with a new one in which the new
     * loader has been added.
     **/
    private synchronized boolean add(ObjectName name, ClassLoader cl) {
	List l = new ArrayList(Arrays.asList(loaders));
	l.add(new LoaderEntry(name, cl));
	loaders = (LoaderEntry[]) l.toArray(EMPTY_LOADER_ARRAY);
	return true;
    }

    /**
     * Same behavior as {@link java.util.List#remove(Object)}.
     * Replace the loader list with a new one in which the old loader
     * has been removed.
     *
     * The ObjectName may be null, in which case the entry to
     * be removed must also have a null ObjectName and the ClassLoader
     * values must match.  If the ObjectName is not null, then
     * the first entry with a matching ObjectName is removed,
     * regardless of whether ClassLoader values match.  (In fact,
     * the ClassLoader parameter will usually be null in this case.)
     **/
    private synchronized boolean remove(ObjectName name, ClassLoader cl) {
	final int size = loaders.length;
	for (int i = 0; i < size; i++) {
	    LoaderEntry entry = loaders[i];
	    boolean match =
		(name == null) ?
		cl == entry.loader :
		name.equals(entry.name);
	    if (match) {
		LoaderEntry[] newloaders = new LoaderEntry[size - 1];
		System.arraycopy(loaders, 0, newloaders, 0, i);
		System.arraycopy(loaders, i + 1, newloaders, i,
				 size - 1 - i);
		loaders = newloaders;
		return true;
	    }
	}
	return false;
    }


    /**
     * List of valid search
     */
    private final Hashtable search= new Hashtable(10);

    /**
     * List of named class loaders.
     */
    private final Hashtable loadersWithNames = new Hashtable(10);

    
    private final static String dbgTag = "ClassLoaderRepositorySupport";


    // from javax.management.loading.DefaultLoaderRepository
    public final Class loadClass(String className) 
	throws ClassNotFoundException {
	return  loadClass(loaders, className, null, null);
    }

    
    // from javax.management.loading.DefaultLoaderRepository
    public final Class loadClassWithout(ClassLoader without, String className) 
	    throws ClassNotFoundException {	
	if (logger.finerOn()) {
	    logger.finer("loadClassWithout", className + 
			 "\twithout " + without);
	} 

	// without is null => just behave as loadClass
	//
	if (without == null)
	    return loadClass(loaders, className, null, null);
	
	// We must try to load the class without the given loader.
	//
	startValidSearch(without, className);
	try {
	    return loadClass(loaders, className, without, null);
	} finally {
	    stopValidSearch(without, className);
	}
    }


    public final Class loadClassBefore(ClassLoader stop, String className)
	    throws ClassNotFoundException {
	if (logger.finerOn())
	    logger.finer("loadClassBefore", className + "\tbefore " + stop);

	if (stop == null)
	    return loadClass(loaders, className, null, null);

	startValidSearch(stop, className);
	try {
	    return loadClass(loaders, className, null, stop);
	} finally {
	    stopValidSearch(stop, className);
	}
    }


    private Class loadClass(final LoaderEntry list[], 
			    final String className,
			    final ClassLoader without,
			    final ClassLoader stop) 
	    throws ClassNotFoundException {
	final int size = list.length;
        for(int i=0; i<size; i++) {
	    try {
		final ClassLoader cl = list[i].loader;
		if (cl == null) // bootstrap class loader
		    return Class.forName(className, false, null);
		if (cl == without)
		    continue;
		if (cl == stop)
		    break;
		if (logger.finerOn()) {
		    logger.finer("loadClass", "trying loader = " + cl);
		}
		/* We used to have a special case for "instanceof
		   MLet" here, where we invoked the method
		   loadClass(className, null) to prevent infinite
		   recursion.  But the rule whereby the MLet only
		   consults loaders that precede it in the CLR (via
		   loadClassBefore) means that the recursion can't
		   happen, and the test here caused some legitimate
		   classloading to fail.  For example, if you have
		   dependencies C->D->E with loaders {E D C} in the
		   CLR in that order, you would expect to be able to
		   load C.  The problem is that while resolving D, CLR
		   delegation is disabled, so it can't find E.  */
		return Class.forName(className, false, cl);
            } catch (ClassNotFoundException e) {
		// OK: continue with next class
	    }
        }

        throw new ClassNotFoundException(className);
    }

    private synchronized void startValidSearch(ClassLoader aloader,
					       String className) 
        throws ClassNotFoundException {
        // Check if we have such a current search
        //
        Vector excluded= (Vector) search.get(className);
        if ((excluded!= null) && (excluded.contains(aloader))) {
	    if (logger.finerOn()) {
		logger.finer("startValidSearch", "already requested loader=" + 
		      aloader + " class= " + className);
	    }            
            throw new ClassNotFoundException(className);
        }
      
        // Add an entry
        //
        if (excluded == null) {
            excluded= new Vector(1);
            search.put(className, excluded);
        }         
        excluded.addElement(aloader);
	if (logger.finerOn()) {
	    logger.finer("startValidSearch", "loader=" + aloader + " class= " + 
		  className);
	}     
    }

    private synchronized void stopValidSearch(ClassLoader aloader,
					      String className) {
    
        // Retrieve the search.
        //
        Vector excluded= (Vector) search.get(className);
        if (excluded!= null) {
            excluded.removeElement(aloader);
	    if (logger.finerOn()) {
		logger.finer("stopValidSearch", "loader=" + aloader + 
		      " class= " + className);
	    }               
	}
    }
  
    public final void addClassLoader(ClassLoader loader) {
	add(null, loader);
    }

    public final void removeClassLoader(ClassLoader loader) {
	remove(null, loader);
    }

    public final synchronized void addClassLoader(ObjectName name, 
						  ClassLoader loader) {
	loadersWithNames.put(name, loader);
	if (!(loader instanceof PrivateClassLoader))
	    add(name, loader);
    }

    public final synchronized void removeClassLoader(ObjectName name) {
	ClassLoader loader = (ClassLoader) loadersWithNames.remove(name);
	if (!(loader instanceof PrivateClassLoader))
	    remove(name, loader);
    }

    public final ClassLoader getClassLoader(ObjectName name) {
	return (ClassLoader)loadersWithNames.get(name);
    }

    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_MBEANSERVER,
		        dbgTag);

}
