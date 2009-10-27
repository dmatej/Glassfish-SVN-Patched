/*
 * @(#)file      CompatibleClassLoaderRepositorySupport.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.4
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
 *
 */
package com.sun.jdmk;


// JMX import
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.management.InstanceNotFoundException;
import javax.management.loading.ClassLoaderRepository;


/**
 * This class wraps a MBeanServer and implements the 
 * {@link ModifiableClassLoaderRepository} interface.
 * Addition and removal of ClassLoaders is not supported in this 
 * implementation: a {@link java.lang.UnsupportedOperationException}
 * is thrown.
 * <p>The {@link ModifiableClassLoaderRepository} interface is implemented
 * for backward compatibility with Java DMK 5.0. However, client code which
 * relied on the <code>addClassLoader(...) / removeClassLoader(...)</code>
 * APIs will no longer operate properly.
 *
 * @since Java DMK 5.1
 */
class CompatibleClassLoaderRepositorySupport 
    implements ModifiableClassLoaderRepository {

    private final MBeanServer inner;
    private final static String unsupported =
	"Operation not supported in this implementation";

    /**
     * Emulate a ModifiableClassLoaderRepository by wrapping a 
     * {@link javax.management.MBeanServer}.
     *
     * @since Java DMK 5.1
     **/
    public CompatibleClassLoaderRepositorySupport(MBeanServer inner) {
	if (inner == null) throw new
	    IllegalArgumentException("inner MBeanServer is null");
	this.inner = inner;
    }

    /**
     * <b>Not supported</b>, unless the underlying MBeanServer is able
     * to return a {@link ModifiableClassLoaderRepository} which supports it.
     * @exception UnsupportedOperationException if unsupported.
     **/
    public void addClassLoader(ClassLoader loader) {
	final ClassLoaderRepository clr = inner.getClassLoaderRepository();
	if (clr instanceof ModifiableClassLoaderRepository)
	     ((ModifiableClassLoaderRepository)clr).addClassLoader(loader);
	else throw new UnsupportedOperationException(unsupported);
    }

    /**
     * <b>Not supported</b>, unless the underlying MBeanServer is able
     * to return a {@link ModifiableClassLoaderRepository} which supports it.
     * @exception UnsupportedOperationException if unsupported.
     **/
    public void removeClassLoader(ClassLoader loader) {
	final ClassLoaderRepository clr = inner.getClassLoaderRepository();
	if (clr instanceof ModifiableClassLoaderRepository)
	    ((ModifiableClassLoaderRepository)clr).removeClassLoader(loader);
	else throw new UnsupportedOperationException(unsupported);
    }


    /**
     * <b>Not supported</b>, unless the underlying MBeanServer is able
     * to return a {@link ModifiableClassLoaderRepository} which supports it.
     * @exception UnsupportedOperationException if unsupported.
     **/
    public void addClassLoader(ObjectName name, ClassLoader loader) {
	final ClassLoaderRepository clr = inner.getClassLoaderRepository();
	if (clr instanceof ModifiableClassLoaderRepository)
	     ((ModifiableClassLoaderRepository)clr).
		 addClassLoader(name,loader);
	else throw new UnsupportedOperationException(unsupported);
    }

    /**
     * <b>Not supported</b>, unless the underlying MBeanServer is able
     * to return a {@link ModifiableClassLoaderRepository} which supports it.
     * @exception UnsupportedOperationException if unsupported.
     **/
    public void removeClassLoader(ObjectName name) {
	final ClassLoaderRepository clr = inner.getClassLoaderRepository();
	if (clr instanceof ModifiableClassLoaderRepository)
	     ((ModifiableClassLoaderRepository)clr).removeClassLoader(name);
	else throw new UnsupportedOperationException(unsupported);
    }

    /**
     * Get a named ClassLoader from the repository.
     **/
    public ClassLoader getClassLoader(ObjectName name) {
	try { return inner.getClassLoader(name); } 
	catch (InstanceNotFoundException e) { return null; }
    }

    public final Class loadClass(String className) 
	throws ClassNotFoundException {
	final ClassLoaderRepository clr = inner.getClassLoaderRepository();
	if (clr == null) 
	    throw new UnsupportedOperationException(unsupported);
	return  clr.loadClass(className);
    }

    
    public final Class loadClassWithout(ClassLoader loader,String className) 
	throws ClassNotFoundException {	
	final ClassLoaderRepository clr = inner.getClassLoaderRepository();
	if (clr == null) 
	    throw new UnsupportedOperationException(unsupported);
	return clr.loadClassWithout(loader,className);
    }

    public final Class loadClassBefore(ClassLoader loader,String className) 
	throws ClassNotFoundException {	
	final ClassLoaderRepository clr = inner.getClassLoaderRepository();
	if (clr == null) 
	    throw new UnsupportedOperationException(unsupported);
	return clr.loadClassBefore(loader,className);
    }
}
