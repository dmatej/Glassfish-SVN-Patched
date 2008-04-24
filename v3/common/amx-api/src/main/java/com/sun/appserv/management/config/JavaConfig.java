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
package com.sun.appserv.management.config;

import com.sun.appserv.management.base.Container;
import com.sun.appserv.management.base.Singleton;
import com.sun.appserv.management.base.XTypes;

import java.util.Map;

/**
	 Configuration for the &lt;java-config&gt; element.
*/

public interface JavaConfig
	extends ConfigElement, PropertiesAccess, Container, Singleton
{
/** The j2eeType as returned by {@link com.sun.appserv.management.base.AMX#getJ2EEType}. */
	public static final String	J2EE_TYPE	= XTypes.JAVA_CONFIG;
	
	public String	getBytecodePreprocessors();
	public void	setBytecodePreprocessors( String value );

	public String	getClasspathPrefix();
	public void	setClasspathPrefix( String value );

	public String	getClasspathSuffix();
	public void	setClasspathSuffix( String value );

	public String	getDebugEnabled();
	public void	setDebugEnabled( String value );

	public String	getDebugOptions();
	public void	setDebugOptions( String value );

	public String	getEnvClasspathIgnored();
	public void	setEnvClasspathIgnored( String value );

	public String	getJavaHome();
	public void	setJavaHome( String value );

	public String	getJavacOptions();
	public void	setJavacOptions( String value );


    /**
        @since AppServer 9.0
     */
    public String   getSystemClasspath();
    
    /**
        @since AppServer 9.0
     */
    public void     setSystemClasspath( String classpath );
    
	public String[]	getJVMOptions();
	
	/**
        Add jvm options.  Using any of the {@link #CollectionOp} commands,
        options may be added, removed or replaced.  If the optioncal command
        is specified, it should be first element in the value array.  Otherwise,
        the command defaults to {@link ComponentOp#COLLECTION_OP_ADD}.
        <p>
        Alt
        <p>
        If a JVM option contains a space or tab, you must enclose
        it in quotes eg </code>"C:Program Files\dir"</code>
        
	 */
	public void	setJVMOptions( String[] value );

	public String	getNativeLibraryPathPrefix();
	public void	setNativeLibraryPathPrefix( String value );

	public String	getNativeLibraryPathSuffix();
	public void	setNativeLibraryPathSuffix( String value );

	public String	getRMICOptions();
	public void	setRMICOptions( String value );

	public String	getServerClasspath();
	public void	setServerClasspath( String value );


// -------------------- Operations --------------------
	/**
		Get the ProfilerConfig MBean.
	 */
	public ProfilerConfig	getProfilerConfig();

	/**
		Creates a profiler element.
		Although a name is specified, only one profiler may exist.

		@param name		identifier
		@param optional	Map of optional attributes.  See {@link ProfilerConfigKeys}
		@return A proxy to the ProfilerConfig MBean.
	 */
	public ProfilerConfig	createProfilerConfig( String name, Map<String,String> optional );

	/**
		Removes profiler element.  Even though only one can exist, the name must be specified.
	 */
	public void			removeProfilerConfig();
}
