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
package com.sun.enterprise.management;

import javax.management.MBeanServerInvocationHandler;
import javax.management.ObjectName;

import java.util.Set;
import java.util.Map;

import com.sun.appserv.management.base.AMX;
import com.sun.appserv.management.base.Util;
import com.sun.appserv.management.util.misc.ExceptionUtil;

import com.sun.appserv.management.config.DomainConfig;
import com.sun.appserv.management.config.ConfigConfig;
import com.sun.appserv.management.config.JavaConfig;
import com.sun.appserv.management.config.SecurityServiceConfig;
import com.sun.appserv.management.config.AdminServiceConfig;

import com.sun.appserv.management.util.misc.GSetUtil;
import com.sun.appserv.management.util.misc.ExceptionUtil;

import com.sun.enterprise.management.config.*;
import com.sun.enterprise.management.support.AMXDebugStuff;
import com.sun.enterprise.management.support.AMXDebugSupportMBean;


/**
    This test run prior to testing any AMX MBeans.
 */
public final class RunMeFirstTest extends AMXTestBase
{
		public
	RunMeFirstTest( )
	{
	    initCoverageInfos();
	}
	
	    private void
	initCoverageInfos()
	{
	    final Set<AMX>  all = getAllAMX();
	    
	    // set the AMX-DEBUG flags on
	    final String AMX_DEBUG  = "-DAMX-DEBUG.enabled=true";
	    final String AMX_DEBUG2  = "-DAMX-DEBUG=true";
	    
	    // set AMX-DEBUG.enabled=true in all ConfigConfig JVM options
	    final Map<String,ConfigConfig> configs = getDomainConfig().getConfigConfigMap();
	    for( final ConfigConfig config : configs.values() )
	    {
	        final JavaConfig jc = config.getJavaConfig();
	        final Set<String>   jvmOptions  = GSetUtil.newStringSet( jc.getJVMOptions() );
	        
	        if ( ! ( jvmOptions.contains( AMX_DEBUG ) || jvmOptions.contains( AMX_DEBUG2 ) ))
	        {
	            jvmOptions.add( AMX_DEBUG );
	            jc.setJVMOptions( GSetUtil.toStringArray( jvmOptions ) );
	            
	            // don't warn for default-config; it's not used by a running server
	            if ( ! config.getName().equals( "default-config" ) )
	            {
    	            warning( "Enabled AMX-DEBUG for config " + config.getName()+
    	                " (restart required)" );
	            }
	        }
	    }
	    
	    // setup default stuff
	    final AMXDebugSupportMBean debug    = getAMXDebugSupportMBean();
	    debug.setAll( true );
	    debug.setDefaultDebug( true );
	    debug.getOutputIDs();
	    
	    for( final AMX amx : all )
	    {
	        final AMXDebugStuff debugStuff  = getTestUtil().asAMXDebugStuff( amx );
	        
	        if ( debugStuff == null )
	        {
	            continue;
	        }
	        
	        try
	        {
                debugStuff.enableAMXDebug( true );
            }
            catch( Throwable t )
            {
                warning( "Couldn't enableAMXDebug() for " + amx.getJ2EEType() );
            }
            
	        try
	        {
                debugStuff.enableCoverageInfo( true );
                debugStuff.clearCoverageInfo();
            }
            catch( Throwable t )
            {
                warning( "Couldn't enableCoverageInfo for " + amx.getJ2EEType() );
            }
	    }
	}
}














