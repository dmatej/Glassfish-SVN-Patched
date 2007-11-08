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
 
package com.sun.enterprise.management.config;

import com.sun.appserv.management.config.AMXConfig;
import com.sun.appserv.management.config.ConfigConfig;
import com.sun.appserv.management.config.ConnectorServiceConfig;

import com.sun.appserv.management.util.misc.ExceptionUtil;


import com.sun.enterprise.management.AMXTestBase;
import com.sun.enterprise.management.Capabilities;

/**
 */
public final class ConnectorServiceConfigTest extends AMXTestBase
{
    public ConnectorServiceConfigTest()
    {
	    if ( checkNotOffline( "ensureDefaultInstance" ) )
	    {
            ensureDefaultInstance( getConfigConfig() );
        }
    }
    
         public static ConnectorServiceConfig
	ensureDefaultInstance( final ConfigConfig configConfig )
	{
	    return configConfig.createConnectorServiceConfig();
	}
	
	    public synchronized void
	testCreateRemove()
	{
	    if ( checkNotOffline( "testCreateRemove" ) )
	    {
    	    final ConfigConfig  configConfig    = getConfigConfig();
    	    
    	    // remove first, in case it's there
    	    configConfig.removeConnectorServiceConfig();
    	    
    	    // create and remove
    	    configConfig.createConnectorServiceConfig();
    	    configConfig.removeConnectorServiceConfig();
    	    
    	    // leave it there
    	    ensureDefaultInstance( configConfig );
	    }
	}
	
	    public synchronized void
	testGetters()
	{
	    if ( checkNotOffline( "ensureDefaultInstance" ) )
	    {
	        ensureDefaultInstance( getConfigConfig() );
    	    
    	    final ConnectorServiceConfig    cs  = getConfigConfig().getConnectorServiceConfig();
    	    
    	    final String    value   = cs.getShutdownTimeoutInSeconds();
    	    assert( value != null );
    	    
    	    try
    	    {
    	        final String    newValue    = "" + (Integer.parseInt( value ) + 1);
        	    cs.setShutdownTimeoutInSeconds( newValue );
        	    final String after  = cs.getShutdownTimeoutInSeconds();
        	    assert( after.equals( newValue ) );
        	    
        	    cs.setShutdownTimeoutInSeconds( "30" );
        	    cs.setShutdownTimeoutInSeconds( newValue );
    	    }
    	    catch( Exception e )
    	    {
    	        warning( getRootCauseStackTrace( e ) );
    	        
    	        failure( "ConnectorServiceConfig.setShutdownTimeoutInSeconds FAILED " +
    	            "(bug #6307916 in com.sun.appserv:type=connector-service MBean)" );
    	    }
	    }
	}
}










