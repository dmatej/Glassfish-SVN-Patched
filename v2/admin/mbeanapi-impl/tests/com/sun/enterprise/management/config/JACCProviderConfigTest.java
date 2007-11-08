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

import java.util.Map;

import javax.management.ObjectName;

import com.sun.appserv.management.util.jmx.MBeanServerConnectionConnectionSource;
import com.sun.appserv.management.util.misc.ExceptionUtil;

import com.sun.appserv.management.base.AMX;
import com.sun.appserv.management.base.Container;
import com.sun.appserv.management.base.XTypes;

import com.sun.appserv.management.config.AMXConfig;
import com.sun.appserv.management.config.SecurityServiceConfig;
import com.sun.appserv.management.config.JACCProviderConfig;


import com.sun.enterprise.management.AMXTestBase;
import com.sun.enterprise.management.Capabilities;

/**
 */
public final class JACCProviderConfigTest extends ConfigMgrTestBase
{
	static final String PROVIDER			= "com.sun.enterprise.security.provider.PolicyWrapper";
	static final String	PROVIDER_FACTORY	= "com.sun.enterprise.security.provider.PolicyConfigurationFactoryImpl";
	static final Map<String,String>	RESERVED			= null;

		public
	JACCProviderConfigTest()
	{
	    if ( checkNotOffline( "ensureDefaultInstance" ) )
	    {
	        ensureDefaultInstance( getConfigConfig().getSecurityServiceConfig() );
	    }
	}
	     public static String
    getDefaultInstanceName()
    {
        return getDefaultInstanceName( "JACCProviderConfig" );
    }
    
	    public static JACCProviderConfig
	ensureDefaultInstance( final SecurityServiceConfig ss )
	{
	    JACCProviderConfig result = ss.getJACCProviderConfigMap().get( getDefaultInstanceName() );
	    
	    if ( result == null )
	    {
	        result  = createInstance( ss, getDefaultInstanceName(), PROVIDER, PROVIDER_FACTORY );
	    }
	    
	    return result;
	}
	
	    public static JACCProviderConfig
	createInstance(
	    final SecurityServiceConfig ss,
	    final String    name,
	    final String    policyProvider,
	    final String    policyConfigurationFactoryProvider )
	{
	    return ss.createJACCProviderConfig( name,
	        policyProvider, policyConfigurationFactoryProvider, null );
	}
	
	
		protected Container
	getProgenyContainer()
	{
		return getConfigConfig().getSecurityServiceConfig();
	}

		protected String
	getProgenyJ2EEType()
	{
		return XTypes.JACC_PROVIDER_CONFIG;
	}


		protected void
	removeProgeny( final String name )
	{
		getConfigConfig().getSecurityServiceConfig().removeJACCProviderConfig( name );
	}

		protected final AMXConfig
	createProgeny(final String name, final Map<String,String> options )
	{
		return getConfigConfig().getSecurityServiceConfig().createJACCProviderConfig(name, PROVIDER, PROVIDER_FACTORY, options);
	}
}


