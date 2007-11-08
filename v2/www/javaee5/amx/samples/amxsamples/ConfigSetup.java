/*
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt or 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License.
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt.  If applicable, 
 * add the following below this CDDL HEADER, with the 
 * fields enclosed by brackets "[]" replaced with your 
 * own identifying information: Portions Copyright [yyyy] 
 * [name of copyright owner]
 */
package amxsamples;

import java.util.Map;
import java.util.HashMap;

import com.sun.appserv.management.DomainRoot;

import com.sun.appserv.management.config.ConfigConfig;
import com.sun.appserv.management.config.StandaloneServerConfig;
import com.sun.appserv.management.config.DomainConfig;
import com.sun.appserv.management.config.ServerConfigKeys;


/**
 */
public final class ConfigSetup
{
    final DomainRoot    mDomainRoot;
    
    public static final String  TEST_SERVER_NAME    = "testServer";
    public static final String  TEST_CONFIG_NAME    = TEST_SERVER_NAME + "-config";
    
        public
    ConfigSetup( final DomainRoot domainRoot )
    {
        mDomainRoot = domainRoot;
    }
    
        public DomainConfig
    getDomainConfig()
    {
        return mDomainRoot.getDomainConfig();
    }
    
        public ConfigConfig
    createConfig( final String name)
    {
        final Map<String,String>    options = new HashMap<String,String>();
        
        final ConfigConfig  config =
            getDomainConfig().createConfigConfig( name, options );
            
        return config;
    }
    
        public boolean
    removeConfig( final String name)
    {
       boolean exists = getDomainConfig().getConfigConfigMap().get( name ) != null;
        
        if ( exists )
        {
            getDomainConfig().removeConfigConfig( name );
        }
        
       return exists;
    }
    
        public void
    setupServerPorts(
        final Map<String,String> options,
        final int   basePort )
    {
        options.put( ServerConfigKeys.HTTP_LISTENER_1_PORT_KEY, "" + (basePort + 0) );
        options.put( ServerConfigKeys.HTTP_LISTENER_2_PORT_KEY, "" + (basePort + 1) );
        options.put( ServerConfigKeys.ORB_LISTENER_1_PORT_KEY, "" + (basePort + 2) );
        options.put( ServerConfigKeys.SSL_PORT_KEY, "" + (basePort + 3) );
        options.put( ServerConfigKeys.SSL_MUTUALAUTH_PORT_KEY, "" + (basePort + 4) );
        options.put( ServerConfigKeys.JMX_SYSTEM_CONNECTOR_PORT_KEY, "" + (basePort + 5) );
        options.put( ServerConfigKeys.JMS_PROVIDER_PORT_KEY, "" + (basePort + 6) );
        options.put( ServerConfigKeys.ADMIN_LISTENER_PORT_KEY, "" + (basePort + 7) );
    }
    
        public StandaloneServerConfig
    createServer(
        final String    name,
        int             basePort,
        final String    nodeAgentName,
        final String    configName )
    {
        final Map<String,String>    options = new HashMap<String,String>();
        
        setupServerPorts( options, basePort );
        
        final StandaloneServerConfig  server =
            getDomainConfig().createStandaloneServerConfig(
                name, nodeAgentName, configName, options );
            
        return server;
    }
    

        public boolean
    removeServer( final String name )
    {
        boolean exists = getDomainConfig().getStandaloneServerConfigMap().get( name ) != null;

        if ( exists )
        {
            getDomainConfig().removeStandaloneServerConfig( name );
        }

        return exists;
    }
}






