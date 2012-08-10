package org.eclipse.jetty.jaxws2spi;

//========================================================================
//$$Id: JettyHttpServerProvider.java 626 2008-01-03 22:30:30Z lorban $$
//
//------------------------------------------------------------------------
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at 
//http://www.apache.org/licenses/LICENSE-2.0
//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
//limitations under the License.
//========================================================================


import java.io.IOException;
import java.net.InetSocketAddress;

import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.DefaultHandler;
import org.eclipse.jetty.server.handler.HandlerCollection;
import org.eclipse.jetty.server.handler.ContextHandlerCollection;

import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpsServer;
import com.sun.net.httpserver.spi.HttpServerProvider;

/**
 * Jetty implementation of <a href="http://java.sun.com/javase/6/docs/jre/api/net/httpserver/spec/index.html">Java HTTP Server SPI</a>
 * @author lorban
 */
@SuppressWarnings("restriction")
public class JettyHttpServerProvider extends HttpServerProvider
{

    public JettyHttpServerProvider() {
		super();
	}

	private static Server _server;

    public static void setServer(Server server)
    {
    	_server = server;
    }

    @Override
    public HttpServer createHttpServer(InetSocketAddress addr, int backlog)
            throws IOException
    {
        Server server = _server;
    	boolean shared = true;

        if (server == null)
        {
        	server = new Server();
        	//System.out.println(server);
        	
        	HandlerCollection handlerCollection = new HandlerCollection();
        	handlerCollection.setHandlers(new Handler[] {new ContextHandlerCollection(), new DefaultHandler()});
			server.setHandler(handlerCollection);

            shared = false;
        }

        JettyHttpServer jettyHttpServer = new JettyHttpServer(server, shared);
        jettyHttpServer.bind(addr, backlog);
        return jettyHttpServer;
    }

    @Override
    public HttpsServer createHttpsServer(InetSocketAddress addr, int backlog) throws IOException
    {
        throw new UnsupportedOperationException();
    }

}
