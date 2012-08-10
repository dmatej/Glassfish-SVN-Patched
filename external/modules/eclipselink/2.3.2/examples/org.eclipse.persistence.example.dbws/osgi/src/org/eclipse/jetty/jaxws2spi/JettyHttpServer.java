package org.eclipse.jetty.jaxws2spi;

//========================================================================
//$$Id: JettyHttpServer.java 626 2008-01-03 22:30:30Z lorban $$
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

import com.sun.net.httpserver.HttpContext;
import com.sun.net.httpserver.HttpHandler;
import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.ContextHandler;
import org.eclipse.jetty.server.handler.HandlerCollection;
import org.eclipse.jetty.server.handler.ContextHandlerCollection;
import org.eclipse.jetty.server.nio.SelectChannelConnector;
import org.eclipse.jetty.util.log.Log;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * Jetty implementation of {@link com.sun.net.httpserver.HttpServer}.
 * @author lorban
 */
@SuppressWarnings("restriction")
public class JettyHttpServer extends com.sun.net.httpserver.HttpServer
{

    private Server _server;
    
    private boolean _serverShared;

    private InetSocketAddress _addr;

    private ThreadPoolExecutor _executor;

    private Map<String, JettyHttpContext> _contexts = new HashMap<String, JettyHttpContext>();
    
    private Map<String, Connector> _connectors = new HashMap<String, Connector>();

    
    public JettyHttpServer(Server server, boolean shared)
    {
        this._server = server;
        this._serverShared = shared;
    }

    @Override
    public void bind(InetSocketAddress addr, int backlog) throws IOException
    {
    	// check if there is already a connector listening
        Connector[] connectors = _server.getConnectors();
        if (connectors != null)
        {
            for (Connector connector : connectors)
            {
                if (connector.getPort() == addr.getPort()) {
                    if (Log.isDebugEnabled()) Log.debug("server already bound to port " + addr.getPort() + ", no need to rebind");
                    return;
                }
            }
        }
        
        if (_serverShared)
        	throw new IOException("jetty server is not bound to port " + addr.getPort());
        
        this._addr = addr;

        if (Log.isDebugEnabled()) Log.debug("binding server to port " + addr.getPort());
        SelectChannelConnector connector = new SelectChannelConnector();
        connector.setAcceptors(1);
        connector.setPort(addr.getPort());
        connector.setHost(addr.getHostName());
        _server.addConnector(connector);
        
        _connectors.put(addr.getHostName() + addr.getPort(), connector);
    }

    @Override
    public InetSocketAddress getAddress()
    {
        return _addr;
    }

    @Override
    public void start()
    {
    	if (_serverShared) return;
    	
        try
        {
            _server.start();
        }
        catch (Exception ex)
        {
            throw new RuntimeException(ex);
        }
    }

    @Override
    public void setExecutor(Executor executor)
    {
        if (executor == null)
            throw new IllegalArgumentException("missing required 'executor' argument");

    	if (!(executor instanceof ThreadPoolExecutor))
    		throw new IllegalArgumentException("only java.util.concurrent.ThreadPoolExecutor instances are allowed, got: " + executor.getClass().getName());
    	
    	if (Log.isDebugEnabled()) Log.debug("using ThreadPoolExecutor for server thread pool");
    	this._executor = (ThreadPoolExecutor) executor;
    	_server.setThreadPool(new ThreadPoolExecutorAdapter(_executor));
    }

    @Override
    public Executor getExecutor()
    {
        return _executor;
    }

    @Override
    public void stop(int delay)
    {
    	cleanUpContexts();
    	cleanUpConnectors();
    	
    	if (_serverShared) return;

    	try
        {
            _server.stop();
        }
        catch (Exception ex)
        {
            throw new RuntimeException(ex);
        }
    }

	private void cleanUpContexts()
	{
        for (Map.Entry<String, JettyHttpContext> stringJettyHttpContextEntry : _contexts.entrySet())
        {
            JettyHttpContext context = stringJettyHttpContextEntry.getValue();
            _server.removeBean(context.getJettyContextHandler());
        }
		_contexts.clear();
	}

    private void cleanUpConnectors()
    {
        for (Map.Entry<String, Connector> stringConnectorEntry : _connectors.entrySet())
        {
            Connector connector = stringConnectorEntry.getValue();
            try
            {
                connector.stop();
            } catch (Exception ex) {
                Log.warn(ex);
            }
            _server.removeConnector(connector);
        }
		_connectors.clear();
	}

	@Override
    public HttpContext createContext(String path, HttpHandler httpHandler)
    {
    	checkIfContextIsFree(path);

        JettyHttpContext context = new JettyHttpContext(this, path, httpHandler);
        JAXWS2ContextHandler jettyContextHandler = context.getJettyContextHandler();

        ContextHandlerCollection chc = findContextHandlerCollection(_server.getHandlers());
        if (chc == null)
        	throw new RuntimeException("could not find ContextHandlerCollection, you must configure one");

        chc.addHandler(jettyContextHandler);
        _contexts.put(path, context);

        return context;
    }

    private ContextHandlerCollection findContextHandlerCollection(Handler[] handlers)
    {
        if (handlers == null)
            return null;

        for (Handler handler : handlers)
        {
            if (handler instanceof ContextHandlerCollection)
            {
                return (ContextHandlerCollection) handler;
            }

            if (handler instanceof HandlerCollection)
            {
                HandlerCollection hc = (HandlerCollection) handler;
                ContextHandlerCollection chc = findContextHandlerCollection(hc.getHandlers());
                if (chc != null)
                    return chc;
            }
        }
        return null;
    }

    private void checkIfContextIsFree(String path)
    {
    	Handler serverHandler = _server.getHandler();
		if (serverHandler instanceof ContextHandler)
		{
			ContextHandler ctx = (ContextHandler) serverHandler;
			if (ctx.getContextPath().equals(path))
	        	throw new RuntimeException("another context already bound to path " + path);
		}
    	
    	Handler[] handlers = _server.getHandlers();
    	if (handlers == null) return;

        for (Handler handler : handlers)
        {
            if (handler instanceof ContextHandler) {
                ContextHandler ctx = (ContextHandler) handler;
                if (ctx.getContextPath().equals(path))
                    throw new RuntimeException("another context already bound to path " + path);
            }
        }
	}

	@Override
    public HttpContext createContext(String path)
    {
        return createContext(path, null);
    }

    @Override
    public void removeContext(String path) throws IllegalArgumentException
    {
        JettyHttpContext context = _contexts.remove(path);
        if (context == null) return;
        _server.removeBean(context.getJettyContextHandler());
    }

    @Override
    public void removeContext(HttpContext context)
    {
        removeContext(context.getPath());
    }

}
