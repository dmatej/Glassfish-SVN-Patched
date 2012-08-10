package org.eclipse.jetty.jaxws2spi;

//========================================================================
//$$Id: JAXWS2ContextHandler.java 623 2008-01-03 21:51:42Z lorban $$
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

import com.sun.net.httpserver.Authenticator;
import com.sun.net.httpserver.Authenticator.Result;
import com.sun.net.httpserver.HttpContext;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpPrincipal;
import org.eclipse.jetty.server.HttpConnection;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.ContextHandler;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * Jetty handler that bridges requests to {@link HttpHandler}.
 * @author lorban
 */
@SuppressWarnings("restriction")
public class JAXWS2ContextHandler extends ContextHandler
{

    private HttpContext _httpContext;

    private HttpHandler _httpHandler;


    public JAXWS2ContextHandler(HttpContext httpContext, HttpHandler httpHandler)
    {
        this._httpContext = httpContext;
        this._httpHandler = httpHandler;
    }

    @Override
    public void doScope(String target, Request baseRequest, HttpServletRequest req, HttpServletResponse resp) throws IOException, ServletException
    {
        if (!target.startsWith(getContextPath())) return;

        JettyHttpExchange jettyHttpExchange = new JettyHttpExchange(_httpContext, req, resp);

        // TODO: add filters processing

        try
        {
            Authenticator auth = _httpContext.getAuthenticator();
	        if (auth != null)
	            handleAuthentication(resp, jettyHttpExchange, auth);
	        else
	        	_httpHandler.handle(jettyHttpExchange);
        }
        catch(Exception ex)
        {
        	PrintWriter writer = new PrintWriter(jettyHttpExchange.getResponseBody());
        	
        	resp.setStatus(500);
        	writer.println("<h2>HTTP ERROR: 500</h2>");
        	writer.println("<pre>INTERNAL_SERVER_ERROR</pre>");
        	writer.println("<p>RequestURI=" + req.getRequestURI() + "</p>");
        	
        	writer.println("<pre>");
			ex.printStackTrace(writer);
        	writer.println("</pre>");
        	
        	writer.println("<p><i><small><a href=\"http://jetty.mortbay.org\">Powered by jetty://</a></small></i></p>");
        	
        	writer.close();
        }
        finally
        {
			Request base_request = (req instanceof Request) ? (Request)req:HttpConnection.getCurrentConnection().getRequest();
			base_request.setHandled(true);
        }
        
    }


	private void handleAuthentication(HttpServletResponse resp, JettyHttpExchange jettyHttpExchange, Authenticator auth) throws IOException
	{
		Result result = auth.authenticate(jettyHttpExchange);
        if (result instanceof Authenticator.Failure)
        {
        	int rc = ((Authenticator.Failure)result).getResponseCode();
        	resp.sendError(rc);
        }
        else if (result instanceof Authenticator.Retry)
        {
        	int rc = ((Authenticator.Retry)result).getResponseCode();
        	resp.sendError(rc);
        }
        else if (result instanceof Authenticator.Success)
        {
        	HttpPrincipal principal = ((Authenticator.Success)result).getPrincipal();
        	jettyHttpExchange.setPrincipal(principal);
    		_httpHandler.handle(jettyHttpExchange);
        }
	}

    public HttpHandler getHttpHandler()
    {
        return _httpHandler;
    }

    public void setHttpHandler(HttpHandler handler)
    {
        this._httpHandler = handler;
    }

}
