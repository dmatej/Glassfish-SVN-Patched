package org.glassfish.fighterfish.sample.osgihttp.helloworld;

import javax.servlet.ServletException;

import org.osgi.service.component.ComponentContext;
import org.osgi.service.http.HttpContext;
import org.osgi.service.http.HttpService;
import org.osgi.service.http.NamespaceException;

/**
 * This class is an SCR component. It registers servlets and resources in activate() method
 * and unregisters them in deactivate() method. It consumes a service of type HttpService. The 
 * service reference is bound and unbound in setHttp() and unsetHttp() method respectively as
 * specified in scr.xml file.
 * 
 * @author sanjeeb.sahoo@oracle.com
 *
 */
public class Main {
	private HttpService http; // Set and unset by setHttp() and unsetHttp() methods that are called by SCR. See scr.xml

	protected void activate(ComponentContext ctx) throws ServletException, NamespaceException {
		// Create an HttpContext and use it for all web resource registration so that they all
		// share the same ServletContext. Note: one HttpContext maps o one ServletContext.
		HttpContext httpCtx = http.createDefaultHttpContext();
		http.registerServlet("/hello", new HelloWorldServlet(), null, httpCtx);
		// add another instance of the servlet as /hello2 to demonstrate that ServletContext is indeed
		// shared by all the servlet instances.
		http.registerServlet("/hello2", new HelloWorldServlet(), null, httpCtx);
		// Map index.jsp to foo.jsp
		http.registerResources("/index.jsp", "/foo.jsp", httpCtx);
	}

	protected void deactivate(ComponentContext ctx) {
		try {
			http.unregister("/hello");
			http.unregister("/hello2");
			http.unregister("/index.jsp");
		} catch (Exception e) {
			// This can happen if the HttpService has been undpeloyed in which case as part of its undepoyment,
			// it would have unregistered all aliases. So, we should protect against such a case.
			System.out.println(e);
		}
	}
	
	protected void setHttp(HttpService http) {
		this.http = http;
	}
	
	protected void unsetHttp(HttpService http) {
		this.http = null;
	}
}
