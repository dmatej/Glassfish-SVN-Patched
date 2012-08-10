package org.eclipse.persistence.example.distributed.collatz.resource;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Consumes;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriInfo;
import javax.ws.rs.core.UriBuilder;
import java.net.URI;


@Path ("items")
@Produces (MediaType.APPLICATION_XML)

// http://jersey.java.net/use/getting-started.html
public class CollatzResource extends Application {

 // The Java method will process HTTP GET requests
     @GET 
     @Produces("text/plain")
      public String getMessage() {
               return "Collatz Sequence";
           }
    
}
