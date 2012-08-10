/*******************************************************************************
 * Copyright (c) 2010-2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *  dclarke - EclipseLink 2.3 - MySports Demo Bug 344608
 ******************************************************************************/
package example.mysports.admin.jaxrs;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.PathSegment;

/**
 * TODO
 *  
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Stateless
@LocalBean
@Path("test")
public class TestJersey {

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String test() {
        return test(new ArrayList<PathSegment>());
    }
    
    @GET
    @Produces(MediaType.TEXT_PLAIN)
    @Path("{paths:.*}") 
    public String test(@PathParam("paths") List<PathSegment> paths) {
        StringWriter writer = new StringWriter();
        writer.write("Test\n");
        
        for (PathSegment segment: paths) {
            writer.write(segment.getPath() + "\n");
        }
        return writer.toString();
    }

}