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

import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.ext.ContextResolver;
import javax.ws.rs.ext.Provider;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.eclipse.persistence.jaxb.xmlmodel.XmlBindings;

/**
 * {@link ContextResolver} to handle using EclipseLink MOXy as the JAXB provider
 * to convert objects into XML when returned from the JAX-RS methods.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Provider
@Produces(MediaType.APPLICATION_XML)
public class XmlBindingsContextResolver implements ContextResolver<JAXBContext> {

    private JAXBContext jc;

    public XmlBindingsContextResolver() {
        try {
            this.jc = JAXBContext.newInstance("org.eclipse.persistence.jaxb.xmlmodel");
        } catch (JAXBException e) {
            throw new RuntimeException("XmlBindingsContextResolver<init> failed: " + e.getMessage(), e);
        }
    }

    @Override
    public JAXBContext getContext(Class<?> clazz) {
        if (XmlBindings.class == clazz) {
            return jc;
        }
        return null;
    }
}