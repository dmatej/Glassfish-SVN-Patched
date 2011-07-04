/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2010 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
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

package org.glassfish.osgicdi.impl;

import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.enterprise.context.Dependent;
import javax.enterprise.context.spi.CreationalContext;
import javax.enterprise.event.Observes;
import javax.enterprise.inject.Any;
import javax.enterprise.inject.CreationException;
import javax.enterprise.inject.Default;
import javax.enterprise.inject.spi.AfterBeanDiscovery;
import javax.enterprise.inject.spi.Bean;
import javax.enterprise.inject.spi.BeforeBeanDiscovery;
import javax.enterprise.inject.spi.Extension;
import javax.enterprise.inject.spi.InjectionPoint;
import javax.enterprise.inject.spi.ProcessAnnotatedType;
import javax.enterprise.inject.spi.ProcessBean;
import javax.enterprise.inject.spi.ProcessInjectionTarget;
import javax.enterprise.util.AnnotationLiteral;

import org.osgi.framework.ServiceException;

import org.glassfish.osgicdi.OSGiService;
import org.glassfish.osgicdi.ServiceUnavailableException;

/**
 * A portable extension that supports discovery and injection of OSGi
 * services from an OSGi service registry into Beans/Java EE components
 * that support injection. 
 * 
 * @see OSGiService
 * @author Sivakumar Thyagarajan
 */
public class OSGiServiceExtension implements Extension{

    /*
     * A map of Framework Service Types to be injected and additional metadata
     * about the OSGiService to be injected.
     */
    private HashMap<Type, Set<InjectionPoint>> servicesToBeInjected
                                = new HashMap<Type, Set<InjectionPoint>>();
    private static Logger logger = Logger.getLogger(OSGiServiceExtension.class.getPackage().getName());

    //Observers for container lifecycle events
    void beforeBeanDiscovery(@Observes BeforeBeanDiscovery bdd){
        debug("beforeBeanDiscovery" + bdd);
        bdd.addQualifier(OSGiService.class); //XXX:needed?
    }

    /**
     * Observer for <code>ProcessInjectionTarget</code> events. This event is
     * fired for every Java EE component class supporting injection that may be 
     * instantiated by the container at runtime. Injections points of every 
     * discovered enabled Java EE component is checked to see if there is a 
     * request for injection of a framework service. 
     */
    void afterProcessInjectionTarget(@Observes ProcessInjectionTarget<?> pb){
        debug("AfterProcessInjectionTarget" + pb.getAnnotatedType().getBaseType());
        Set<InjectionPoint> ips = pb.getInjectionTarget().getInjectionPoints();
        discoverServiceInjectionPoints(ips);
    }
    
    /**
     * Observer for <code>ProcessBean</code> events. This event is
     * fired fire an event for each enabled bean, interceptor or decorator 
     * deployed in a bean archive, before registering the Bean object. 
     * Injections points of every discovered enabled Java EE component is 
     * checked to see if there is a request for injection of a framework 
     * service. 
     */
    void afterProcessBean(@Observes ProcessBean pb){
        debug("afterProcessBean - " + pb.getAnnotated().getBaseType());
        Set<InjectionPoint> ips = pb.getBean().getInjectionPoints();
        discoverServiceInjectionPoints(ips);
    }

    /*
     * Discover injection points where the framework service is requested
     * through the <code>OSGiService</code> qualifier and a map is 
     * populated for all framework services that have been requested.
     */
    private void discoverServiceInjectionPoints(Set<InjectionPoint> ips) {
        for (Iterator<InjectionPoint> iterator = ips.iterator(); 
                                                    iterator.hasNext();) {
            InjectionPoint injectionPoint = iterator.next();
            Set<Annotation> qualifs = injectionPoint.getQualifiers();
            for (Iterator<Annotation> qualifIter = qualifs.iterator(); 
                                                    qualifIter.hasNext();) {
                Annotation annotation = qualifIter.next();
                if (annotation.annotationType().equals(OSGiService.class)){
                    printDebugForInjectionPoint(injectionPoint);
                    String s = "---- Injection requested for " +
                            "framework service type " + injectionPoint.getType()
                            + " and annotated with dynamic="
                            + injectionPoint.getAnnotated()
                                    .getAnnotation(OSGiService.class)
                                    .dynamic()
                            + ", serviceCriteria="
                            + injectionPoint.getAnnotated()
                                    .getAnnotation(OSGiService.class)
                                    .serviceCriteria();
                    logger.logp(Level.INFO, "OSGiServiceExtension", "discoverServiceInjectionPoints", s);
                    //Keep track of service-type and its injection point
                    //Add to list of framework services to be injected
                    addServiceInjectionInfo(injectionPoint);
                    debug("number of injection points for " 
                            + injectionPoint.getType() + "=" 
                            + servicesToBeInjected.size());
                    
                }
            }
        }
    }

    private void addServiceInjectionInfo(InjectionPoint injectionPoint) {
        Type key = injectionPoint.getType();
        if (!servicesToBeInjected.containsKey(key)){
            servicesToBeInjected.put(key, new CopyOnWriteArraySet<InjectionPoint>());
        }
        servicesToBeInjected.get(key).add(injectionPoint);
    }

    /**
     * Observer for <code>AfterBeanDiscovery</code> events. This 
     * observer method is used to register <code>Bean</code>s for the framework
     * services that have been requested to be injected. 
     */
    void afterBeanDiscovery(@Observes AfterBeanDiscovery abd){
        debug("After Bean Discovery");
        for (Iterator<Type> iterator = this.servicesToBeInjected.keySet().iterator(); 
                                                iterator.hasNext();) {
            Type type =  iterator.next();
            //If the injection point's type is not a Class or Interface, we
            //don't know how to handle this. 
            if (!(type instanceof Class)) {
                //XXX: need to handle Instance<Class>. This fails currently
                logger.logp(Level.WARNING, "OSGiServiceExtension", "afterBeanDiscovery",
                        "Unknown type: {0}", new Object[]{type});
                abd.addDefinitionError(new UnsupportedOperationException(
                        "Injection target type " + type + "not supported"));
                break; //abort deployment
            }
            //Add the Bean representing the framework service so that it
            //is available for injection
            addBean(abd, type, this.servicesToBeInjected.get(type));
        }
    }

    /*
     * Add a <code>Bean</code> for the framework service requested. Instantiate
     * or discover the bean from the framework service registry, 
     * and return a reference to the service if a dynamic reference is requested.
     */
    private void addBean(AfterBeanDiscovery abd, final Type type, 
            final Set<InjectionPoint> injectionPoints) {
        List<OSGiService> registeredBeans = new ArrayList<OSGiService>();
        for (Iterator<InjectionPoint> iterator = injectionPoints.iterator(); iterator
                .hasNext();) {
            final InjectionPoint svcInjectionPoint = iterator.next();
            if (!registeredBeans.contains(svcInjectionPoint.getAnnotated().getAnnotation(OSGiService.class))) {
                debug(" --- Adding an OSGi service BEAN " 
                        + type + " for " + svcInjectionPoint);
                OSGiService os = svcInjectionPoint.getAnnotated().getAnnotation(OSGiService.class);
                if (!os.dynamic()) {
                    //If Static, check for existence of Service before going 
                    //ahead and adding a Bean.
                    //If a service that matches the requirements specified
                    //is unavailable, fail deployment by throwing
                    //a <code>ServiceUnavailableException</code>
                    try {
                        OSGiServiceFactory.checkServiceAvailability(svcInjectionPoint);
                    } catch (ServiceUnavailableException sue) {
                        sue.printStackTrace();
                        throw new ServiceUnavailableException("A static OSGi service " +
                        		"reference was requested in " + 
                        		svcInjectionPoint + ". However no "  + 
                        		svcInjectionPoint.getType() 
                        		+ " service available", 
                                ServiceException.SUBCLASSED, sue);
                    }
                }
                abd.addBean(new OSGiServiceBean(svcInjectionPoint));
                registeredBeans.add(svcInjectionPoint.getAnnotated().getAnnotation(OSGiService.class));
            } else {
                debug(" --- NOT Adding an OSGi service BEAN " 
                        + type + " for " + svcInjectionPoint 
                        + "as there has already been one registered for" 
                        + svcInjectionPoint.getAnnotated().getAnnotation(OSGiService.class));
                
            }
        }
    }
    

    /*
     * A <code>Bean</code> class that represents an OSGi Service 
     */
    private final class OSGiServiceBean implements Bean {
        private final Type type;
        private final InjectionPoint svcInjectionPoint;
        private final OSGiService os;


        private OSGiServiceBean(InjectionPoint injectionPoint) {
            this.svcInjectionPoint = injectionPoint;
            this.type = this.svcInjectionPoint.getType();
            this.os = this.svcInjectionPoint.getAnnotated().getAnnotation(OSGiService.class);
        }

        @Override
        public Object create(CreationalContext ctx) {
            debug("create::" + type);
            //get the service from the service registry
            try {
                return OSGiServiceFactory.getService(this.svcInjectionPoint);
            } catch (ServiceUnavailableException e) {
                e.printStackTrace();
                throw new CreationException(e);
            }
        }

        @Override
        public void destroy(Object instance,
                CreationalContext creationalContext) {
          //debug("destroy::" + instance);
          //unget the service reference
          OSGiServiceFactory.ungetService(instance, this.svcInjectionPoint);
        }

        @Override
        public Class getBeanClass() {
            return (Class)type;
        }

        @Override
        public Set<InjectionPoint> getInjectionPoints() {
          return Collections.emptySet();
        }

        @Override
        public String getName() {
            return type + getServiceMetadata();
        }
        
        private String getServiceMetadata() {
            return "_dynamic_" + os.dynamic() + 
                           "_criteria_" + os.serviceCriteria() 
                           + "_waitTimeout" + os.waitTimeout();
        }

        @Override
        public Set<Annotation> getQualifiers() {
            Set<Annotation> s = new HashSet<Annotation>();
            s.add(new AnnotationLiteral<Default>() {});
            s.add(new AnnotationLiteral<Any>() {});
            //Add the appropriate parameters to the OSGiService qualifier
            //as requested in the injection point
            s.add(new OSGiServiceQualifierType(this.os)); 
            return s;
        }

        @Override
        public Class<? extends Annotation> getScope() {
            return Dependent.class; 
            //Similar to Java EE comp resources made available as Dependent only
            //we now allow OSGi services as Dependent Beans only. 
        }

        @Override
        public Set<Class<? extends Annotation>> getStereotypes() {
            return Collections.emptySet();
        }

        @Override
        public Set<Type> getTypes() {
            Set<Type> s = new HashSet<Type>();
            s.add(type); 
            s.add(Object.class);
            return s;
        }

        @Override
        public boolean isAlternative() {
            return false;
        }

        @Override
        public boolean isNullable() {
            return false;
        }
    }

    /*
     * Represents an annotation type instance of OSGiService
     * with parameters equal to those specified in the injection point
     */
    private final class OSGiServiceQualifierType 
                            extends AnnotationLiteral<OSGiService> 
                            implements OSGiService {
        private String serviceCriteria = "";
        private boolean dynamic = false;
        private int waitTimeout = -1;

        public OSGiServiceQualifierType(OSGiService os){
            this.serviceCriteria = os.serviceCriteria();
            this.dynamic = os.dynamic();
            this.waitTimeout  = os.waitTimeout();
        }
        @Override
        public String serviceCriteria(){
            return this.serviceCriteria;
        }

        @Override
        public boolean dynamic() {
            return this.dynamic;
        }

        @Override
        public int waitTimeout() {
            return this.waitTimeout;
        }
    }
    
    private void debug(String string) {
        logger.logp(Level.FINE, "OSGiServiceExtension", "debug", getClass().getSimpleName() + ":: {0}", new Object[]{string});
    }

    private void printDebugForInjectionPoint(InjectionPoint injectionPoint) {
        if (logger.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder();
            sb.append("@@@@@@@ INJECTION-POINT: Annotation:"
                    + injectionPoint.getAnnotated()); // annotatedfield
            sb.append(" ,Bean:" + injectionPoint.getBean());// bean
            sb.append(" ,Class:" + injectionPoint.getClass()); // r untime
                                                           // class?
            sb.append(" ,Member:" + injectionPoint.getMember());// Field
            sb.append(" ,Qualifiers:" + injectionPoint.getQualifiers());// qualifiers
            sb.append(" ,Type:" + injectionPoint.getType()); // type of injection point
            logger.logp(Level.FINE, "OSGiServiceExtension", "printDebugForInjectionPoint",
                    getClass().getSimpleName() + ":: {0}", new Object[]{sb});
        }
    }
    
}
