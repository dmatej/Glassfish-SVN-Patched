/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - Bug 324357 - Employee example using JSF-EJB-JPA for 2.1.2 
 ******************************************************************************/
package eclipselink.example.jpa.employee.services.diagnostics;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.persistence.EntityManagerFactory;

import org.eclipse.persistence.internal.helper.Helper;
import org.eclipse.persistence.internal.jpa.EntityManagerImpl;
import org.eclipse.persistence.internal.sessions.IdentityMapAccessor;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.server.Server;
import org.eclipse.persistence.sessions.server.ServerSession;

import eclipselink.example.jpa.employee.services.EmployeeBaseService;

/**
 * 
 * @author dclarke
 * @since ElcipseLInk 2.1.2
 */
public class Diagnostics {

    private EmployeeBaseService service;

    public Diagnostics(EmployeeBaseService service) {
        this.service = service;
    }

    protected EmployeeBaseService getService() {
        return this.service;
    }

    /**
     * DIAGNOSTICS
     * 
     * Return the latest SQL strings captured by the diagnostics. If the
     * diagnostics have not been enabled then an empty collection is returned.
     */
    public Collection<String> getLatestSQL() {
        return SQLCaptureInterceptor.getLatestSQL(getServerSession());
    }


    /**
     * DIAGNOSTICS
     * 
     * Returns the shared server session sued within the
     * {@link EntityManagerFactory}. This is provided for use within diagnostics
     * portions of this example and should not generally be used in an
     * EclipseLink JPA application.
     */
    protected Server getServerSession() {
        EntityManagerImpl emImpl = (EntityManagerImpl) JpaHelper.getEntityManager(getService().getEntityManager());

        try {
        if (emImpl != null) {
            return emImpl.getServerSession();
        }

        throw new IllegalStateException("Could not access shared server session from: " + emImpl);
        } finally {
            emImpl.close();
        }
    }

    public List<CacheSize> getCacheSizes() {
        ServerSession session = (ServerSession) getServerSession();

        if (session != null) {
            List<CacheSize> sizes = new ArrayList<CacheSize>();
            
            @SuppressWarnings("unchecked")
            Iterator<Class<?>> classesI = session.getIdentityMapAccessorInstance().getIdentityMapManager().getIdentityMapClasses();
            
            while (classesI.hasNext()) {
                Class<?> entityClass = classesI.next();
                sizes.add(new CacheSize(Helper.getShortClassName(entityClass), ((IdentityMapAccessor) session.getIdentityMapAccessor()).getIdentityMap(entityClass).getSize()));
            }
            
            return sizes;
        }
        return null;
    }

    public String clearCaches() {
        Server session = getServerSession();

        if (session != null) {
            session.getIdentityMapAccessor().initializeAllIdentityMaps();
        }
        return null;
    }

}
