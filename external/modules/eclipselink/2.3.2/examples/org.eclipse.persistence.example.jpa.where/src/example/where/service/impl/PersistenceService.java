package example.where.service.impl;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

/**
 * 
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public abstract class PersistenceService {

    public static final String PU_NAME = "geonames";

    private EntityManagerFactory emf;

    private ServiceFactory serviceFactory;

    protected PersistenceService(ServiceFactory factory) {
        this.serviceFactory = factory;
    }

    protected ServiceFactory getServiceFactory() {
        return this.serviceFactory;
    }

    protected EntityManagerFactory getEMF() {
        if (this.emf == null) {
            this.emf = createEMF(null, true, getServiceFactory(), true);
        }

        return this.emf;
    }

    public static EntityManagerFactory createEMF(Map properties, boolean addSEProperties, ServiceFactory serviceFactory, boolean logSQL) {
        Map props = properties == null ? new HashMap() : properties;

        if (addSEProperties) {
            props.putAll(serviceFactory.getSEProperties(logSQL));
        }

        return Persistence.createEntityManagerFactory(PU_NAME, props);
    }

}
