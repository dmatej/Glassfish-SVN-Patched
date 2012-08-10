package example.where.service.impl;

import java.util.Map;

import example.where.service.LocationService;
import example.where.service.UserService;

public abstract class ServiceFactory {

    private static ServiceFactory singleton;

    private static final String PROPERTY = "where.service-factory";

    public static ServiceFactory getSingleton() {

        if (singleton == null) {
            synchronized (PROPERTY) {
                if (singleton == null) {
                    String serviceProperty = System.getProperty(PROPERTY);

                    // Use EclipseLink by default
                    if (serviceProperty == null) {
                        singleton = new EclipseLinkServiceFactory();
                    } else {
                        try {
                            Class serviceFactoryClass = Class.forName(serviceProperty);

                            singleton = (ServiceFactory) serviceFactoryClass.newInstance();
                        } catch (Exception e) {
                            throw new IllegalArgumentException("ServiceFactory.getSinglton() failed for: " + serviceProperty);
                        }
                    }
                }
            }
        }

        return singleton;
    }

    public UserService createUserService() {
        return new UserServiceImpl(this);
    }

    public LocationService createLocationService() {
        return new LocationServiceImpl(this);
    }

    protected abstract Map getSEProperties(boolean logSQL);
}
