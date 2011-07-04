package org.glassfish.fighterfish.test.app18;

import org.glassfish.osgicdi.OSGiService;
import org.glassfish.osgicdi.ServiceUnavailableException;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.service.log.LogService;

import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.inject.Inject;

import java.util.Dictionary;
import java.util.Properties;

/**
 * Session Bean implementation class EjbLifecycleObserverEJB
 */
@Singleton
@Startup
public class EjbLifecycleObserverEJB implements EjbLifecycleObserver {

    @Inject
    @OSGiService(dynamic = true)
    EventAdmin eventAdmin;
    
    /* (non-Javadoc)
     * @see org.glassfish.fighterfish.test.app18.EjbLifecycleObserver#installService(java.lang.String)
     */
    @Override
    public void registered(String serviceName) {
        String message = "EjbLifecycleObserverEJB.registered() " + serviceName;
        System.out.println(message);
        try {
            Dictionary eventProps = new Properties();
            eventProps.put("eventType", "REGISTERED");
            eventProps.put("serviceName", serviceName);
            Event event = new Event("org/glassfish/fighterfist/test/app18", eventProps);
            eventAdmin.sendEvent(event);
        } catch (ServiceUnavailableException e) {
            System.out.println("EjbLifecycleObserverEJB.registered() " + e);
        }
    }

}
