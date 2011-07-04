package org.glassfish.fighterfish.test.app18;

import org.glassfish.osgicdi.OSGiService;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.osgi.framework.ServiceReference;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.DependsOn;
import javax.ejb.EJB;
import javax.ejb.LocalBean;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.inject.Inject;

/**
 * Session Bean implementation class ServiceListenerEJB
 */
@Singleton
@Startup
@DependsOn("EjbLifecycleObserverEJB")
public class ServiceListenerEJB {

    @Inject BundleContext bundleCtx;
    
    @Inject 
    MyServiceListener listener;
    
    @PostConstruct
    public void installListener() {
        String filter = "(" + Constants.OBJECTCLASS + "=" + Foo.class.getName() + ")";
        try {
            bundleCtx.addServiceListener(listener, filter);
        } catch (InvalidSyntaxException e) {
            e.printStackTrace();
        }
    }

    @PreDestroy
    public void uninstallListener() {
        bundleCtx.removeServiceListener(listener);
        System.out.println("ServiceListenerEJB.uninstallListener() " + "Removed service listener " + listener);
    }

    public static class MyServiceListener implements ServiceListener 
    {
        @Inject @OSGiService(dynamic=true)
        private EjbLifecycleObserver observer;
        public synchronized void serviceChanged(ServiceEvent event) {
            System.out.println(getClass().getName() + ".serviceChanged() " + event);
            switch (event.getType()) {
                case ServiceEvent.REGISTERED:
                    observer.registered(Foo.class.getName());
                    break;
            }
        }
    }

}
