package org.glassfish.fighterfish.test.app18;
import javax.ejb.Local;

@Local
public interface EjbLifecycleObserver {
    void registered(String serviceName);
}
