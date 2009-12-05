package org.jboss.interceptor.util.proxy;

import javassist.util.proxy.MethodHandler;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public interface TargetInstanceProxy<T>
{
   T getTargetInstance();

   Class<? extends T> getTargetClass();
}
