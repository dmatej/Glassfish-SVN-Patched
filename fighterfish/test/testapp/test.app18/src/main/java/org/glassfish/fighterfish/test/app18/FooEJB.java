package org.glassfish.fighterfish.test.app18;

import javax.ejb.DependsOn;
import javax.ejb.Singleton;
import javax.ejb.Stateless;

/**
 * Session Bean implementation class FooEJB
 */
@Singleton
@DependsOn("ServiceListenerEJB")
public class FooEJB implements Foo {
    public void bar() {}
}
