package com.sun.enterprise.configapi.tests;

import com.sun.enterprise.module.bootstrap.ModuleStartup;
import com.sun.enterprise.module.bootstrap.StartupContext;
import org.jvnet.hk2.annotations.Inject;
import org.jvnet.hk2.annotations.Service;

import com.sun.enterprise.config.serverbeans.*;

/**
 * Created by IntelliJ IDEA.
 * User: dochez
 * Date: Sep 12, 2007
 * Time: 4:33:43 PM
 * To change this template use File | Settings | File Templates.
 */
@Service
public class Main implements ModuleStartup {
    @Inject
    Domain domain;

    @Inject(name="http-listener-1")
    HttpListener listener;

    @Inject
    public void setStartupContext(StartupContext startupContext) {

    }

    public void run() {
        System.out.println("listener =" + listener.getPort());
        for (Property prop : listener.getProperty()) {
            System.out.println("prop = " + prop);
        }
    }
}
