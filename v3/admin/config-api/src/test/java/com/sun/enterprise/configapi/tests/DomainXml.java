package com.sun.enterprise.configapi.tests;


import com.sun.enterprise.module.bootstrap.Populator;
import com.sun.enterprise.module.bootstrap.StartupContext;
import org.jvnet.hk2.annotations.Inject;
import org.jvnet.hk2.annotations.Service;
import org.jvnet.hk2.config.ConfigParser;
import org.jvnet.hk2.config.DomDocument;

/**
 * @author Jerome Dochez
 */
@Service
public class DomainXml implements Populator {

    @Inject
    StartupContext context;
    
    public void run(ConfigParser parser) {
        long now = System.currentTimeMillis();
        System.out.println("Startupcontext = " + context.getRootDirectory());
        DomDocument document = parser.parse(getClass().getClassLoader().getResource("domain.xml"));
        System.out.println("time to parse domain.xml : " + String.valueOf(System.currentTimeMillis() - now));
    }
}
