package com.sun.enterprise.v3.data;

import org.jvnet.hk2.annotations.Scoped;
import org.jvnet.hk2.annotations.Service;
import org.jvnet.hk2.component.Singleton;

import java.util.HashMap;
import java.util.Map;

/**
 * Registry for deployed Applications
 */
@Service
@Scoped(Singleton.class)
public class ApplicationRegistry {

    private Map<String, ApplicationInfo> apps = new HashMap<String, ApplicationInfo>();

    public void add(String name, ApplicationInfo info) {
        apps.put(name, info);
    }

    public ApplicationInfo get(String name) {
        return apps.get(name);
    }

    public void remove(String name) {
        apps.remove(name);
    }

}
