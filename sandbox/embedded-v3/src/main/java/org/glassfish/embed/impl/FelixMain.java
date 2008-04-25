/*
 * Copyright (c) 2008, Kohsuke Kawaguchi. All Rights Reserved.
 */

package org.glassfish.embed.impl;

import org.apache.felix.framework.Felix;
import org.apache.felix.framework.cache.BundleCache;
import org.apache.felix.framework.util.StringMap;
import org.osgi.framework.BundleActivator;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import com.sun.enterprise.module.bootstrap.Which;
import com.sun.enterprise.v3.server.AppServerStartup;

/**
 * Experimental launch code that starts Felix.
 *
 * Abandoned for now as it requires a multi-jar set up, making it unsuitable for
 * the embedded use.
 *
 * @author Kohsuke Kawaguchi
 */
@Deprecated
public class FelixMain {
    public static void main(String[] args) throws Exception {
        Properties felixProps = new Properties();

        File f = new File("./felix.cache"); // TODO: I first want to sketch the whole thing
        f.mkdirs();
        felixProps.setProperty(BundleCache.CACHE_PROFILE_DIR_PROP,f.getAbsolutePath());

        // let the system properties override our defaults
        felixProps.putAll(System.getProperties());

        System.out.println(Which.jarFile(AppServerStartup.class));

        // Create an instance of the framework.
        List<BundleActivator> initialActivators = new ArrayList<BundleActivator>();
        initialActivators.add(new MainActivator());
        Felix felix = new Felix(new StringMap(felixProps, false), initialActivators);
        felix.start();
    }
}
