/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.glassfish.embed;

import com.sun.enterprise.web.WebDeployer;

import java.net.URL;
import java.io.IOException;

/**
 * @author Kohsuke Kawaguchi
 */
public class WebDeployer2 extends WebDeployer {
    protected URL getDefaultWebXML() throws IOException {
        return getClass().getClassLoader().getResource("default-web.xml");
    }
}
