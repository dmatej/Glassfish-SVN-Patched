/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 *
 */

package org.glassfish.embed;

import com.sun.enterprise.module.Module;
import com.sun.enterprise.module.bootstrap.BootException;
import com.sun.enterprise.module.bootstrap.StartupContext;
import com.sun.enterprise.module.impl.ModulesRegistryImpl;
import com.sun.enterprise.v3.admin.adapter.AdminConsoleAdapter;
import com.sun.enterprise.v3.server.DomainXml;
import com.sun.enterprise.v3.server.ServerEnvironment;
import com.sun.enterprise.v3.server.ApplicationLifecycle;
import com.sun.enterprise.v3.server.SnifferManager;
import com.sun.enterprise.v3.services.impl.LogManagerService;
import com.sun.enterprise.v3.deployment.DeploymentContextImpl;
import com.sun.enterprise.v3.deployment.DeployCommand;
import com.sun.enterprise.v3.data.ApplicationInfo;
import com.sun.enterprise.v3.common.PlainTextActionReporter;
import com.sun.enterprise.deploy.shared.ArchiveFactory;
import com.sun.enterprise.util.io.FileUtils;
import com.sun.enterprise.security.SecuritySniffer;
import com.sun.enterprise.web.WebDeployer;
import com.sun.hk2.component.InhabitantsParser;
import com.sun.web.security.RealmAdapter;
import org.glassfish.deployment.autodeploy.AutoDeployService;
import org.glassfish.api.deployment.archive.ReadableArchive;
import org.glassfish.api.deployment.archive.ArchiveHandler;
import org.glassfish.api.container.Sniffer;
import org.glassfish.api.Startup;
import org.glassfish.internal.api.Init;
import org.glassfish.web.WebEntityResolver;
import org.glassfish.embed.impl.ProxyModuleDefinition;
import org.glassfish.embed.impl.DomainXml2;
import org.glassfish.embed.impl.ServerEnvironment2;
import org.glassfish.embed.impl.WebDeployer2;
import org.glassfish.embed.impl.EntityResolverImpl;
import org.jvnet.hk2.component.Habitat;
import org.jvnet.hk2.component.Inhabitant;

import java.io.File;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.Properties;
import java.util.logging.Logger;

/**
 * Launches a mock-up HK2 environment that doesn't provide
 * any classloader isolation. Instead, the whole thing is loaded
 * from the single classloader.
 *
 * @author Kohsuke Kawaguchi
 */
public class Main {
    public static void main(String[] args) throws Exception {
        GlassFish glassfish = new GlassFish();
        GFHttpListener listener = glassfish.createHttpListener(8080);
        GFVirtualServer vs = glassfish.createVirtualServer(listener);

        // deploy(new File("./simple.war"),habitat);
        // deploy(new File("./JSPWiki.war"),habitat);
        GFApplication app = glassfish.deploy(new File("./hudson.war"));

        // wait for enter
        new BufferedReader(new InputStreamReader(System.in)).readLine();

        app.undeploy();
        glassfish.stop();
    }
}
