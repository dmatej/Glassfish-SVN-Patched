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

package org.glassfish.embed.test;

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
public class Main extends com.sun.enterprise.module.bootstrap.Main {
    public static void main(String[] args) {
        try {
            new Main().boot(args);
            // TODO: show how to shut down v3 cleanly.
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(-1);
        }
    }

    public void boot(String[] args) throws IOException, BootException {
        final Module[] proxyMod = new Module[1];
        ModulesRegistryImpl mrs = new ModulesRegistryImpl(null) {
            public Module find(Class clazz) {
                Module m = super.find(clazz);
                if(m==null)
                    return proxyMod[0];
                return m;
            }
        };
        proxyMod[0] = mrs.add(new ProxyModuleDefinition(Main.class.getClassLoader()));

        StartupContext startupContext = new StartupContext(new File("./temp"), args);

        Habitat habitat = launch(mrs,startupContext);

        // deploy(new File("./simple.war"),habitat);
        // deploy(new File("./JSPWiki.war"),habitat);
        deploy(new File("./hudson.war"),habitat);
    }

    // TODO: refactoring of ApplicationLifecycle is crucial to make this code presentable
    private void deploy(File war, Habitat habitat) throws IOException {
        ApplicationLifecycle appLife = habitat.getComponent(ApplicationLifecycle.class);
        SnifferManager snifMan = habitat.getComponent(SnifferManager.class);
        ArchiveFactory archiveFactory = habitat.getComponent(ArchiveFactory.class);
        ServerEnvironment env = habitat.getComponent(ServerEnvironment.class);

        ReadableArchive a = archiveFactory.openArchive(war);
        ArchiveHandler h = appLife.getArchiveHandler(a);

        // explode
        File tmpDir = new File("./exploded");
        FileUtils.whack(tmpDir);
        tmpDir.mkdirs();
        h.expand(a, archiveFactory.createArchive(tmpDir));
        a.close();
        a = archiveFactory.openArchive(tmpDir);

        // now prepare sniffers
        ClassLoader parentCL = snifMan.createSnifferParentCL(null);
        ClassLoader cl = h.getClassLoader(parentCL, a);
        Collection<Sniffer> activeSniffers = snifMan.getSniffers(a, cl);


        // TODO: we need to stop this totally type-unsafe way of passing parameters
        Properties params = new Properties();
        params.put(DeployCommand.NAME,a.getName());
        params.put(DeployCommand.ENABLED,"true");
        final DeploymentContextImpl deploymentContext = new DeploymentContextImpl(Logger.getAnonymousLogger(), a, params, env);
        deploymentContext.setClassLoader(cl);

        PlainTextActionReporter r = new PlainTextActionReporter();
        ApplicationInfo appInfo = appLife.deploy(activeSniffers, deploymentContext, r);
        r.writeReport(System.out);

        // wait for enter
        new BufferedReader(new InputStreamReader(System.in)).readLine();

        // undeploy
        // TODO: API abstraction problem.
        r = new PlainTextActionReporter();
        appLife.undeploy(a.getName(),deploymentContext, r);
        r.writeReport(System.out);

        stopDomain(habitat);
    }

    private void stopDomain(Habitat habitat) {
        for (Inhabitant<? extends Startup> svc : habitat.getInhabitants(Startup.class)) {
            try {
                svc.release();
            } catch(Throwable e) {
                e.printStackTrace();
            }
        }

        for (Inhabitant<? extends Init> svc : habitat.getInhabitants(Init.class)) {
            try {
                svc.release();
            } catch(Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Tweaks the 'recipe' --- for embedded use, we'd like GFv3 to behave a little bit
     * differently from normal stand-alone use.
     */
    @Override
    protected InhabitantsParser createInhabitantsParser(Habitat habitat) {
        InhabitantsParser parser = super.createInhabitantsParser(habitat);

        // we don't want GFv3 to reconfigure all the loggers
        parser.drop(LogManagerService.class);

        // we don't need admin CLI support.
        // TODO: admin CLI should be really moved to a separate class
        parser.drop(AdminConsoleAdapter.class);

        // don't care about auto-deploy either
        parser.drop(AutoDeployService.class);

        // we don't really parse domain.xml from disk
        parser.replace(DomainXml.class, DomainXml2.class);

        // security code needs a whole lot more work to work in the modular environment.
        // disabling it for now.
        parser.drop(SecuritySniffer.class);

        // we provide our own ServerEnvironment
        parser.replace(ServerEnvironment.class,ServerEnvironment2.class);

        // WebContainer has a bug in how it looks up Realm, but this should work around that.
        parser.drop(RealmAdapter.class);

        // override the location of default-web.xml
        parser.replace(WebDeployer.class, WebDeployer2.class);

//        // override the location of cached DTDs and schemas
//        parser.replace(DigesterFactory.class, DigesterFactory2.class);
        parser.replace(WebEntityResolver.class, EntityResolverImpl.class);

        return parser;
    }
}
