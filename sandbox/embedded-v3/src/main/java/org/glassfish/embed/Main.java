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

import com.sun.enterprise.module.bootstrap.StartupContext;
import com.sun.enterprise.module.bootstrap.BootException;
import com.sun.enterprise.module.impl.ModulesRegistryImpl;
import com.sun.enterprise.module.ModulesRegistry;
import com.sun.enterprise.module.Module;
import com.sun.enterprise.v3.server.DomainXml;
import com.sun.enterprise.v3.services.impl.LogManagerService;
import com.sun.hk2.component.InhabitantsParser;
import com.sun.hk2.component.KeyValuePairParser;
import com.sun.hk2.component.ExistingSingletonInhabitant;
import com.sun.hk2.component.ConstructorWomb;

import java.io.File;
import java.io.IOException;
import java.util.Collections;

import org.jvnet.hk2.component.Habitat;
import org.jvnet.hk2.component.Inhabitant;
import org.jvnet.hk2.component.MultiMap;

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
            long start = System.currentTimeMillis();
            new Main().boot(args);

            System.out.println("GFv3 started in "+(System.currentTimeMillis()-start)+"ms");
            // block forever. TODO: show how to shut down v3 cleanly.
            Object o = new Object();
            synchronized (o) {
                o.wait();
            }
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

        // HACK TO BE REMOVED
        System.setProperty("com.sun.aas.instanceRoot",".");

        launch(mrs,startupContext);
    }

    @Override
    protected InhabitantsParser createInhabitantsParser(Habitat habitat) {
        return new InhabitantsParser(habitat) {
            protected void add(Inhabitant i, KeyValuePairParser kvpp) {
                // we'll build the equivalent of domain.xml on our own,
                // so don't let the default component parse it.
//                if(i.typeName().equals(DomainXml.class.getName()))
//                    return;

                if(i.typeName().equals(LogManagerService.class.getName()))
                    return;

                if(i.typeName().equals(DomainXml.class.getName())) {
                    MultiMap metadata = new MultiMap(i.metadata());
                    metadata.set("class", Collections.singletonList(DomainXmlParser2.class.getName()));
                    super.add(new ConstructorWomb(DomainXmlParser2.class,habitat, metadata),kvpp);
                    return;
                }


                super.add(i, kvpp);
            }
        };
    }
}
