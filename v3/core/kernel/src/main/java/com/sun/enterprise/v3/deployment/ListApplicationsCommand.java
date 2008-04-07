/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2008 Sun Microsystems, Inc. All rights reserved.
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
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
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
 */

package com.sun.enterprise.v3.deployment;

import org.glassfish.api.ActionReport;
import org.glassfish.api.admin.AdminCommand;
import org.glassfish.api.admin.AdminCommandContext;
import org.glassfish.api.Param;
import org.glassfish.api.I18n;
import org.jvnet.hk2.annotations.Service;
import org.jvnet.hk2.annotations.Inject;
import org.jvnet.hk2.annotations.Scoped;
import org.jvnet.hk2.component.PerLookup;
import com.sun.enterprise.v3.admin.CommandRunner;
import com.sun.enterprise.util.LocalStringManagerImpl;
import com.sun.enterprise.config.serverbeans.Module;
import com.sun.enterprise.config.serverbeans.Application;

/**
 *
 * @author Jerome Dochez
 */
@Service(name="list-applications")
@I18n("list.applications")
@Scoped(PerLookup.class)
public class ListApplicationsCommand extends ListComponentsCommand {

    @Param(optional=true)
    String type = null;

    final private static LocalStringManagerImpl localStrings = new LocalStringManagerImpl(DeployDirCommand.class);

    public void execute(AdminCommandContext context) {
        final ActionReport report = context.getActionReport();

        if (!checkTypeValue(type, report)) {
            return;
        }
        ActionReport.MessagePart part = report.getTopMessagePart();        
        part.setMessage(localStrings.getLocalString("list.applications.success", "list-applications successful"));
        int numOfApplications = 0;
        for (Module module : applications.getModules()) {
            if (module instanceof Application) {
                final Application app = (Application)module;
                if (app.getObjectType().equals("user")) {
                    if (type==null || isApplicationOfThisType(app, type)) {
                        ActionReport.MessagePart childPart = part.addChild();
                        childPart.setMessage(app.getName() + " " +
                                             getSnifferEngines(app, true));
                            //this is a kludge so that NB Plugin can get these information
                            //the "nb-" prefix indicates that it's a kludge for NB plugin
                        childPart.addProperty("nb-name", app.getName());
                        childPart.addProperty("nb-location", app.getLocation());
                        childPart.addProperty("nb-engine", getSnifferEngines(app, false));
                        childPart.addProperty("nb-enabled", app.getEnabled());
                        childPart.addProperty("nb-directory-deployed", app.getDirectoryDeployed());
                        childPart.addProperty("nb-context-root", (app.getContextRoot()==null)?"":
                                                                  app.getContextRoot());
                        
                        numOfApplications++;
                    }
                }
            }
        }
        if (numOfApplications == 0) {
            part.setMessage(localStrings.getLocalString("list.components.no.elements.to.list", "Nothing to List."));            
        }
        report.setActionExitCode(ActionReport.ExitCode.SUCCESS);
    }
}
