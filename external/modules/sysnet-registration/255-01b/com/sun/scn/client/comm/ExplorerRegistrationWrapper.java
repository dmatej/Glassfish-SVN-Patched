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
 */

package com.sun.scn.client.comm;

import com.sun.scn.dao.Domain;
import com.sun.scn.servicetags.Agent;
import com.sun.scn.servicetags.AuthenticationCredential;
import com.sun.scn.servicetags.EnvironmentInformation;
import com.sun.scn.servicetags.SunOnlineAccount;
import com.sun.scn.servicetags.SvcTag;
import com.sun.scn.client.comm.RegSender;
import com.sun.scn.client.comm.NoAuthRegSender;
import com.sun.scn.client.comm.SvcTagException;
import com.sun.scn.client.util.InventoryEnvironmentTarget;
import com.sun.scn.client.util.SCRKClientHelper;

import java.net.ConnectException;
import java.net.UnknownHostException;
import java.net.InetAddress;

import java.security.KeyPair;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.List;
import java.util.UUID;
import java.util.Locale;
import java.util.ResourceBundle;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class ExplorerRegistrationWrapper extends RegistrationWrapper {

    /**
     * Constructor.
     *
     * @param registratorId The identifier of the process performing the registration.
     * Ex: for Basic Reg version 1.1.2, the registratorId would be br_1.1.2
     */
    public ExplorerRegistrationWrapper(String registratorId) {
        super(registratorId, null, -1);
        useAuthentication(false);
    }

    /**
     * Constructor.
     *
     * @param registratorId The identifier of the process performing the registration.
     * Ex: for Basic Reg version 1.1.2, the registratorId would be br_1.1.2
     * @param proxyHost The proxy host to be used for all outgoing messages.
     * @param proxyPort The proxy port to be used for all outgoing messages.
     */
    public ExplorerRegistrationWrapper(String registratorId, String proxyHost,
            int proxyPort) {
        super(registratorId, proxyHost, proxyPort);
        useAuthentication(false);
    }

    /**
     * Determines if the given service tag urn is registered to any of the
     * domains the user has access to.
     *
     * @return True if the tag is registered to one of the user's domains,
     * false otherwise.
     */
    public boolean isTagRegisteredToUser(String user, String serviceTagURN) {
        AuthenticationCredential cred =
            new AuthenticationCredential(null, null, user);
        List<Domain> domains = null;

        try {
            domains = getDomains(cred);
        } catch (Exception e) {
            // ignore for now
        }

        if (domains == null) {
            return false;
        }

        for (Domain domain : domains) {
            try {
                if (isSvcTagRegistered(serviceTagURN, cred, domain)) {
                    return true;
                }
            } catch (Exception e) {
                // ignore for now
            }
        }

        return false;
    }

    /**
     * Gets an "explorer" domain for the user, if an existing explorer domain cannot
     * be found, one is created and returned.
     *
     * @return the domain
     * @throws SvcTagException if there was an issue while trying
     * to get the domain
     */
    public Domain getExplorerDomain(AuthenticationCredential cred)
            throws SvcTagException {
        if (cred == null) {
            throw new SvcTagException(
                resources.getString("error.auth.nullcredentials"));
        }

        String user = cred.getUsername();
        Domain domain = null;

        if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
            domain = NoAuthRegSender.getExplorerDomain(user);
        } else {
            domain = NoAuthRegSender.getExplorerDomain(user, proxyHost, proxyPort);
        }
        return domain;
    }
}
