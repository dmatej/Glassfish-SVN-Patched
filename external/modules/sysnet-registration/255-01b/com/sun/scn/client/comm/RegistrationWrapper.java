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

import java.util.logging.Logger;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegistrationWrapper {
    private String regId;
    protected String proxyHost;
    protected int proxyPort;
    private boolean useAuth;
    private static final Set<Character> validChars = getValidChars();
    private static final int MAX_THREADS = 5;

    /** Logger for RegistrationWrapper */
    private static Logger log =
        Logger.getLogger(RegistrationWrapper.class.getName());

    /** Resource bundle with default locale */
    protected ResourceBundle resources = null;

    /**
     * Constructor.
     *
     * @param registratorId The identifier of the process performing the registration.
     * Ex: for Basic Reg version 1.1.2, the registratorId would be br_1.1.2
     */
    public RegistrationWrapper(String registratorId) {
        init(registratorId, null, -1);
    }

    /**
     * Constructor.
     *
     * @param registratorId The identifier of the process performing the registration.
     * Ex: for Basic Reg version 1.1.2, the registratorId would be br_1.1.2
     * @param proxyHost The proxy host to be used for all outgoing messages.
     * @param proxyPort The proxy port to be used for all outgoing messages.
     */
    public RegistrationWrapper(String registratorId, String proxyHost,
            int proxyPort) {
        init(registratorId, proxyHost, proxyPort);
    }

    /**
     * Specifies whether or not authentication should be used when communicating
     * with Sun Service Endpoints.  Note that unless this client is running in
     * within a known environment, the majority of these calls will fail if
     * useAuth is set to false.  The default for using authentication is set to true.
     *
     * @param useAuth Specifies whether calls to the Sun Service Endpoints should
     * be using authentication.
     */
    public void useAuthentication(boolean useAuth) {
        this.useAuth = useAuth;
    }

    /**
     * Sets the target for the inventory environment to use.  Not intended
     * for use outside of Sun internal development.  This overrides the default
     * value in the Connection.properties file.
     *
     * @param envTarget The environment to use for registrations and
     * authentication.
     */
    public void setInventoryEnvironmentTarget(InventoryEnvironmentTarget envTarget) {
        //System.out.println("Using the " + envTarget + " environment for registrations");
        System.setProperty(RegSender.TARGET_PROPERTY_KEY,
            envTarget.getEnvironmentTarget());
        System.setProperty(NoAuthRegSender.TARGET_PROPERTY_KEY,
            envTarget.getEnvironmentTarget());
        System.setProperty(SCRKClientHelper.TARGET_PROPERTY_KEY,
            envTarget.getEnvironmentTarget());
    }

    /**
     * Returns the resource bundle in use for the wrapper.
     *
     * @return The resource bundle.
     */
    public ResourceBundle getResourceBundle() {
        return resources;
    }

    /**
     * Sets the resource bundle for use in the wrapper.
     *
     * @param bundle The resource bundle.
     */
    public void setResourceBundle(ResourceBundle bundle) {
        resources = bundle;
    }

    /**
     * Returns the privacy policy text.
     *
     * @return The privacy policy text.
     */
    public String getPrivacyPolicyText() {
        return resources.getString("privacy.text");
    }

    /**
     * Returns the URL for the terms of use.
     *
     * @return The URL for the terms of use.
     */
    public String getTermsOfUseLink() {
        return resources.getString("tou.link");
    }

    /**
     * Returns the list of domains the user has privileges to view.
     *
     * @return the list of domains
     * @throws SvcTagException if there was an issue while trying
     * to retrieve the list of domains
     */
    public List<Domain> getDomains(AuthenticationCredential cred)
            throws SvcTagException {
        if (cred == null) {
            throw new SvcTagException(
                resources.getString("error.auth.nullcredentials"));
        }

        String clientId = cred.getRegistrationClientId();
        KeyPair keyPair = cred.getKeyPair();
        String user = cred.getUsername();
        List<Domain> domains = null;

        if (useAuth) {
            if (clientId == null || keyPair == null || user == null) {
                return null;
            }

            if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                domains = RegSender.getUserDomains(user, keyPair, clientId);
            } else {
                domains = RegSender.getUserDomains(user, proxyHost, proxyPort,
                    keyPair, clientId);
            }
        } else {
            if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                domains = NoAuthRegSender.getUserDomains(user);
            } else {
                domains = NoAuthRegSender.getUserDomains(user, proxyHost, proxyPort);
            }
        }
        return domains;
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

        Domain domain = null;
        String clientId = cred.getRegistrationClientId();
        KeyPair keyPair = cred.getKeyPair();
        String user = cred.getUsername();

        if (useAuth) {
            if (clientId == null || keyPair == null || user == null) {
                return null;
            }

            if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                domain = RegSender.getExplorerDomain(user, keyPair, clientId);
            } else {
                domain = RegSender.getExplorerDomain(user, proxyHost, proxyPort,
                    keyPair, clientId);
            }
        } else {
            if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                domain = NoAuthRegSender.getExplorerDomain(user);
            } else {
                domain = NoAuthRegSender.getExplorerDomain(user, proxyHost, proxyPort);
            }
        }
        return domain;
    }

    /**
     * Creates a Sun Online Account.
     *
     * @param soa The sun online account to create.
     *
     * @throws SvcTagException if there was an issue while trying to create
     * the Sun Online Account.
     */
    public void createSunOnlineAccount(SunOnlineAccount soa)
            throws SvcTagException {
        if (soa.getUserid() == null || soa.getUserid().trim().equals("")) {
            throw new SvcTagException(resources.getString("create.soa.missing.userid"));
        }
        if (soa.getCountry() == null || soa.getCountry().trim().equals("")) {
            throw new SvcTagException(resources.getString("create.soa.missing.country"));
        }
        if (soa.getLastname() == null || soa.getLastname().trim().equals("")) {
            throw new SvcTagException(resources.getString("create.soa.missing.lastname"));
        }
        if (soa.getFirstname() == null || soa.getFirstname().trim().equals("")) {
            throw new SvcTagException(resources.getString("create.soa.missing.firstname"));
        }
        if (soa.getEmail() == null || soa.getEmail().trim().equals("")) {
            throw new SvcTagException(resources.getString("create.soa.missing.email"));
        }
        if (soa.getPassword() == null || soa.getPassword().length() < 6) {
            throw new SvcTagException(SvcTagException.PASSWORD_TOO_SHORT,
                resources.getString("create.soa.invalid.password"));
        }
        if (soa.getScreenName() != null) {
            if (!isValidScreenNameFormat(soa.getScreenName())) {
                throw new SvcTagException(resources.getString("create.soa.invalid.screename.format"));
            }
        }

        try {
            if (useAuth) {
                RegSender.createSunOnlineAccount(soa, proxyHost, proxyPort);
            } else {
                NoAuthRegSender.createSunOnlineAccount(soa, proxyHost, proxyPort);
            }
        } catch (SvcTagException ste) {
            if (ste.getCode() == SvcTagException.GENERAL_ERROR) {
                throw new SvcTagException(resources.getString("fatal.create.soa.error"));
            }
            throw ste;
        }
    }

    /**
     * Checks if the given service tag is registered to the given user's domain.
     *
     * @return whether or not the given service tag is registered.
     *
     * @throws AuthenticationException if there is an issue with authentication.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     *
     */
    public boolean isSvcTagRegistered(String serviceTagURN,
            AuthenticationCredential cred, Domain domain)
            throws AuthenticationException, ConnectException,
            UnknownHostException {

        SvcTag st = null;
        String clientId = cred.getRegistrationClientId();
        KeyPair keyPair = cred.getKeyPair();

        try {
            if (useAuth) {
                if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                    st = RegSender.getSvcTag
                        ("svctag/" + serviceTagURN,
                        keyPair, clientId, domain.getDomainId());
                } else {
                    st = RegSender.getSvcTag
                        ("svctag/" + serviceTagURN,
                        proxyHost, proxyPort, keyPair, clientId, domain.getDomainId());
                }
            } else {
                if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                    st = NoAuthRegSender.getSvcTag
                        ("svctag/" + serviceTagURN, domain.getDomainId());
                } else {
                    st = NoAuthRegSender.getSvcTag
                        ("svctag/" + serviceTagURN, proxyHost, proxyPort, domain.getDomainId());
                }
            }
        } catch (SvcTagException ste) {
            return false;
        }

        if (st != null) {
            if (st.getStatus().equals("REGISTERED")
                    && st.getSubStatus().equals("MATCHED_DOMAINS")) {
                return true;
            }
        }

        return false;
    }

    /**
     * Checks if the given service tag is registered to any domain.
     *
     * @return whether or not the given service tag is registered to any domain.
     *
     * @throws SvcTagException if there is an issue with obtaining the service
     * tag registration status.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     *
     */
    public boolean isSvcTagRegistered(String serviceTagURN)
            throws SvcTagException, ConnectException, UnknownHostException {
        return RegSender.isSvcTagRegistered(serviceTagURN);
    }

    /**
     * Determines if the given service tag urn is registered to any of the
     * domains the user has access to.
     *
     * @return True if the tag is registered to one of the user's domains,
     * false otherwise.
     */
    public boolean isTagRegisteredToUser(String serviceTagURN,
            AuthenticationCredential cred) {
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
     * Registers the service tag to the user's default domain.
     *
     * @param environment The environment the service tag runs in.
     * @param serviceTag The service tag to register.
     * @param registryURN The URN of the registry that the service tag belongs to.
     * @param user The Sun Online Account username
     * @param pass The Sun Online Account password
     *
     * @throws SvcTagException if there is an issue with registering the service
     * tag.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     *
     * @deprecated  As of v 2.0.1, replaced by
     *              {@link #registerServiceTag(
                     EnvironmentInformation, SvcTag, String,
                     AuthenticationCredential, Domain) }
     */
    public void registerServiceTag(EnvironmentInformation environment,
            SvcTag serviceTag, String registryURN, String user, String pass)
            throws SvcTagException, AuthenticationException, ConnectException,
            UnknownHostException {
        List<SvcTag> set = new ArrayList<SvcTag>();
        set.add(serviceTag);
        registerServiceTags(environment, set, registryURN, user, pass);
    }

    /**
     * Registers the service tag to the user's default domain.
     *
     * @param environment The environment the service tag runs in.
     * @param serviceTag The service tag to register.
     * @param registryURN The URN of the registry that the service tag belongs to.
     * @param cred Authentication credentials from a successful authentication.
     *
     * @throws SvcTagException if there is an issue with registering the service
     * tag.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     *
     * @deprecated  As of v 2.0.1, replaced by
     *              {@link #registerServiceTag(
                     EnvironmentInformation, SvcTag, String,
                     AuthenticationCredential, Domain) }
     */
    public void registerServiceTag(EnvironmentInformation environment,
            SvcTag serviceTag, String registryURN,
            AuthenticationCredential cred)
            throws SvcTagException, AuthenticationException, ConnectException,
            UnknownHostException {
        List<SvcTag> set = new ArrayList<SvcTag>();
        set.add(serviceTag);
        registerServiceTags(environment, set, registryURN, cred);
    }

    /**
     * Registers the service tag to the provided domain.  Note that the user
     * must have permissions on the provided domain for the registration to
     * succeed.
     *
     * @param environment The environment the service tag runs in.
     * @param serviceTag The service tag to register.
     * @param registryURN The URN of the registry that the service tag belongs to.
     * @param cred Authentication credentials from a successful authentication.
     * @param domain The domain to register the service tag into.
     *
     * @throws SvcTagException if there is an issue with registering the service
     * tag.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     */
    public void registerServiceTag(EnvironmentInformation environment,
            SvcTag serviceTag, String registryURN,
            AuthenticationCredential cred, Domain domain)
            throws SvcTagException, AuthenticationException, ConnectException,
            UnknownHostException {
        List<SvcTag> set = new ArrayList<SvcTag>();
        set.add(serviceTag);
        registerServiceTags(environment, set, registryURN, cred, domain);
    }

    /**
     * Registers the user with the SCRK Client Registration Service.  If this
     * succeeds, then the user's credentials were correct.  If there are any
     * issues with authenticating the user, an exception will be thrown.
     *
     * @param user The Sun Online Account username.
     * @param pass The Sun Online Account password.
     *
     * @throws SvcTagException if there is an issue with registering the service
     * tag.
     * @throws AuthenticationException if there is an issue with authentication.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     */
    public AuthenticationCredential authenticate(String user, String pass)
            throws SvcTagException, AuthenticationException, ConnectException,
            UnknownHostException {
        KeyPair keyPair = null;
        String clientId = null;

        /**
         * Create keypair and use it to get client registration ID.
         **/
        try {
            SCRKClientHelper clientHelper = new SCRKClientHelper();
            keyPair = clientHelper.getKeyPair();
            clientId = clientHelper.getClientRegId(keyPair, user, pass,
                proxyHost, proxyPort);
        } catch (ConnectException ce) {
            throw ce;
        } catch (UnknownHostException uhe) {
            throw uhe;
        } catch (RuntimeException rte) {
            throw new SvcTagException(rte.getMessage());
        } catch (Exception e) {
            throw new AuthenticationException(e.getMessage());
        }

        if (keyPair == null) {
            throw new SvcTagException(resources.getString("error.scrk.keypair"));
        }

        if (clientId == null) {
            throw new AuthenticationException(resources.getString("error.scrk.regid"));
        }

        AuthenticationCredential cred =
            new AuthenticationCredential(keyPair, clientId, user);
        return cred;
    }

    /**
     * Unauthenticates the user with the SCRK Client Unregistration Service.  Does
     * a best effort to unregister the authentication credentials.  This should be
     * called after completing the registration flow.
     *
     * @param cred The authentication credentials to unregister.
     */
    public void unauthenticate(AuthenticationCredential cred) {
        try {
            SCRKClientHelper clientHelper = new SCRKClientHelper();
            clientHelper.deleteClientRegId(cred.getKeyPair(),
                cred.getRegistrationClientId(), proxyHost, proxyPort);
        } catch (Exception e) {
        }
    }

    /**
     * Registers the set of service tags to the user's default domain.
     * Note that it is expected that all the
     * service tags to register are contained in the same registry and run in
     * the same environment.  If that isn't the case, then each service tag
     * should be registered individually.
     *
     * @param environment The environment the service tags run in.
     * @param serviceTags The set of service tags to register.
     * @param registryURN The URN of the registry that the service tags belong to.
     * @param user The Sun Online Account username
     * @param pass The Sun Online Account password
     *
     * @throws SvcTagException if there is an issue with registering the service
     * tag.
     * @throws AuthenticationException if there is an issue with authentication.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     *
     * @deprecated  As of v 2.0.1, replaced by
     *              {@link #registerServiceTags(
                     EnvironmentInformation, List, String,
                     AuthenticationCredential, Domain) }
     */
    public void registerServiceTags(EnvironmentInformation environment,
            List<SvcTag> serviceTags, String registryURN, String user, String pass)
            throws SvcTagException, AuthenticationException, ConnectException,
            UnknownHostException {
        KeyPair keyPair;
        String agentUrn;
        String clientId;
        AuthenticationCredential cred = authenticate(user, pass);
        /**
         * Get domains created for this particular user.
         **/
        List<Domain> domains = getDomains(cred);
        if (domains != null && domains.size() > 1) {
            log.fine(resources.getString("error.multiple.domains.found"));
            /*
            throw new SvcTagException(
                resources.getString("error.multiple.domains.found"));
            */
        }

        Domain domain = getDefaultDomain(domains, user);

        if (domain == null) {
            throw new SvcTagException(
                resources.getString("error.default.domain"));
        }

        registerServiceTags(environment, serviceTags, registryURN,
            cred, domain);

        unauthenticate(cred);
    }

    /**
     * Registers the set of service tags to the user's default domain.
     * Note that it is expected that all the
     * service tags to register are contained in the same registry and run in
     * the same environment.  If that isn't the case, then each service tag
     * should be registered individually.  Uses the user's default domain (if
     * one is found).  Note that if a user has multiple domains, then the user
     * should be presented with the choice of domains to register into.
     *
     * @param environment The environment the service tags run in.
     * @param serviceTags The set of service tags to register.
     * @param registryURN The URN of the registry that the service tags belong to.
     * @param cred Authentication credentials from a successful authentication.
     *
     * @throws SvcTagException if there is an issue with registering the service
     * tag.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     *
     * @deprecated  As of v 2.0.1, replaced by
     *              {@link #registerServiceTags(
                     EnvironmentInformation, List, String,
                     AuthenticationCredential, Domain) }
     */
    public void registerServiceTags(EnvironmentInformation environment,
            List<SvcTag> serviceTags, String registryURN,
            AuthenticationCredential cred)
            throws SvcTagException, ConnectException, UnknownHostException {
        if (cred == null) {
            throw new SvcTagException(
                resources.getString("error.auth.nullcredentials"));
        }

        /**
         * Get domains created for this particular user.
         **/
        List<Domain> domains = getDomains(cred);
        if (domains != null && domains.size() > 1) {
            log.fine(resources.getString("error.multiple.domains.found"));
            /*
            throw new SvcTagException(
                resources.getString("error.multiple.domains.found"));
            */
        }

        Domain domain = getDefaultDomain(domains, cred.getUsername());

        if (domain == null) {
            throw new SvcTagException(
                resources.getString("error.default.domain"));
        }

        registerServiceTags(environment, serviceTags, registryURN,
            cred, domain);
    }

    /**
     * Registers the set of service tags to the provided domain.
     * Note that it is expected that all the
     * service tags to register are contained in the same registry and run in
     * the same environment.  If that isn't the case, then each service tag
     * should be registered individually.
     * Also note that the user must have permissions on the provided domain
     * for the registration attempt to succeed.
     *
     * @param environment The environment the service tags run in.
     * @param serviceTags The set of service tags to register.
     * @param registryURN The URN of the registry that the service tags belong to.
     * @param cred Authentication credentials from a successful authentication.
     * @param domain The domain to register the service tags into
     * @param useThreading Option to either use threading when registering the
     * individual service tags or not
     *
     * @throws SvcTagException if there is an issue with registering the service
     * tag.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     */
    public void registerServiceTags(EnvironmentInformation environment,
            List<SvcTag> serviceTags, String registryURN,
            AuthenticationCredential cred, Domain domain,
            boolean useThreading)
            throws SvcTagException, ConnectException, UnknownHostException {
        KeyPair keyPair;
        String agentUrn;
        String clientId;
        String user;

        if (domain == null) {
            throw new SvcTagException(
                resources.getString("error.null.domain"));
        }

        if (cred == null) {
            throw new SvcTagException(
                resources.getString("error.auth.nullcredentials"));
        }

        clientId = cred.getRegistrationClientId();
        keyPair = cred.getKeyPair();
        user = cred.getUsername();

        /**
         * Create an Agent
         **/
        try {
            agentUrn = registryURN;

            if (agentUrn == null) {
                agentUrn = "urn:st:"
                    + regId + UUID.randomUUID().toString();
            }

            Agent agent = new Agent(agentUrn);

            // Set hostname
            agent.host = environment.getHostname();

            agent.hostid = environment.getHostId();

            agent.serialNumber = environment.getSerialNumber();

            // Set OS name
            agent.system = environment.getOsName();

            // set OS version
            agent.release = environment.getOsVersion();

            // Set OS architecture
            agent.architecture = environment.getOsArchitecture();

            // Set type of model
            agent.platform = environment.getSystemModel();

            // Set manufacture
            agent.manufacturer = environment.getSystemManufacturer();

            // Set CPU manufacture
            agent.cpuManufacturer = environment.getCpuManufacturer();

            agent.physmem = environment.getPhysMem();
            agent.sockets = environment.getSockets();
            agent.cores = environment.getCores();
            agent.virtcpus = environment.getVirtCpus();
            agent.cpuname = environment.getCpuName();
            agent.clockrate = environment.getClockRate();

            // Submit request to Product Registration Service
            // to register the agent
            if (useAuth) {
                if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                    RegSender.sendPut
                        (agent.toXMLElement(false),
                        "agent/" + agent.getAgentURN(),
                        keyPair, clientId);
                } else {
                    RegSender.sendPut
                        (agent.toXMLElement(false),
                        "agent/" + agent.getAgentURN(),
                        proxyHost, proxyPort, keyPair, clientId);
                }
            } else {
                if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                    NoAuthRegSender.sendPut
                        (agent.toXMLElement(false),
                        "agent/" + agent.getAgentURN());
                } else {
                    NoAuthRegSender.sendPut
                        (agent.toXMLElement(false),
                        "agent/" + agent.getAgentURN(), proxyHost, proxyPort);
                }
            }
        } catch (SvcTagException ste) {
            if (ste.getCode() == SvcTagException.HTTP_ERROR
                    && ste.getDetailMessage() != null) {
                //throw new SvcTagException(ste.getDetailMessage());
                throw new SvcTagException(
                    resources.getString("error.registration.agent"));
            }
            throw new SvcTagException(resources.getString("fatal.register.error"));
            //throw ste;
        } catch (Exception e) {
            throw new SvcTagException(
                resources.getString("error.registration.agent"));
        }

        final String regClientURN = "urn:st:" + regId + UUID.randomUUID().toString();
        if (useThreading) {
            List<Thread> threads = new ArrayList<Thread>();
            final KeyPair kp = keyPair;
            final String au = agentUrn;
            final Domain d = domain;
            final String ci = clientId;
            final String u = user;
            for (final SvcTag serviceTag : serviceTags) {
                threads.add(new Thread() {
                    public void run() {
                        try {
                            // initialize the service tag for registration
                            serviceTag.setRegistrationClientURN(regClientURN);
                            serviceTag.setUserID(u);
                            serviceTag.setAgentURN(au);
                            serviceTag.setStatus(SvcTag.REGISTERED);
                            serviceTag.setDomainId(d.getDomainId());
                            serviceTag.setDomainName(d.getDomainName());
                            //System.out.println(serviceTag.toXMLString());
                            if (useAuth) {
                                if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                                    RegSender.sendPut(
                                        serviceTag.toXMLElement(),
                                        "svctag/" +
                                        serviceTag.getInstanceURN(),
                                        kp, ci);
                                } else {
                                    RegSender.sendPut(
                                        serviceTag.toXMLElement(),
                                        "svctag/" +
                                        serviceTag.getInstanceURN(),
                                        proxyHost, proxyPort,
                                        kp, ci);
                                }
                            } else {
                                if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                                    NoAuthRegSender.sendPut(
                                        serviceTag.toXMLElement(),
                                        "svctag/" +
                                        serviceTag.getInstanceURN());
                                } else {
                                    NoAuthRegSender.sendPut(
                                        serviceTag.toXMLElement(),
                                        "svctag/" +
                                        serviceTag.getInstanceURN(),
                                        proxyHost, proxyPort);
                                }
                            }
                        } catch (SvcTagException ste) {
                            if (ste.getCode() == SvcTagException.HTTP_ERROR
                                    && ste.getDetailMessage() != null) {
                                log.fine("Exception encountered: " + ste.getDetailMessage());
                            }
                            log.fine(resources.getString("fatal.register.error"));
                        } catch (Exception e) {
                            log.fine(resources.getString("error.registration.svctag"));
                            log.fine("Exception encountered: " + e.getMessage());
                        }
                    }
                });

                if (threads.size() == MAX_THREADS) {
                    for (Thread th : threads) {
                        th.start();
                    }

                    for (Thread th : threads) {
                        try {
                            th.join();
                        } catch (Exception e) {
                        }
                    }
                    for (Thread th : threads) {
                        th = null;
                    }

                    threads = new ArrayList<Thread>(MAX_THREADS);
                }
            }

            for (Thread t : threads) {
                t.start();
            }

            for (Thread t : threads) {
                try {
                    t.join();
                } catch (Exception e) {
                }
            }
        } else {
        for (SvcTag serviceTag : serviceTags) {
            try {
                // initialize the service tag for registration
                serviceTag.setRegistrationClientURN(regClientURN);
                serviceTag.setUserID(user);
                serviceTag.setAgentURN(agentUrn);
                serviceTag.setStatus(SvcTag.REGISTERED);
                serviceTag.setDomainId(domain.getDomainId());
                serviceTag.setDomainName(domain.getDomainName());
                //System.out.println(serviceTag.toXMLString());

                // Submit request to Product Registration Service to register the
                // service tag.
                if (useAuth) {
                    if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                        RegSender.sendPut(
                            serviceTag.toXMLElement(),
                            "svctag/" +
                            serviceTag.getInstanceURN(),
                            keyPair, clientId);
                    } else {
                        RegSender.sendPut(
                            serviceTag.toXMLElement(),
                            "svctag/" +
                            serviceTag.getInstanceURN(),
                            proxyHost, proxyPort,
                            keyPair, clientId);
                    }
                } else {
                    if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                        NoAuthRegSender.sendPut(
                            serviceTag.toXMLElement(),
                            "svctag/" +
                            serviceTag.getInstanceURN());
                    } else {
                        NoAuthRegSender.sendPut(
                            serviceTag.toXMLElement(),
                            "svctag/" +
                            serviceTag.getInstanceURN(),
                            proxyHost, proxyPort);
                    }
                }
            } catch (SvcTagException ste) {
                if (ste.getCode() == SvcTagException.HTTP_ERROR
                        && ste.getDetailMessage() != null) {
                    throw new SvcTagException(ste.getDetailMessage());
                }
                throw new SvcTagException(resources.getString("fatal.register.error"));
                //throw ste;
            } catch (Exception e) {
                throw new SvcTagException(
                    resources.getString("error.registration.svctag"));
            }
        }
        }
    }

    /**
     * Registers the set of service tags to the provided domain.
     * Note that it is expected that all the
     * service tags to register are contained in the same registry and run in
     * the same environment.  If that isn't the case, then each service tag
     * should be registered individually.
     * Also note that the user must have permissions on the provided domain
     * for the registration attempt to succeed.
     *
     * @param environment The environment the service tags run in.
     * @param serviceTags The set of service tags to register.
     * @param registryURN The URN of the registry that the service tags belong to.
     * @param cred Authentication credentials from a successful authentication.
     * @param domain The domain to register the service tags into
     *
     * @throws SvcTagException if there is an issue with registering the service
     * tag.
     * @throws ConnectException if there was an exception connecting to the service.
     * @throws UnknownHostException if there was an unknown host exception while
     * trying to connect to the service.
     */
    public void registerServiceTags(EnvironmentInformation environment,
            List<SvcTag> serviceTags, String registryURN,
            AuthenticationCredential cred, Domain domain)
            throws SvcTagException, ConnectException, UnknownHostException {
        registerServiceTags(environment, serviceTags, registryURN, cred, domain, true);
    }

    /**
     * Converts an agent object into an EnvironmentInformation object.
     *
     * @return the newly mapped environment information.
     */
    public static EnvironmentInformation convertAgentToEnvironmentInformation(Agent agent) {
        EnvironmentInformation env = new EnvironmentInformation(false);
        if (agent == null) {
            return env;
        }
        env.setHostname(agent.host);
        env.setHostId(agent.hostid);
        env.setSerialNumber(agent.serialNumber);
        env.setOsName(agent.system);
        env.setOsVersion(agent.release);
        env.setOsArchitecture(agent.architecture);
        env.setSystemModel(agent.platform);
        env.setSystemManufacturer(agent.manufacturer);
        env.setCpuManufacturer(agent.cpuManufacturer);
        return env;
    }

    /**
     * Returns a list of currently accepted security questions for creating a
     * Sun Online Account.
     *
     * @return the list of available security questions.
     */
    public List<String> getAvailableSecurityQuestions() {
        List<String> secQuestions = new ArrayList<String>();
        secQuestions.add(resources.getString("security.question.color"));
        secQuestions.add(resources.getString("security.question.pet"));
        secQuestions.add(resources.getString("security.question.vacation"));
        return secQuestions;
    }

    /**
     * Returns a list of currently accepted countries for creating a
     * Sun Online Account.
     *
     * @return the list of available countries.
     */
    public List<String> getAvailableCountries() {
        List<String> countries = new ArrayList<String>();
        countries.add(resources.getString("country.Albania"));
        countries.add(resources.getString("country.Algeria"));
        countries.add(resources.getString("country.AmericanSamoa"));
        countries.add(resources.getString("country.Andorra"));
        countries.add(resources.getString("country.Angola"));
        countries.add(resources.getString("country.Anguilla"));
        countries.add(resources.getString("country.Antarctica"));
        countries.add(resources.getString("country.Antigua&Barbuda"));
        countries.add(resources.getString("country.Argentina"));
        countries.add(resources.getString("country.Armenia"));
        countries.add(resources.getString("country.Aruba"));
        countries.add(resources.getString("country.AscensionIsland"));
        countries.add(resources.getString("country.Australia"));
        countries.add(resources.getString("country.Austria"));
        countries.add(resources.getString("country.Azerbaijan"));
        countries.add(resources.getString("country.Bahamas"));
        countries.add(resources.getString("country.Bahrain"));
        countries.add(resources.getString("country.Bangladesh"));
        countries.add(resources.getString("country.Barbados"));
        countries.add(resources.getString("country.Belarus"));
        countries.add(resources.getString("country.Belgium"));
        countries.add(resources.getString("country.Belize"));
        countries.add(resources.getString("country.Benin"));
        countries.add(resources.getString("country.Bermuda"));
        countries.add(resources.getString("country.Bhutan"));
        countries.add(resources.getString("country.Bolivia"));
        countries.add(resources.getString("country.BosniaandHerzegovina"));
        countries.add(resources.getString("country.Botswana"));
        countries.add(resources.getString("country.BouvetIsland"));
        countries.add(resources.getString("country.Brazil"));
        countries.add(resources.getString("country.BritishIndianOceanTerritory"));
        countries.add(resources.getString("country.BruneiDarussalam"));
        countries.add(resources.getString("country.Bulgaria"));
        countries.add(resources.getString("country.BurkinaFaso"));
        countries.add(resources.getString("country.Burundi"));
        countries.add(resources.getString("country.Cambodia"));
        countries.add(resources.getString("country.Cameroon"));
        countries.add(resources.getString("country.Canada"));
        countries.add(resources.getString("country.CapeVerde"));
        countries.add(resources.getString("country.CaymanIslands"));
        countries.add(resources.getString("country.CentralAfricanRepublic"));
        countries.add(resources.getString("country.Chad"));
        countries.add(resources.getString("country.Chile"));
        countries.add(resources.getString("country.China"));
        countries.add(resources.getString("country.ChristmasIsland"));
        countries.add(resources.getString("country.Cocos(Keeling)Islands"));
        countries.add(resources.getString("country.Colombia"));
        countries.add(resources.getString("country.Comoros"));
        countries.add(resources.getString("country.Congo,DemocraticRepublicofthe"));
        countries.add(resources.getString("country.Congo,Republicof"));
        countries.add(resources.getString("country.CookIslands"));
        countries.add(resources.getString("country.CostaRica"));
        countries.add(resources.getString("country.Coted'Ivoire"));
        countries.add(resources.getString("country.Croatia/Hrvatska"));
        countries.add(resources.getString("country.Cyprus"));
        countries.add(resources.getString("country.CzechRepublic"));
        countries.add(resources.getString("country.Denmark"));
        countries.add(resources.getString("country.Djibouti"));
        countries.add(resources.getString("country.Dominica"));
        countries.add(resources.getString("country.DominicanRepublic"));
        countries.add(resources.getString("country.EastTimor"));
        countries.add(resources.getString("country.Ecuador"));
        countries.add(resources.getString("country.Egypt"));
        countries.add(resources.getString("country.ElSalvador"));
        countries.add(resources.getString("country.EquatorialGuinea"));
        countries.add(resources.getString("country.Eritrea"));
        countries.add(resources.getString("country.Estonia"));
        countries.add(resources.getString("country.Ethiopia"));
        countries.add(resources.getString("country.FalklandIslands(Malvina)"));
        countries.add(resources.getString("country.FaroeIslands"));
        countries.add(resources.getString("country.Fiji"));
        countries.add(resources.getString("country.Finland"));
        countries.add(resources.getString("country.France"));
        countries.add(resources.getString("country.France(Metropolitan)"));
        countries.add(resources.getString("country.FrenchGuiana"));
        countries.add(resources.getString("country.FrenchPolynesia"));
        countries.add(resources.getString("country.FrenchSouthernTerritories"));
        countries.add(resources.getString("country.Gabon"));
        countries.add(resources.getString("country.Gambia"));
        countries.add(resources.getString("country.Georgia"));
        countries.add(resources.getString("country.Germany"));
        countries.add(resources.getString("country.Ghana"));
        countries.add(resources.getString("country.Gibraltar"));
        countries.add(resources.getString("country.Greece"));
        countries.add(resources.getString("country.Greenland"));
        countries.add(resources.getString("country.Grenada"));
        countries.add(resources.getString("country.Guadeloupe"));
        countries.add(resources.getString("country.Guam"));
        countries.add(resources.getString("country.Guatemala"));
        countries.add(resources.getString("country.Guernsey"));
        countries.add(resources.getString("country.Guinea"));
        countries.add(resources.getString("country.Guinea-Bissau"));
        countries.add(resources.getString("country.Guyana"));
        countries.add(resources.getString("country.Haiti"));
        countries.add(resources.getString("country.HeardandMcDonaldIslands"));
        countries.add(resources.getString("country.HolySee(CityVaticanState)"));
        countries.add(resources.getString("country.Honduras"));
        countries.add(resources.getString("country.HongKong"));
        countries.add(resources.getString("country.Hungary"));
        countries.add(resources.getString("country.Iceland"));
        countries.add(resources.getString("country.India"));
        countries.add(resources.getString("country.Indonesia"));
        countries.add(resources.getString("country.Ireland"));
        countries.add(resources.getString("country.IsleofMan"));
        countries.add(resources.getString("country.Israel"));
        countries.add(resources.getString("country.Italy"));
        countries.add(resources.getString("country.Jamaica"));
        countries.add(resources.getString("country.Japan"));
        countries.add(resources.getString("country.Jersey"));
        countries.add(resources.getString("country.Jordan"));
        countries.add(resources.getString("country.Kazakhstan"));
        countries.add(resources.getString("country.Kenya"));
        countries.add(resources.getString("country.Kiribati"));
        countries.add(resources.getString("country.Korea,Republicof"));
        countries.add(resources.getString("country.Kuwait"));
        countries.add(resources.getString("country.Kyrgyzstan"));
        countries.add(resources.getString("country.LaoPeople'sDemocraticRepublic"));
        countries.add(resources.getString("country.Latvia"));
        countries.add(resources.getString("country.Lebanon"));
        countries.add(resources.getString("country.Lesotho"));
        countries.add(resources.getString("country.Liberia"));
        countries.add(resources.getString("country.LibyanArabJamahiriya"));
        countries.add(resources.getString("country.Liechtenstein"));
        countries.add(resources.getString("country.Lithuania"));
        countries.add(resources.getString("country.Luxembourg"));
        countries.add(resources.getString("country.Macau"));
        countries.add(resources.getString("country.Macedonia,FormerYugoslavRepublic"));
        countries.add(resources.getString("country.Madagascar"));
        countries.add(resources.getString("country.Malawi"));
        countries.add(resources.getString("country.Malaysia"));
        countries.add(resources.getString("country.Maldives"));
        countries.add(resources.getString("country.Mali"));
        countries.add(resources.getString("country.Malta"));
        countries.add(resources.getString("country.MarshallIslands"));
        countries.add(resources.getString("country.Martinique"));
        countries.add(resources.getString("country.Mauritania"));
        countries.add(resources.getString("country.Mauritius"));
        countries.add(resources.getString("country.Mayotte"));
        countries.add(resources.getString("country.Mexico"));
        countries.add(resources.getString("country.Micronesia,FederalStateof"));
        countries.add(resources.getString("country.Moldova,Republicof"));
        countries.add(resources.getString("country.Monaco"));
        countries.add(resources.getString("country.Mongolia"));
        countries.add(resources.getString("country.Montserrat"));
        countries.add(resources.getString("country.Morocco"));
        countries.add(resources.getString("country.Mozambique"));
        countries.add(resources.getString("country.Namibia"));
        countries.add(resources.getString("country.Nauru"));
        countries.add(resources.getString("country.Nepal"));
        countries.add(resources.getString("country.Netherlands"));
        countries.add(resources.getString("country.NetherlandsAntilles"));
        countries.add(resources.getString("country.NewCaledonia"));
        countries.add(resources.getString("country.NewZealand"));
        countries.add(resources.getString("country.Nicaragua"));
        countries.add(resources.getString("country.Niger"));
        countries.add(resources.getString("country.Nigeria"));
        countries.add(resources.getString("country.Niue"));
        countries.add(resources.getString("country.NorfolkIslands"));
        countries.add(resources.getString("country.NorthernMarianaIslands"));
        countries.add(resources.getString("country.Norway"));
        countries.add(resources.getString("country.Oman"));
        countries.add(resources.getString("country.Pakistan"));
        countries.add(resources.getString("country.Palau"));
        countries.add(resources.getString("country.Panama"));
        countries.add(resources.getString("country.PapuaNewGuinea"));
        countries.add(resources.getString("country.Paraguay"));
        countries.add(resources.getString("country.Peru"));
        countries.add(resources.getString("country.Philippines"));
        countries.add(resources.getString("country.PitcairnIsland"));
        countries.add(resources.getString("country.Poland"));
        countries.add(resources.getString("country.Portugal"));
        countries.add(resources.getString("country.PuertoRico"));
        countries.add(resources.getString("country.Qatar"));
        countries.add(resources.getString("country.ReunionIsland"));
        countries.add(resources.getString("country.Romania"));
        countries.add(resources.getString("country.RussianFederation"));
        countries.add(resources.getString("country.Rwanda"));
        countries.add(resources.getString("country.SaintKittsandNevis"));
        countries.add(resources.getString("country.SaintLucia"));
        countries.add(resources.getString("country.SaintVincentandtheGrenadines"));
        countries.add(resources.getString("country.SanMarino"));
        countries.add(resources.getString("country.SaoTomeandPrincipe"));
        countries.add(resources.getString("country.SaudiArabia"));
        countries.add(resources.getString("country.Senegal"));
        countries.add(resources.getString("country.Seychelles"));
        countries.add(resources.getString("country.SierraLeone"));
        countries.add(resources.getString("country.Singapore"));
        countries.add(resources.getString("country.SlovakRepublic"));
        countries.add(resources.getString("country.Slovenia"));
        countries.add(resources.getString("country.SolomonIslands"));
        countries.add(resources.getString("country.Somalia"));
        countries.add(resources.getString("country.SouthAfrica"));
        countries.add(resources.getString("country.SouthGeorgiaandtheSouthSandwichIslands"));
        countries.add(resources.getString("country.Spain"));
        countries.add(resources.getString("country.SriLanka"));
        countries.add(resources.getString("country.StPierreandMiquelon"));
        countries.add(resources.getString("country.St.Helena"));
        countries.add(resources.getString("country.Suriname"));
        countries.add(resources.getString("country.SvalbardandJanMayenIslands"));
        countries.add(resources.getString("country.Swaziland"));
        countries.add(resources.getString("country.Sweden"));
        countries.add(resources.getString("country.Switzerland"));
        countries.add(resources.getString("country.Taiwan"));
        countries.add(resources.getString("country.Tajikistan"));
        countries.add(resources.getString("country.Tanzania"));
        countries.add(resources.getString("country.Thailand"));
        countries.add(resources.getString("country.Togo"));
        countries.add(resources.getString("country.Tokelau"));
        countries.add(resources.getString("country.Tonga"));
        countries.add(resources.getString("country.TrinidadandTobago"));
        countries.add(resources.getString("country.Tunisia"));
        countries.add(resources.getString("country.Turkey"));
        countries.add(resources.getString("country.Turkmenistan"));
        countries.add(resources.getString("country.TurksandCaicosIslands"));
        countries.add(resources.getString("country.Tuvalu"));
        countries.add(resources.getString("country.USMinorOutlyingIslands"));
        countries.add(resources.getString("country.Uganda"));
        countries.add(resources.getString("country.Ukraine"));
        countries.add(resources.getString("country.UnitedArabEmirates"));
        countries.add(resources.getString("country.UnitedKingdom"));
        countries.add(resources.getString("country.UnitedStates"));
        countries.add(resources.getString("country.Uruguay"));
        countries.add(resources.getString("country.Uzbekistan"));
        countries.add(resources.getString("country.Vanuatu"));
        countries.add(resources.getString("country.Venezuela"));
        countries.add(resources.getString("country.Vietnam"));
        countries.add(resources.getString("country.VirginIsland(British)"));
        countries.add(resources.getString("country.VirginIslands(USA)"));
        countries.add(resources.getString("country.WallisandFutunaIslands"));
        countries.add(resources.getString("country.WesternSahara"));
        countries.add(resources.getString("country.WesternSamoa"));
        countries.add(resources.getString("country.Yemen"));
        countries.add(resources.getString("country.Yugoslavia"));
        countries.add(resources.getString("country.Zambia"));
        countries.add(resources.getString("country.Zimbabwe"));
        return countries;
    }

    public Domain getDefaultDomain(List<Domain> domains, String user) {
        Domain defaultDomain = null;
        if (domains != null && user != null && domains.size() > 0) {
            for (Domain d : domains) {
                if (d.getDomainName().toLowerCase().contains
                                                       (user.toLowerCase())) {
                    defaultDomain = d;
                    return defaultDomain;
                }
            }
            defaultDomain = domains.get(0);
        }
        return defaultDomain;
    }

    private void init(String registratorId, String proxyHost, int proxyPort) {
        if (registratorId == null) {
            this.regId = "";
        } else {
            this.regId = registratorId + ":";
        }
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        Locale locale = Locale.getDefault();
        resources = ResourceBundle.getBundle
            ("com.sun.scn.client.resources.bundle.RegistrationWrapper", locale);
        this.useAuth = true;
    }

    private boolean isValidScreenNameFormat(String screenName) {
        if (screenName == null) {
            return false;
        }
        int len = screenName.length();
        if (len < 3 || len > 30) {
            return false;
        }


        for (int i=0; i<len; i++) {
            if (!validChars.contains(screenName.charAt(i))) {
                return false;
            }
        }

        return true;
    }

    private static Set<Character> getValidChars() {
        Set<Character> set = new HashSet<Character>();

        final char chars[] = {
            'a','b','c','d','e','f','g','h','i','j','k','l','m',
            'n','o','p','q','r','s','t','u','v','w','x','y','z',
            'A','B','C','D','E','F','G','H','I','J','K','L','M',
            'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
            '0','1','2','3','4','5','6','7','8','9',
            '.','-','_' };

        for (int i=0; i<chars.length; i++) {
            set.add(chars[i]);
        }

        return set;
    }
}
