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

package com.sun.scn.client.registry;

import com.sun.scn.servicetags.SvcTag;
import com.sun.scn.servicetags.util.XMLUtil;
import com.sun.scn.util.XMLErrorSuppressor;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import org.xml.sax.SAXException;

/**
 * Provides the ability to import the local registry.
 */
public class ServiceTagRegistry {
    private final static String solarisSTClient =
        "/usr/bin/stclient";

    private final static String linuxSTClient =
        "/opt/sun/servicetag/bin/stclient";

    private final static String windowsSTClient =
        "C:\\Program Files\\Sun\\servicetag\\stclient.exe";

    public final static String Solaris8URN =
        "urn:uuid:a7a38948-2bd5-11d6-98ce-9d3ac1c0cfd7";
    public final static String Solaris9URN =
        "urn:uuid:4f82caac-36f3-11d6-866b-85f428ef944e";
    public final static String Solaris10URN =
        "urn:uuid:5005588c-36f3-11d6-9cec-fc96f718e113";
    public final static String Solaris11URN =
        "urn:uuid:6df19e63-7ef5-11db-a4bd-080020a9ed93";

    private String registryURN;
    private String registryVersion;
    private List<SvcTag> serviceTags;
    private boolean isLoaded = false;

    public static void main(String args[]) {
        try {
            ServiceTagRegistry str = ServiceTagRegistry.loadLocalRegistry();
            for (SvcTag svctag : str.getServiceTags()) {
                System.out.println(svctag.toXMLString());
            }
            System.out.println("Registry urn: " + str.getRegistryURN());
            System.out.println("Version: " + str.getRegistryVersion());

            SvcTag osTag = str.getSolarisServiceTag();
            System.out.println(osTag.toXMLString());
        } catch (ServiceTagRegistryException stre) {
            stre.printStackTrace();
        }
    }

    private ServiceTagRegistry() {}

    /**
     * Attempts to load the local service tag registry using the system's
     * predefined stclient.
     *
     * @return The ServiceTagRegistry
     * @throws ServiceTagRegistryException if there was a problem loading the
     * registry.
     */
    public static ServiceTagRegistry loadLocalRegistry()
            throws ServiceTagRegistryException {
        ServiceTagRegistry str = new ServiceTagRegistry();

        String stclientCmd = null;

        try {
            stclientCmd = getSTClientCommand();
        } catch (FileNotFoundException e) {
            throw new ServiceTagRegistryException(e.getMessage());
        }

        str.load(stclientCmd, "-x");
        return str;
    }

    /**
     * Attempts to load the local service tag registry using the provided
     * stclient.
     *
     * @param stclient The full path and executable of the stclient binary.
     * @return The ServiceTagRegistry
     * @throws ServiceTagRegistryException if there was a problem loading the
     * registry.
     */
    public static ServiceTagRegistry loadLocalRegistry(String stclient)
            throws ServiceTagRegistryException {
        ServiceTagRegistry str = new ServiceTagRegistry();
        str.load(stclient, "-x");
        return str;
    }

    /**
     * Attempts to load the local non-privileged service tag registry contained
     * in the specified registry location.
     *
     * @param location The full path and location of the registry.
     * @return The ServiceTagRegistry
     * @throws ServiceTagRegistryException if there was a problem loading the
     * registry.
     */
    public static ServiceTagRegistry loadLocalNonPrivilegedRegistry(String location)
            throws ServiceTagRegistryException {
        if (true) throw new RuntimeException("Method not implemented");
        ServiceTagRegistry str = new ServiceTagRegistry();
        //str.load(stclient, "-x");
        return str;
    }

    /**
     * Attempts to load the local non-privileged service tag registry contained
     * in the user's home directory.
     *
     * @return The ServiceTagRegistry
     * @throws ServiceTagRegistryException if there was a problem loading the
     * registry.
     */
    public static ServiceTagRegistry loadLocalNonPrivilegedRegistry()
            throws ServiceTagRegistryException {
        if (true) throw new RuntimeException("Method not implemented");
        ServiceTagRegistry str = new ServiceTagRegistry();
        //str.load(stclient, "-x");
        return str;
    }

    /**
     * Returns the version obtained from the local registry.
     *
     * @throws RegistryNotLoadedException if the registry has not been
     * loaded.
     */
    public String getRegistryVersion() throws RegistryNotLoadedException {
        if (!isLoaded) {
            throw new
                RegistryNotLoadedException("Registry has not been loaded");
        }
        return registryVersion;
    }

    /**
     * Returns the registry URN obtained from the local registry.  Will
     * return null if no registry URN was found (this is the case with
     * service tags 1.0).
     *
     * @throws RegistryNotLoadedException if the registry has not been
     * loaded.
     */
    public String getRegistryURN() throws RegistryNotLoadedException {
        if (!isLoaded) {
            throw new
                RegistryNotLoadedException("Registry has not been loaded");
        }
        return registryURN;
    }

    /**
     * Returns the list of service tags obtained from the local registry.
     *
     * @throws RegistryNotLoadedException if the registry has not been
     * loaded.
     */
    public List<SvcTag> getServiceTags() throws RegistryNotLoadedException {
        if (!isLoaded) {
            throw new
                RegistryNotLoadedException("Registry has not been loaded");
        }
        return serviceTags;
    }

    /**
     * Returns the service tag obtained from the local registry that corresponds
     * to the service tag for the Solaris Operating System.
     *
     * @throws RegistryNotLoadedException if the registry has not been
     * loaded.
     */
    public SvcTag getSolarisServiceTag() throws RegistryNotLoadedException {
        if (!isLoaded) {
            throw new
                RegistryNotLoadedException("Registry has not been loaded");
        }

        if (serviceTags == null) {
            return null;
        }

        for (SvcTag svctag : serviceTags) {
            if (svctag.getProductURN().equals(Solaris8URN)
                    || svctag.getProductURN().equals(Solaris9URN)
                    || svctag.getProductURN().equals(Solaris10URN)
                    || svctag.getProductURN().equals(Solaris11URN)) {
                return svctag;
            }
        }

        return null;
    }

    private void setRegistryVersion(String registryVersion) {
        this.registryVersion = registryVersion;
    }

    private void setRegistryURN(String registryURN) {
        this.registryURN = registryURN;
    }

    private void setServiceTags(List<SvcTag> serviceTags) {
        this.serviceTags = serviceTags;
    }

    private void load(String command, String commandOption)
            throws ServiceTagRegistryException {
        try {
            String cmd[] = {command, commandOption};
            Process proc = Runtime.getRuntime().exec(cmd);
            int rc = proc.waitFor();
            if (rc != 0) {
                throw new ServiceTagRegistryException(
                    "Unable to load registry, return code: " + rc);
            }
            List<SvcTag> svcTags = null;

            DocumentBuilderFactory factory =
                DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();

            builder.setErrorHandler(new XMLErrorSuppressor());

            Document doc = builder.parse(proc.getInputStream());
            NodeList regNodeList = doc.getElementsByTagName("registry");
            if ((regNodeList != null) && (regNodeList.getLength() > 0)) {
                Element registry = (Element) (regNodeList.item(0));
                String registryURN = registry.getAttribute("urn");
                if (registryURN != null) {
                    if (registryURN.trim().equals("")) {
                        registryURN = null;
                    }
                }
                setRegistryURN(registryURN);

                String registryVersion = registry.getAttribute("version");
                setRegistryVersion(registryVersion);
            }

            NodeList stNodeList = doc.getElementsByTagName("service_tag");
            svcTags = new ArrayList<SvcTag>();

            if ((stNodeList != null) && (stNodeList.getLength() > 0)) {

                for (int i = 0; i < stNodeList.getLength(); i++) {
                    Element svcTag = (Element) (stNodeList.item(i));
                    SvcTag st = new SvcTag(XMLUtil.getRequiredTextValue(svcTag,
                        "instance_urn"));
                    st.setState(svcTag);
                    svcTags.add(st);
                }
            }

            setServiceTags(svcTags);
        } catch (Exception e) {
            throw new ServiceTagRegistryException(
                "Unable to load registry, message: " + e.getMessage());
        }
        isLoaded = true;
    }

    private static String getSTClientCommand() throws FileNotFoundException {
        String osName = System.getProperty("os.name");
        String cmd = null;
        if (osName == null) {
            throw new FileNotFoundException("No stclient found for os: "
                + osName);
        } else if (osName.equalsIgnoreCase("sunos")) {
            cmd = solarisSTClient;
        } else if (osName.equalsIgnoreCase("linux")) {
            cmd = windowsSTClient;
        } else if (osName.equalsIgnoreCase("windows")) {
            cmd = linuxSTClient;
        } else {
            throw new FileNotFoundException("No stclient found for os: "
                + osName);
        }
        File f = new File(cmd);
        if (!f.exists()) {
            throw new FileNotFoundException("No stclient found at expected location: "
                + cmd);
        }
        return cmd;
    }
}
