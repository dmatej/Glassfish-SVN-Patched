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

package com.sun.scn.servicetags;

import com.sun.scn.servicetags.util.XMLUtil;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import org.xml.sax.InputSource;

import java.io.StringReader;
import java.io.UnsupportedEncodingException;

import java.net.URLEncoder;

import java.text.DateFormat;
import java.text.SimpleDateFormat;

import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;


/**
 * Respresents a Service Tag agent and the system on which it runs.
 */
public class Agent implements Comparable<Agent> {
    private ArrayList<SvcTag> svcTags = new ArrayList<SvcTag>();
    private ArrayList<String> svcTagURNs = new ArrayList<String>();

    public String agentURN;
    public String receivedIPAddress;
    public Date receivedTimestamp;
    public Date timestamp = new Date();
    private int port; // Port number the actual agent is running on
    public String ipAddress; // IP address of the actual agent's message
    public String host;
    public String system;
    public String release;
    public String machine;
    public String architecture;
    public String platform;
    public String manufacturer;
    public String cpuManufacturer;
    public String serialNumber;
    public String hostid;
    public String agentVersion;
    public String helperVersion;

    public String physmem = "";
    public String sockets = "";
    public String cores = "";
    public String virtcpus = "";
    public String cpuname = "";
    public String clockrate = "";

    /**
     * TODO: make private?
     */
    public Agent(String agentURN) {
        this.agentURN = agentURN;
    }

    public static Agent getNew(String agentURN) {
        return new Agent(agentURN);
    }

    /**
     * Sets the state of an Agent using the contents of the
     * supplied DOM Element.
     */
    public void setState(Element e) {
        ipAddress = XMLUtil.getOptionalTextValue(e, "ip_address");
        agentVersion = XMLUtil.getOptionalTextValue(e, "agent_version");
        helperVersion = XMLUtil.getOptionalTextValue(e, "registry_version");

        NodeList sysInfoNodeList = e.getElementsByTagName("system_info");
        assert (sysInfoNodeList.getLength() == 1);

        Element e2 = (Element) (sysInfoNodeList.item(0));

        host = XMLUtil.getRequiredTextValue(e2, "host");
        system = XMLUtil.getRequiredTextValue(e2, "system");
        release = XMLUtil.getRequiredTextValue(e2, "release");
        architecture = XMLUtil.getRequiredTextValue(e2, "architecture");
        platform = XMLUtil.getRequiredTextValue(e2, "platform");
        manufacturer = XMLUtil.getRequiredTextValue(e2, "manufacturer");
        if (manufacturer != null) {
            manufacturer = manufacturer.replace("_"," ");
        }

        cpuManufacturer = XMLUtil.getOptionalTextValue(e2, "cpu_manufacturer");
        if (cpuManufacturer != null) {
            cpuManufacturer = cpuManufacturer.replace("_"," ");
        }

        serialNumber = XMLUtil.getOptionalTextValue(e2, "serial_number");
        if (serialNumber == null) {
            serialNumber = "";
        }

        hostid = XMLUtil.getOptionalTextValue(e2, "hostid");
        if (hostid == null) {
            hostid = "";
        }

        physmem = XMLUtil.getOptionalTextValue(e2, "physmem");
        if (physmem == null) {
            physmem = "";
        }
        sockets = "";
        cores = "";
        virtcpus = "";
        cpuname = "";
        clockrate = "";

        NodeList cpuinfoNodeList = e.getElementsByTagName("cpuinfo");
        if (cpuinfoNodeList == null || cpuinfoNodeList.getLength() == 0) {
            return;
        }
        Element e3 = (Element) (cpuinfoNodeList.item(0));

        sockets = XMLUtil.getOptionalTextValue(e2, "sockets");
        if (sockets == null) {
            sockets = "";
        }

        cores = XMLUtil.getOptionalTextValue(e2, "cores");
        if (cores == null) {
            cores = "";
        }

        virtcpus = XMLUtil.getOptionalTextValue(e2, "virtcpus");
        if (virtcpus == null) {
            virtcpus = "";
        }

        cpuname = XMLUtil.getOptionalTextValue(e2, "name");
        if (cpuname == null) {
            cpuname = "";
        }

        clockrate = XMLUtil.getOptionalTextValue(e2, "clockrate");
        if (clockrate == null) {
            clockrate = "";
        }

    }

    public void addSvcTagURN(String svcTagURN) {
        svcTagURNs.add(svcTagURN);
    }

    public synchronized void addSvcTag(SvcTag svcTag) {
        svcTags.add(svcTag);
    }

    public void setPort(int port) {
        this.port = port;
    }

    public int getPort() {
        return port;
    }

    public String getAgentURN() {
        return agentURN;
    }

    public String getHostid() {
        return hostid;
    }

    public String getSerialNumber() {
        return serialNumber;
    }

    public String getCPUManufacturer() {
        return cpuManufacturer;
    }

    public String getAgentVersion() {
        return agentVersion;
    }

    public String getHelperVersion() {
        return helperVersion;
    }

    /**
     * Gets the IP address from which the current Agent state was
     * received (such as src IP of the Collector which sent the last
     * update to the Catcher).
     *
     * TODO: move elsewhere, since this may not be truly common?
     */
    public String getReceivedIPAddress() {
        return receivedIPAddress;
    }

    /**
     * Gets the time when the current Agent state was received (such as
     * when the Catcher got its last update from a Collector).
     *
     * TODO: move elsewhere, since this may not be truly common?
     */
    public Date getReceivedTimestamp() {
        // Need to return a clone, since Date is mutable
        return (Date) (receivedTimestamp.clone());
    }

    public Date getTimestamp() {
        // Need to return a clone, since Date is mutable
        return (Date) (timestamp.clone());
    }

    public String getTimestampString() {
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss z");

        return df.format(timestamp);
    }

    public String getIPAddress() {
        return ipAddress;
    }

    public String getHost() {
        return host;
    }

    public String getSystem() {
        return system;
    }

    public String getRelease() {
        return release;
    }

    public String getMachine() {
        return machine;
    }

    public String getArchitecture() {
        return architecture;
    }

    public String getPlatform() {
        return platform;
    }

    public String getManufacturer() {
        return manufacturer;
    }

    // TODO: align w/ getSvcTags
    public List<String> getSvcTagURNs() {
        // return a copy so that consumers can't add to our internal list
        return new ArrayList<String>(svcTagURNs);
    }

    // TODO: align w/ getSvcTagURNs
    public List<SvcTag> getSvcTags() {
        // return a copy so that consumers can't add to our internal list
        return new ArrayList<SvcTag>(svcTags);
    }

    public Element toXMLElement() throws Exception {
        return toXMLElement(false);
    }

    public Element toXMLElement(boolean deep) throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();

        StringBuilder sb = new StringBuilder();
        sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
          .append("<st1:request xmlns:st1=\"http://www.sun.com/stv1/agent\">\n")
          .append(toXMLString(deep)).append("</st1:request>\n");

        InputSource is = new InputSource(new StringReader(sb.toString()));
        Document doc = builder.parse(is);

        return doc.getDocumentElement();
    }

    public String toXMLString() {
        return toXMLString(false);
    }

    public String toXMLString(boolean deep) {
        StringBuilder sb = new StringBuilder();
        Formatter fmt = new Formatter(sb);

        fmt.format("<agent>\n");

        fmt.format("  <agent_urn>%s</agent_urn>\n", agentURN);
        //fmt.format("  <ip_address>%s</ip_address>\n", ipAddress);
        fmt.format("  <agent_timestamp>%s</agent_timestamp>\n",
            getTimestampString());

        fmt.format("  <system_info>\n");
        fmt.format("    <system>%s</system>\n", encode(system));
        fmt.format("    <host>%s</host>\n", encode(host));
        fmt.format("    <release>%s</release>\n", encode(release));
        fmt.format("    <architecture>%s</architecture>\n", encode(architecture));
        fmt.format("    <platform>%s</platform>\n", encode(platform));
        fmt.format("    <manufacturer>%s</manufacturer>\n", encode(manufacturer));
        fmt.format("    <cpu_manufacturer>%s</cpu_manufacturer>\n",
            encode(cpuManufacturer));

        if (serialNumber != null && !serialNumber.equals("")) {
            fmt.format("    <serial_number>%s</serial_number>\n", encode(serialNumber));
        }

        if (hostid != null && !hostid.equals("")) {
            fmt.format("    <hostid>%s</hostid>\n", encode(hostid));
        }

        if (physmem != null && !physmem.equals("")) {
            fmt.format("    <physmem>%s</physmem>\n", encode(physmem));
        }
        if (sockets != null && !sockets.equals("")
                || cores != null && !cores.equals("")
                || virtcpus != null && !virtcpus.equals("")
                || cpuname != null && !cpuname.equals("")
                || clockrate != null && !clockrate.equals("")) {
            fmt.format("    <cpuinfo>\n");
            fmt.format("      <sockets>%s</sockets>\n", encode(sockets));
            fmt.format("      <cores>%s</cores>\n", encode(cores));
            fmt.format("      <virtcpus>%s</virtcpus>\n", encode(virtcpus));
            fmt.format("      <name>%s</name>\n", encode(cpuname));
            fmt.format("      <clockrate>%s</clockrate>\n", encode(clockrate));
            fmt.format("    </cpuinfo>\n");
        }

        fmt.format("  </system_info>\n");

        if (deep == false) {
            for (String svcTagURN : svcTagURNs) {
                fmt.format("    <service_tag_urn>%s</service_tag_urn>\n",
                    svcTagURN);
            }
        } else {
            for (SvcTag svcTag : svcTags) {
                if (svcTag.getAgentVersion() == null
                        || svcTag.getAgentVersion().equals("")) {
                    svcTag.setAgentVersion(getAgentVersion());
                }
                if (svcTag.getHelperVersion() == null
                        || svcTag.getHelperVersion().equals("")) {
                    svcTag.setHelperVersion(getHelperVersion());
                }
                fmt.format("%s", svcTag.toXMLString());
            }
        }

        fmt.format("</agent>\n");

        return sb.toString();
    }

    public String toXMLString(List<SvcTag> tags) {
        StringBuilder sb = new StringBuilder();
        Formatter fmt = new Formatter(sb);

        fmt.format("<agent>\n");

        fmt.format("  <agent_urn>%s</agent_urn>\n", agentURN);
        fmt.format("  <agent_timestamp>%s</agent_timestamp>\n",
            getTimestampString());

        fmt.format("  <system_info>\n");
        fmt.format("    <system>%s</system>\n", encode(system));
        fmt.format("    <host>%s</host>\n", encode(host));
        fmt.format("    <release>%s</release>\n", encode(release));
        fmt.format("    <architecture>%s</architecture>\n", encode(architecture));
        fmt.format("    <platform>%s</platform>\n", encode(platform));
        fmt.format("    <manufacturer>%s</manufacturer>\n", encode(manufacturer));
        fmt.format("    <cpu_manufacturer>%s</cpu_manufacturer>\n",
            encode(cpuManufacturer));

        if (physmem != null && !physmem.equals("")) {
            fmt.format("    <physmem>%s</physmem>\n", encode(physmem));
        }
        if (sockets != null && !sockets.equals("")
                || cores != null && !cores.equals("")
                || virtcpus != null && !virtcpus.equals("")
                || cpuname != null && !cpuname.equals("")
                || clockrate != null && !clockrate.equals("")) {
            fmt.format("    <cpuinfo>\n");
            fmt.format("      <sockets>%s</sockets>\n", encode(sockets));
            fmt.format("      <cores>%s</cores>\n", encode(cores));
            fmt.format("      <virtcpus>%s</virtcpus>\n", encode(virtcpus));
            fmt.format("      <name>%s</name>\n", encode(cpuname));
            fmt.format("      <clockrate>%s</clockrate>\n", encode(clockrate));
            fmt.format("    </cpuinfo>\n");
        }

        fmt.format("  </system_info>\n");

        for (SvcTag svcTag : tags) {
            fmt.format(svcTag.toXMLString());
        }

        fmt.format("</agent>\n");

        return sb.toString();
    }

    public int compareTo(Agent agent) {
        return getHost().compareTo(agent.getHost());
    }

    public boolean equals(Object obj) throws ClassCastException {
        if (obj == null) {
            return false;
        }
        return ((Agent) obj).getAgentURN().equals(this.getAgentURN());
    }

    public int hashCode() {
        return this.getAgentURN().hashCode();
    }

    public String encode(String s) {
        if (s == null) {
            return s;
        }

        try {
            return URLEncoder.encode(s, "UTF-8");
        } catch (UnsupportedEncodingException uee) {
            return s;
        }
    }
}
