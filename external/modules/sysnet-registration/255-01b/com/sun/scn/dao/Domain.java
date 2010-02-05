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

package com.sun.scn.dao;


/*
 * Domain.java
 *
 */
import com.sun.scn.servicetags.util.XMLUtil;

import org.w3c.dom.Element;

import java.util.Formatter;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.net.URLDecoder;

public class Domain implements Comparable<Domain> {
    private int domainId;
    private int parentId;
    private String domainName;
    private String orgName;

    public Domain(int domainId, String domainName, int parentId) {
        this.domainId = domainId;
        this.domainName = domainName;
        this.parentId = parentId;
    }

    public Domain(int domainId, String domainName, boolean isManager, int parentId) {
        this.domainId = domainId;
        this.domainName = domainName;
        this.parentId = parentId;
    }

    public Domain(Element e) {
        try {
            domainId = Integer.parseInt(XMLUtil.getRequiredTextValue(e,
                        "domainId"));
            parentId = Integer.parseInt(XMLUtil.getRequiredTextValue(e,
                        "parentId"));
            domainName = decode(XMLUtil.getRequiredTextValue(e, "domainName"));
        } catch (Exception ex) {
            domainId = Integer.parseInt(XMLUtil.getRequiredTextValue(e,
                        "domainid"));
            parentId = Integer.parseInt(XMLUtil.getRequiredTextValue(e,
                        "domainparentid"));
            domainName = decode(XMLUtil.getRequiredTextValue(e, "domainname"));
        }

        orgName = XMLUtil.getOptionalTextValue(e, "orgName");
    }

    public int getDomainId() {
        return domainId;
    }

    public int getParentId() {
        return parentId;
    }

    public String getDomainName() {
        if (domainName == null) {
            return "";
        }

        return domainName;
    }

    public void setDomainId(int domainId) {
        this.domainId = domainId;
    }

    public void setParentId(int parentId) {
        this.parentId = parentId;
    }

    public void setDomainName(String domainName) {
        if (domainName == null) {
            domainName = "";
        }
        this.domainName = domainName;
    }

    public String getOrgName() {
        if (orgName == null) {
            return "";
        }

        return orgName;
    }

    public String toXML() {
        StringBuilder sb = new StringBuilder();
        Formatter fmt = new Formatter(sb);

        fmt.format("  <domain>\n");
        fmt.format("    <domainid>%d</domainid>\n", getDomainId());
        fmt.format("    <domainname>%s</domainname>\n", encode(getDomainName()));
        fmt.format("    <domainparentid>%d</domainparentid>\n", getParentId());
        fmt.format("  </domain>\n");

        return sb.toString();
    }

    public String toString() {
        return getDomainName();
    }

    public String encode(String s) {
        if (s == null) {
            return s;
        }
        s = s.replace("&","%26");
        s = s.replace("<","%3C");
        s = s.replace(">","%3E");
        return s;
    }

    public String decode(String s) {
        if (s == null) {
            return s;
        }
        try {
            return URLDecoder.decode(s, "UTF-8");
        } catch (UnsupportedEncodingException uee) {
            return s;
        }
    }

    public int compareTo(Domain d) {
        return this.getDomainId() - d.getDomainId();
    }

    public int hashCode() {
        return this.getDomainId();
    }

    public boolean equals(Object obj) {
        if (obj instanceof Domain) {
            Domain d = (Domain) obj;
            return this.getDomainId() == d.getDomainId();
        }
        return false;
    }
}
