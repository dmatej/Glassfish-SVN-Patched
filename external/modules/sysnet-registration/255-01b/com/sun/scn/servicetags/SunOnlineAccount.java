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
 * Respresents a sun online account.
 */
public class SunOnlineAccount {

    private String userid;
    private String password;
    private String confirmPassword;
    private String lastname;
    private String firstname;
    private String email;
    private String country;
    private String company;
    private String securityQuestion;
    private String securityAnswer;
    private String touVersion;
    private String touResponse;
    private String streetAddress;
    private String streetAddress2;
    private String city;
    private String state;
    private String zip;
    private String screenName;
    private String jobFunction;
    private String school;

    public void setUserid(String s) {
        userid = s;
    }

    public void setPassword(String s) {
        password = s;
    }

    public void setConfirmPassword(String s) {
        confirmPassword = s;
    }

    public void setLastname(String s) {
        lastname = s;
    }

    public void setFirstname(String s) {
        firstname = s;
    }

    public void setEmail(String s) {
        email = s;
    }

    public void setCountry(String s) {
        country = s;
    }

    public void setCompany(String s) {
        company = s;
    }

    public void setSecurityQuestion(String s) {
        securityQuestion = s;
    }

    public void setSecurityAnswer(String s) {
        securityAnswer = s;
    }

    public void setTouVersion(String s) {
        touVersion = s;
    }

    public void setTouResponse(String s) {
        touResponse = s;
    }

    public void setStreetAddress(String s) {
        streetAddress = s;
    }

    public void setStreetAddress2(String s) {
        streetAddress2 = s;
    }

    public void setCity(String s) {
        city = s;
    }

    public void setState(String s) {
        state = s;
    }

    public void setZip(String s) {
        zip = s;
    }

    public void setScreenName(String s) {
        screenName = s;
    }

    public void setIsStudent(boolean b) {
        if (b) {
            jobFunction = "Student";
        } else {
            jobFunction = null;
        }
    }

    public void setSchool(String s) {
        school = s;
    }

    public String getUserid() {
        return userid;
    }

    public String getPassword() {
        return password;
    }

    public String getConfirmPassword() {
        return confirmPassword;
    }

    public String getLastname() {
        return lastname;
    }

    public String getFirstname() {
        return firstname;
    }

    public String getEmail() {
        return email;
    }

    public String getCountry() {
        return country;
    }

    public String getCompany() {
        return company;
    }

    public String getSecurityQuestion() {
        return securityQuestion;
    }

    public String getSecurityAnswer() {
        return securityAnswer;
    }

    public String getTouVersion() {
        return touVersion;
    }

    public String getTouResponse() {
        return touResponse;
    }

    public String getStreetAddress() {
        return streetAddress;
    }

    public String getStreetAddress2() {
        return streetAddress2;
    }

    public String getCity() {
        return city;
    }

    public String getState() {
        return state;
    }

    public String getZip() {
        return zip;
    }

    public String getScreenName() {
        return screenName;
    }

    public boolean isStudent() {
        if (jobFunction != null && jobFunction.equals("Student")) {
            return true;
        }
        return false;
    }

    public String getSchool() {
        return school;
    }

    /**
     * Sets the state of an SOA using the contents of the
     * supplied DOM Element.
     */
    public void setState(Element e) {
        userid = XMLUtil.getRequiredTextValue(e, "userid");
        password = XMLUtil.getRequiredTextValue(e, "password");
        lastname = XMLUtil.getRequiredTextValue(e, "lastname");
        firstname = XMLUtil.getRequiredTextValue(e, "firstname");
        email = XMLUtil.getRequiredTextValue(e, "email");
        country = XMLUtil.getRequiredTextValue(e, "country");
        company = XMLUtil.getOptionalTextValue(e, "company");
        securityQuestion = XMLUtil.getRequiredTextValue(e, "securityQuestion");
        securityAnswer = XMLUtil.getRequiredTextValue(e, "securityAnswer");
        touVersion = XMLUtil.getRequiredTextValue(e, "touVersion");
        touResponse = XMLUtil.getRequiredTextValue(e, "touResponse");
        streetAddress = XMLUtil.getOptionalTextValue(e, "streetAddress");
        streetAddress2 = XMLUtil.getOptionalTextValue(e, "streetAddress2");
        city = XMLUtil.getOptionalTextValue(e, "city");
        state = XMLUtil.getOptionalTextValue(e, "state");
        zip = XMLUtil.getOptionalTextValue(e, "zip");
        screenName = XMLUtil.getOptionalTextValue(e, "screenName");
        jobFunction = XMLUtil.getOptionalTextValue(e, "jobFunction");
        school = XMLUtil.getOptionalTextValue(e, "school");
    }

    public Element toXMLElement() throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();

        InputSource is = new InputSource(new StringReader(toXMLRequestString()));
        Document doc = builder.parse(is);

        return doc.getDocumentElement();
    }

    public String toXMLRequestString() {
        StringBuilder sb = new StringBuilder();
        sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
          .append("<st1:request xmlns:st1=\"http://www.sun.com/stv1/soa\">\n")
          .append(toXMLString()).append("</st1:request>\n");
        return sb.toString();
    }

    public String toXMLString() {
        StringBuilder sb = new StringBuilder();
        Formatter fmt = new Formatter(sb);

        fmt.format("<soa_registration>\n");

        fmt.format("  <userid>%s</userid>\n", userid);
        fmt.format("  <password>%s</password>\n", password);
        fmt.format("  <firstname>%s</firstname>\n", firstname);
        fmt.format("  <lastname>%s</lastname>\n", lastname);
        fmt.format("  <email>%s</email>\n", email);
        fmt.format("  <country>%s</country>\n", country);
        fmt.format("  <company>%s</company>\n", company);
        fmt.format("  <securityQuestion>%s</securityQuestion>\n", securityQuestion);
        fmt.format("  <securityAnswer>%s</securityAnswer>\n", securityAnswer);
        fmt.format("  <touVersion>%s</touVersion>\n", touVersion);
        fmt.format("  <touResponse>%s</touResponse>\n", touResponse);

        if (streetAddress != null) {
            fmt.format("  <streetAddress>%s</streetAddress>\n", streetAddress);
        }

        if (streetAddress2 != null) {
            fmt.format("  <streetAddress2>%s</streetAddress2>\n", streetAddress2);
        }

        if (city != null) {
            fmt.format("  <city>%s</city>\n", city);
        }

        if (state != null) {
            fmt.format("  <state>%s</state>\n", state);
        }

        if (zip != null) {
            fmt.format("  <zip>%s</zip>\n", zip);
        }

        if (screenName != null) {
            fmt.format("  <screenName>%s</screenName>\n", screenName);
        }

        if (jobFunction != null) {
            fmt.format("  <jobFunction>%s</jobFunction>\n", jobFunction);
        }

        if (school != null && isStudent()) {
            fmt.format("  <school>%s</school>\n", school);
        }

        fmt.format("</soa_registration>\n");

        //System.err.println(sb.toString());
        return sb.toString();
    }
}
