/*
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the License).  You may not use this file except in
 * compliance with the License.
 * 
 * You can obtain a copy of the license at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html or
 * glassfish/bootstrap/legal/CDDLv1.0.txt.
 * See the License for the specific language governing 
 * permissions and limitations under the License.
 * 
 * When distributing Covered Code, include this CDDL 
 * Header Notice in each file and include the License file 
 * at glassfish/bootstrap/legal/CDDLv1.0.txt.  
 * If applicable, add the following below the CDDL Header, 
 * with the fields enclosed by brackets [] replaced by
 * you own identifying information: 
 * "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Copyright 2006 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.enterprise.v3.admin;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Bunch of utility methods
 *
 * @author Jerome Dochez
 */
public class Utils {

    public final static Pattern p = Pattern.compile("[^\\$]*([^}]*}).*");

    /**
     * Decode a string containing a ${xxx.yyy.zzz} system property name into
     * a String where the property name is replaced by its value.
     *
     * @param original the string containing the system property name surrounded with ${}
     * @return the decoded string with the system property value
     */
    public static String decode(final String original) {

        Matcher m = p.matcher(original);
        if (m.matches()) {
            String pattern = m.group(1);
            String propName  = pattern.substring(2, pattern.length()-1);
            return original.replace(pattern, System.getProperty(propName));
        }
        return original;
    }

    /**
     * Get a system propety given a property name, possibly trying all combination
     * of upercase, nane mangling to get a value.
     *
     * @param propName the approximate system property name
     * @return the property value if found, null otherwise
     */
    public static String getProperty(String propName) {
        // xxx.yyy
        String value = System.getProperty(propName);
        if (value!=null) {
            return value;
        }
        // XXX.YYY
        value = System.getProperty(propName.toUpperCase());
        if (value!=null) {
            System.setProperty(propName, value);
            return value;
        }
        // xxx_yyy
        value = System.getProperty(propName.replace('.', '_'));
        if (value!=null) {
            System.setProperty(propName, value);
            return value;
        }
        // XXX_YYY
        value = System.getProperty(propName.toUpperCase().replace('.','_'));
        if (value!=null) {
            System.setProperty(propName, value);
        }
        return value;
    }    

}
