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

package com.sun.scn.util;

import java.util.*;
import java.io.*;


/**
 * Utilizes output from PSN collector packages to try to obtain S/N for
 * Solaris systems, and tries to obtain S/N, system manufacturer, and
 * cpu manufacturer for Linux systems.
 *
 * In cases where the PSN collector package has not been previously installed,
 * this tries to mimic the behavior from the PSN collector where possible.
 */
public class PSNCollector {
    private static final int SN  = 1;
    private static final int SYS = 2;
    private static final int CPU = 3;

    private PSNCollector() {
    }

    /**
     * Tries to obtain and return the cpu manufacturer.
     * @return The cpu manufacturer (null if not found or an error occurred)
     */
    public static String getLinuxCPUManufacturer() {
        String tmp = getLinuxPSNInfo(CPU);
        if (tmp != null) {
            return tmp;
        }

        File f = new File("/proc/cpuinfo");
        if (f.exists()) {
            String contents = getFileContents("/proc/cpuinfo");
            if (contents != null) {
                StringTokenizer st = new StringTokenizer(contents, "\n");
                while (st.hasMoreTokens()) {
                    String line = st.nextToken();
                    int indx = line.indexOf("vendor_id");
                    if (indx != -1) {
                        indx = line.indexOf(":");
                        if (indx != -1 && (indx + 1) < line.length()) {
                            return line.substring(indx+1).trim();
                        }
                    }
                }
            }
        }

        // returns null if it can't be found or an error happened
        return getLinuxDMIInfo("dmi type 4", "manufacturer");
    }

    /**
     * Tries to obtain and return the system manufacturer.
     * @return The system manufacturer (null if not found or an error occurred)
     */
    public static String getLinuxSystemManufacturer() {
        String tmp = getLinuxPSNInfo(SYS);
        if (tmp != null) {
            return tmp;
        }

        // returns null if it can't be found or an error happened
        return getLinuxDMIInfo("dmi type 1", "manufacturer");
    }

    /**
     * Tries to obtain and return the serial number of the system.
     * @return The serial number (null if not found or an error occurred)
     */
    public static String getLinuxSN() {
        String tmp = getLinuxPSNInfo(SN);
        if (tmp != null) {
            return tmp;
        }

        // returns null if it can't be found or an error happened
        return getLinuxDMIInfo("dmi type 1", "serial number");
    }

    /**
     * Tries to obtain the serial number.
     * @return The serial number (nul if not found or an error occurred)
     */
    public static String getSolarisSN() {
        String tmpSN = null;

        // try to read from the psn file if it exists
        File f = new File("/var/run/psn");
        if (f.exists()) {
            String tmp = getFileContents("/var/run/psn");
            if (tmp != null) {
                return tmp.trim();
            }
        }

        // if we're here, then we'll try sneep
        tmpSN = getSneepSN();
        if (tmpSN != null) {
            return tmpSN;
        }

        // if we're here, then we'll try smbios (type 1)
        tmpSN = getSmbiosData("1", "Serial Number: ");
        if (tmpSN != null) {
            return tmpSN;
        }

        // if we're here, then we'll try smbios (type 3)
        tmpSN = getSmbiosData("3", "Serial Number: ");
        if (tmpSN != null) {
            return tmpSN;
        }

        // give up and return
        return null;
    }

    /**
     * Tries to obtain the cpu manufacturer.
     * @return The cpu manufacturer (null if not found or an error occurred)
     */
    public static String getSolarisCPUManufacturer() {
        // not fully accurate, this could be another manufacturer (fujitsu for example)
        if ("sparc".equalsIgnoreCase(System.getProperty("os.arch"))) {
            return "Sun Microsystems, Inc";
        }

        // if we're here, then we'll try smbios (type 3)
        String tmpManufacturer = getSmbiosData("3", "Manufacturer: ");
        if (tmpManufacturer != null) {
            return tmpManufacturer;
        }

        // give up and return
        return null;
    }

    /**
     * Tries to obtain the system manufacturer.
     * @return The system manufacturer (null if not found or an error occurred)
     */
    public static String getSolarisSystemManufacturer() {
        // not fully accurate, this could be another manufacturer (fujitsu for example)
        if ("sparc".equalsIgnoreCase(System.getProperty("os.arch"))) {
            return "Sun Microsystems, Inc";
        }

        // if we're here, then we'll try smbios (type 1)
        String tmpManufacturer = getSmbiosData("1", "Manufacturer: ");
        if (tmpManufacturer != null) {
            return tmpManufacturer;
        }

        // give up and return
        return null;
    }

    private static String getLinuxPSNInfo(int target) {
        // try to read from the psn file if it exists
        File f = new File("/var/run/psn");
        if (f.exists()) {
            String contents = getFileContents("/var/run/psn");
            if (contents == null) {
                return null;
            }
            StringTokenizer st = new StringTokenizer(contents, "\n");
            String tmp = contents;
            if (st.countTokens() >= target) {
                for (int i=0; i<target; i++) {
                    tmp = st.nextToken();
                }
                return tmp.trim();
            }
        }

        // default case is to return null
        return null;
    }

    // reads from dmidecode with the given type and target
    // returns null if nothing was found or an error occurred
    private static String getLinuxDMIInfo(String dmiType, String target) {
        String cmd[] = {"/usr/sbin/dmidecode"};
        String output = getAllOutputFromCommandLine(cmd);

        if (output == null) {
            return null;
        }
        StringTokenizer newLineTok = new StringTokenizer(output, "\n");
        boolean dmiFlag = false;
        while (newLineTok.hasMoreTokens()) {
            String line = newLineTok.nextToken();
            if (dmiFlag) {
                int indx = line.toLowerCase().indexOf(target);
                if (indx != -1) {
                    indx = line.toLowerCase().indexOf(target + ":");
                    if (indx != -1 && line.length() > (target.length() + 1)) {
                        return line.substring(indx + target.length() + 1).trim();
                    } else {
                        StringTokenizer colonTok = new StringTokenizer(line, ":");
                        String tmp = line.trim();
                        while (colonTok.hasMoreTokens()) {
                            tmp = colonTok.nextToken().trim();
                        }
                        return tmp;
                    }
                }
            } else if (line.toLowerCase().indexOf(dmiType) != -1) {
                dmiFlag = true;
            }
        }
        return null;
    }

    private static String getFromCommandLine(String cmd[]) {
        String res = null;
        try {
            Runtime rt = Runtime.getRuntime();
            Process proc = rt.exec(cmd);
            int rv = proc.waitFor();

            if (rv == 0) {
                BufferedReader br = new BufferedReader(
                    new InputStreamReader(proc.getInputStream()));
                String line = null;
                if ((line = br.readLine()) != null) {
                    res = line;
                }
                br.close();
            }

        } catch (Exception e) {
            //e.printStackTrace();
        }
        return res;
    }

    private static String getFileContents(String file) {
        StringBuilder sb = new StringBuilder();
        try {
            BufferedReader br = new BufferedReader(new FileReader(file));
            String line = null;
            while ((line = br.readLine()) != null) {
                sb.append(line).append("\n");
            }
            br.close();
        } catch (Exception e) {
            //e.printStackTrace();
        }
        return sb.toString();
    }

    private static String getAllOutputFromCommandLine(String cmd[]) {
        StringBuilder sb = new StringBuilder();
        try {
            Runtime rt = Runtime.getRuntime();
            Process proc = rt.exec(cmd);
            int rv = proc.waitFor();

            if (rv == 0) {
                BufferedReader br = new BufferedReader(
                    new InputStreamReader(proc.getInputStream()));
                String line = null;
                while ((line = br.readLine()) != null) {
                    sb.append(line).append("\n");
                }
                br.close();
            }

        } catch (Exception e) {
            //e.printStackTrace();
        }
        return sb.toString();
    }

    private static String getSmbiosData(String type, String target) {
        String smbiosCMD[] = {"/usr/sbin/smbios","-t",type};
        String output = getAllOutputFromCommandLine(smbiosCMD);

        if (output == null) {
            return null;
        }

        StringTokenizer st = new StringTokenizer(output, "\n");
        while (st.hasMoreTokens()) {
            String line = st.nextToken();
            String tok = target;
            int indx = line.indexOf(tok);
            if (indx != -1) {
                String tmp = line.substring(tok.length()+1).trim();
                if (tmp != null) {
                    if (!tmp.equalsIgnoreCase("not available")
                            && !tmp.equalsIgnoreCase("to be filled by o.e.m")) {
                        return tmp;
                    }
                }
            }
        }

        return null;
    }

    private static String getSneepSN() {
        String basedirCMD[] = {"pkgparam","SUNWsneep","BASEDIR"};
        String basedir = getFromCommandLine(basedirCMD);
        if (basedir == null) {
            return null;
        }

        String sneepCMD[] = {basedir + "/bin/sneep"};
        String sneepSN = getFromCommandLine(sneepCMD);

        if (sneepSN == null || sneepSN.equalsIgnoreCase("unknown")) {
            return null;
        }

        return sneepSN;
    }
}
