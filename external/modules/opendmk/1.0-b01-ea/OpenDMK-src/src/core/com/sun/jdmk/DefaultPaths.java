/*
 * @(#)file      DefaultPaths.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.21
 * @(#)date      07/04/04
 *
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright (c) 2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU General
 * Public License Version 2 only ("GPL") or the Common Development and
 * Distribution License("CDDL")(collectively, the "License"). You may not use
 * this file except in compliance with the License. You can obtain a copy of the
 * License at http://opendmk.dev.java.net/legal_notices/licenses.txt or in the 
 * LEGAL_NOTICES folder that accompanied this code. See the License for the 
 * specific language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file found at
 *     http://opendmk.dev.java.net/legal_notices/licenses.txt
 * or in the LEGAL_NOTICES folder that accompanied this code.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.
 * 
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * 
 *       "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding
 * 
 *       "[Contributor] elects to include this software in this distribution
 *        under the [CDDL or GPL Version 2] license."
 * 
 * If you don't indicate a single choice of license, a recipient has the option
 * to distribute your version of this file under either the CDDL or the GPL
 * Version 2, or to extend the choice of license to its licensees as provided
 * above. However, if you add GPL Version 2 code and therefore, elected the
 * GPL Version 2 license, then the option applies only if the new code is made
 * subject to such option by the copyright holder.
 * 
 */

package com.sun.jdmk;


// java import
//
import java.io.File;
import java.util.StringTokenizer;

/** 
 * This class represents a set of default directories used by Java DMK.
 * @deprecated Use {@link com.sun.jdmk.defaults.DefaultPaths
 * com.sun.jdmk.defaults.DefaultPaths} instead.
 */
/* We stutter com.sun.etc in the @link because otherwise Javadoc truncates
   it to "DefaultPaths", which is also the name of this class and hence
   confusing.  */
public class DefaultPaths {

    // private constructor defined to "hide" the default public constructor
    private DefaultPaths() {
	
    }
    
    // PUBLIC STATIC METHODS
    //----------------------

    /**
     * Returns the installation directory for Java DMK.
     *
     * The default value of the installation directory is:
     * <UL>
     * <LI><CODE>&lt;base_dir&gt; + File.separator + SUNWjdmk + File.separator + 5.1 </CODE> or,
     * <LI><CODE>System.getProperty("user.dir")</CODE> if the JDMK installation directory could not be derived from the <CODE>CLASSPATH</CODE>.
     * </UL>
     *
     * @return Java DMK installation directory.
     * @deprecated use {@link com.sun.jdmk.defaults.DefaultPaths
     * com.sun.jdmk.defaults.DefaultPaths} instead.
     */
    public static String getInstallDir() {
	return com.sun.jdmk.defaults.DefaultPaths.getInstallDir();
    }

    /**
     * Returns the installation directory for Java DMK concatenated with dirname.
     *
     * The default value of the installation directory is:
     * <UL>
     * <LI><CODE>&lt;base_dir&gt; + File.separator + SUNWjdmk + File.separator + 5.1 </CODE> or,
     * <LI><CODE>System.getProperty("user.dir")</CODE> if the JDMK installation directory could not be derived from the <CODE>CLASSPATH</CODE>.
     * </UL>
     *
     * @param dirname The directory to be appended.
     *
     * @return Java DMK installation directory + <CODE>File.separator</CODE> + <CODE>dirname</CODE>.
     * @deprecated use {@link com.sun.jdmk.defaults.DefaultPaths
     * com.sun.jdmk.defaults.DefaultPaths} instead.
     */
    public static String getInstallDir(String dirname) {
	return com.sun.jdmk.defaults.DefaultPaths.getInstallDir(dirname);
    }

    /**
     * Sets the installation directory for Java DMK.
     *
     * @param dirname The directory where Java DMK resides.
     * @deprecated use {@link com.sun.jdmk.defaults.DefaultPaths
     * com.sun.jdmk.defaults.DefaultPaths} instead.
     */
    public static void setInstallDir(String dirname) {    
	com.sun.jdmk.defaults.DefaultPaths.setInstallDir(dirname);
    }

    /**
     * Returns the <CODE>etc</CODE> directory for Java DMK.
     * <P>
     * The default value of the <CODE>etc</CODE> directory is:
     * <UL>
     * <LI><CODE>DefaultPaths.getInstallDir("etc")</CODE>.
     * </UL>
     *
     * @return Java DMK <CODE>etc</CODE> directory.
     * @deprecated use {@link com.sun.jdmk.defaults.DefaultPaths
     * com.sun.jdmk.defaults.DefaultPaths} instead.
     */
    public static String getEtcDir() {
	return com.sun.jdmk.defaults.DefaultPaths.getEtcDir();
    }

    /**
     * Returns the <CODE>etc</CODE> directory for Java DMK concatenated with dirname.
     * <P>
     * The default value of the <CODE>etc</CODE> directory is:
     * <UL>
     * <LI><CODE>DefaultPaths.getInstallDir("etc")</CODE>.
     * </UL>
     *
     * @param dirname The directory to be appended.
     *
     * @return Java DMK <CODE>etc</CODE> directory + <CODE>File.separator</CODE> + <CODE>dirname</CODE>.
     * @deprecated use {@link com.sun.jdmk.defaults.DefaultPaths
     * com.sun.jdmk.defaults.DefaultPaths} instead.
     */
    public static String getEtcDir(String dirname) {
	return com.sun.jdmk.defaults.DefaultPaths.getEtcDir(dirname);
    }

    /**
     * Sets the <CODE>etc</CODE> directory for Java DMK.
     *
     * @param dirname The <CODE>etc</CODE> directory for Java DMK.
     * @deprecated use {@link com.sun.jdmk.defaults.DefaultPaths
     * com.sun.jdmk.defaults.DefaultPaths} instead.
     */
    public static void setEtcDir(String dirname) {    
	com.sun.jdmk.defaults.DefaultPaths.setEtcDir(dirname);
    }

    /**
     * Returns the <CODE>tmp</CODE> directory for the product.
     * <P>
     * The default value of the <CODE>tmp</CODE> directory is:
     * <UL>
     * <LI><CODE>DefaultPaths.getInstallDir("tmp")</CODE>.
     * </UL>
     *
     * @return Java DMK <CODE>tmp</CODE> directory.
     * @deprecated use {@link com.sun.jdmk.defaults.DefaultPaths
     * com.sun.jdmk.defaults.DefaultPaths} instead.
     */
    public static String getTmpDir() {	
	return com.sun.jdmk.defaults.DefaultPaths.getTmpDir();

    }

    /**
     * Returns the <CODE>tmp</CODE> directory for Java DMK concatenated with dirname.
     * <P>
     * The default value of the <CODE>tmp</CODE> directory is:
     * <UL>
     * <LI><CODE>DefaultPaths.getInstallDir("tmp")</CODE>.
     * </UL>
     *
     * @param dirname The directory to be appended.
     *
     * @return Java DMK <CODE>tmp</CODE> directory + <CODE>File.separator</CODE> + <CODE>dirname</CODE>.
     * @deprecated use {@link com.sun.jdmk.defaults.DefaultPaths
     * com.sun.jdmk.defaults.DefaultPaths} instead.
     */
    public static String getTmpDir(String dirname) {	
	return com.sun.jdmk.defaults.DefaultPaths.getTmpDir(dirname);
    }

    /**
     * Sets the <CODE>tmp</CODE> directory for the product
     *
     * @param dirname The <CODE>tmp</CODE> directory for Java DMK.
     * @deprecated Use {@link com.sun.jdmk.defaults.DefaultPaths
     * com.sun.jdmk.defaults.DefaultPaths} instead.
     */
    public static void setTmpDir(String dirname) {
	com.sun.jdmk.defaults.DefaultPaths.setTmpDir(dirname);
    }


}
