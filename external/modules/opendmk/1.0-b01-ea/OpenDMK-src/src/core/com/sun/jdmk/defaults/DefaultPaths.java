/*
 * @(#)file      DefaultPaths.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.17
 * @(#)lastedit      07/03/08
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


package com.sun.jdmk.defaults;


// java import
import java.io.File;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.StringTokenizer;
import java.net.URL;
import java.net.URLDecoder;

// jdmk import
import com.sun.jdmk.internal.ClassLogger;


/** 
 * This class represents a set of default directories used by Java DMK.
 *
 * @since Java DMK 5.0
 */
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
     * <CODE>&lt;base_dir&gt; + File.separator + SUNWjdmk + File.separator + 5.1 </CODE>
     *
     * @return Java DMK installation directory.
     */
    public static String getInstallDir() {
	logger.trace("getInstallDir", "starts");
        if (installDir == null)
            return useCodeSource();
        else
            return installDir;
    }

    /**
     * Returns the installation directory for Java DMK concatenated with dirname.
     *
     * The default value of the installation directory is:
     * <CODE>&lt;base_dir&gt; + File.separator + SUNWjdmk + File.separator + 5.1 </CODE>
     *
     * @param dirname The directory to be appended.
     *
     * @return Java DMK installation directory + <CODE>File.separator</CODE> + <CODE>dirname</CODE>.
     */
    public static String getInstallDir(String dirname) {
        if (installDir == null) {
            if (dirname == null) {
                return getInstallDir();
            } else {
                return getInstallDir() + File.separator + dirname;
            }
        } else {
            if (dirname == null) {
                return installDir;
            } else {
                return installDir + File.separator + dirname;
            }
        }
    }

    /**
     * Sets the installation directory for Java DMK.
     *
     * @param dirname The directory where Java DMK resides.
     */
    public static void setInstallDir(String dirname) {    
        installDir = dirname;
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
     */
    public static String getEtcDir() {
        if (etcDir == null)
            return getInstallDir("etc");
        else
            return etcDir;
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
     */
    public static String getEtcDir(String dirname) {
        if (etcDir == null) {
            if (dirname == null) {
                return getEtcDir();
            } else {
                return getEtcDir() + File.separator + dirname;
            }
        } else {
            if (dirname == null) {
                return etcDir;
            } else {
                return etcDir + File.separator + dirname;
            }
        }
    }

    /**
     * Sets the <CODE>etc</CODE> directory for Java DMK.
     *
     * @param dirname The <CODE>etc</CODE> directory for Java DMK.
     */
    public static void setEtcDir(String dirname) {    
        etcDir = dirname;
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
     */
    public static String getTmpDir() {
	 if (tmpDir == null)
            return getInstallDir("tmp");
        else
            return tmpDir;
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
     */
    public static String getTmpDir(String dirname) {
        if (tmpDir == null) {
            if (dirname == null) {
                return getTmpDir();
            } else {
                return getTmpDir() + File.separator + dirname;
            }
        } else {
            if (dirname == null) {
                return tmpDir;
            } else {
                return tmpDir + File.separator + dirname;
            }
        }
    }

    /**
     * Sets the <CODE>tmp</CODE> directory for the product
     *
     * @param dirname The <CODE>tmp</CODE> directory for Java DMK.
     */
    public static void setTmpDir(String dirname) {
        tmpDir = dirname;
    }


    // PRIVATE STATIC METHODS
    //-----------------------

    private static String useCodeSource() {
	try {
	    String thisCodePath = null;
	    try {
		URL thisCodeLocation = (URL)
		    AccessController.doPrivileged(new PrivilegedAction() {
			    public Object run() {
				return DefaultPaths.class.getProtectionDomain()
				    .getCodeSource().getLocation();
			    }
			});
		if (thisCodeLocation.getProtocol().equalsIgnoreCase("file")) {
		    try {
			thisCodePath = 
			    URLDecoder.decode(thisCodeLocation.getPath(),
					      "UTF-8");
		    } catch (java.io.UnsupportedEncodingException x) {
			logger.warning("useCodeSource", 
				       "Can't decode code base location: " +
				       thisCodeLocation + "\n\t\tError is: "
				       + x);
			logger.finest("useCodeSource",x);
		    }
		} else {
		    logger.warning("useCodeSource",
				   "This code base location is " +
				   "not a file protocol URL: " + 
				   thisCodeLocation);
		}
		logger.fine("useCodeSource","CodeBase=" + thisCodeLocation);
		logger.fine("useCodeSource","CodePath=" + thisCodePath);
	    } catch (SecurityException se) {
		// Missing RuntimePermission("getProtectionDomain")
		// thisCodePath stays null => installDir will be unchanged
		logger.warning("useCodeSource", se);
	    }
	    if (thisCodePath != null) {
		try {
		    // Location of this code is assumed to be in a jar
		    // in a subdir of install dir (normally lib/jdmkrt.jar)
		    final File parent =
			new File(thisCodePath).getParentFile().getParentFile();
		    if (parent != null) {
			boolean parentExists = ((Boolean)
		        AccessController.doPrivileged(new PrivilegedAction() {
				public Object run() {
				    return new Boolean(parent.exists());
                                }
			    })).booleanValue();
			if (parentExists) {
			    installDir = parent.getPath();
			}
		    }
		} catch (SecurityException se) {
		    // Missing permission to check read
		    // => installDir will be unchanged
		    logger.warning("useCodeSource", se);
		}
	    }
	} catch (RuntimeException r) {
	    logger.fine("useCodeSource",r);
	}
	if (installDir == null) {
	    logger.warning("getInstallDir",
			   "Couldn't find JDMK install dir");
	} else {
	    logger.config("getInstallDir","JDMK_INSTALL_DIR="+installDir);
	}
	return installDir;
    }

    // PRIVATE VARIABLES
    //------------------

    private static final ClassLogger logger =
	new ClassLogger(ClassLogger.LOGGER_MISC, "DefaultPaths");

    /**
     * Directories used by Java DMK.
     */
    private static String etcDir;
    private static String tmpDir;
    private static String installDir;
}
