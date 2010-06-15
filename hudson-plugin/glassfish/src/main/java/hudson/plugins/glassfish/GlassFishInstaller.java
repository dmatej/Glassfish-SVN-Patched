/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
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


package hudson.plugins.glassfish;

import hudson.FilePath;
import hudson.model.AbstractBuild;

import java.io.IOException;
import java.io.PrintStream;
import java.io.File;
import java.net.URL;

/**
 * @author Harshad Vilekar
 */
@SuppressWarnings("deprecation")
public final class GlassFishInstaller {

    //define PATHs relative to project workspace
    public static final String GFV3HOME_DIR = "glassfishv3/glassfish/";
    public static final String GFV3BIN_DIR = GFV3HOME_DIR + "bin/";
    public static final String GFV3ASADMIN_CMD = GFV3BIN_DIR + "asadmin";

    private static final String GFV3PASSWORD_FILE = GFV3HOME_DIR + "config/passwordfile";
    private static final String USER_PASSWORD_FLAGS = " --passwordfile " + GFV3PASSWORD_FILE + " --user admin ";

    private  AbstractBuild build;
    private  PrintStream logger;

    public GlassFishInstaller(AbstractBuild build, PrintStream logger) {
        this.build = build;
        this.logger = logger;
    }

    /**
     * Install GlassFish: Simply Get GlassFish .zip bundle from the specified URL
     * and unzip the file.
     */
    public boolean installGlassFishFromZipBundle(String GFZipBundleURLString) {

        try {
            FilePath projectWorkspace = build.getProject().getWorkspace();

            // download / copy the build
            URL GFZipBundleURL = new URL(GFZipBundleURLString);
            String fileName = new File(GFZipBundleURL.getPath()).getName();
            logger.println("Copying the file: " + GFZipBundleURLString);
            FilePath zipFile = new FilePath(projectWorkspace, fileName);
            zipFile.copyFrom(GFZipBundleURL);

            // delete all the traces of earlier installation
            FilePath installDir = new FilePath(projectWorkspace, GFV3HOME_DIR);
            if (installDir.exists()) {
                logger.println("Deleting earlier installation from: " + installDir.toURI().toString());
                installDir.deleteContents();
                installDir.delete();
            }

            logger.println("Unzipping " + fileName + " at: " + projectWorkspace.toURI().toString());

            zipFile.unzip(projectWorkspace);

            // After the bundle is unzippd, asadmin command files are missing execute permission.
            // To workaround, explicitly add execute permission to the required GF admin commands
            FilePath cmdFile = new FilePath(projectWorkspace, GFV3BIN_DIR + "asadmin");
            // b001001001 represets execute permission to all on Unix
            // Do a "bitwise inclusive OR operation" to assign the exec permission
            cmdFile.chmod(cmdFile.mode() | Integer.parseInt("001001001", 2));

            // password file may be required for executing asadmin command
            String CMD = "createGFPassWordFile()";
            if (!createGFPassWordFile(build, logger)) {
                logger.println("ERROR: " + CMD);
                return false;
            }
            return true;

        } catch (IOException e) {
            e.printStackTrace();
            logger.println("IOException !");
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.println("InterruptedException!");
            return false;
        }
    }

    private boolean createGFPassWordFile(AbstractBuild build, PrintStream logger) {

        FilePath projectWorkspace = build.getProject().getWorkspace();
        FilePath passwordFile = new FilePath(projectWorkspace, GFV3PASSWORD_FILE);
        String passwd = "AS_ADMIN_PASSWORD=adminadmin\n"
                + "AS_ADMIN_MASTERPASSWORD=changeit\n";

        try {
            passwordFile.write(passwd, null);

        } catch (IOException e) {
            e.printStackTrace();
            logger.println("IOException !");
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.println("InterruptedException!");
            return false;
        }

        return true;
    }
}
