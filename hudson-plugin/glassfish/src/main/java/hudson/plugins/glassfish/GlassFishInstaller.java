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
import hudson.model.Executor;
import hudson.model.Computer;
import hudson.remoting.Callable;

import java.io.IOException;
import java.io.PrintStream;
import java.io.File;
import java.net.URL;
import java.util.List;

/**
 * GlassFish Application Server .zip Installer.
 * @author Harshad Vilekar
 */
@SuppressWarnings("deprecation")
public final class GlassFishInstaller {

    //define PATHs relative to project workspace
    static String GFHOME_DIR;
    static String GFBIN_DIR;
    static String GFPASSWORD_FILE;
    static String GFADMIN_CMD;
    static String USER_PASSWORD_FLAGS;
    static String OS_NAME;
    private AbstractBuild build;
    private PrintStream logger;
    private FilePath projectWorkspace, installDir;
    FilePath binDir;
    private String installDirStr;

    public GlassFishInstaller(AbstractBuild build, PrintStream logger) {
        this.build = build;
        this.logger = logger;

        OS_NAME = remoteComputerGetSystemProperty("os.name").toLowerCase();
        logger.println("os.name=" + OS_NAME);


        if (isWindows()) {
            GFHOME_DIR = build.getProject().getWorkspace().toString() + "\\glassfishv3\\glassfish\\";
            GFBIN_DIR = GlassFishInstaller.GFHOME_DIR + "bin\\";
            GFADMIN_CMD = GlassFishInstaller.GFBIN_DIR + "asadmin.bat";

            GFPASSWORD_FILE = GFHOME_DIR + "config\\passwordfile";
            USER_PASSWORD_FLAGS = " --passwordfile " + GFPASSWORD_FILE + " --user admin ";
        } else {
            GFHOME_DIR = build.getProject().getWorkspace().toString() + "/glassfishv3/glassfish/";
            GFBIN_DIR = GlassFishInstaller.GFHOME_DIR + "bin/";
            GFADMIN_CMD = GlassFishInstaller.GFBIN_DIR + "asadmin";

            GFPASSWORD_FILE = GFHOME_DIR + "config/passwordfile";
            USER_PASSWORD_FLAGS = " --passwordfile " + GFPASSWORD_FILE + " --user admin ";
        }

        projectWorkspace = build.getProject().getWorkspace();
        installDir = new FilePath(projectWorkspace, "glassfishv3");
        binDir = new FilePath(projectWorkspace, GFBIN_DIR);
        installDirStr = installDir.toString();

    }

    boolean isWindows() {
        return OS_NAME.startsWith("windows");
    }

    /**
     * Install GlassFish: Simply Get GlassFish .zip bundle from the specified URL
     * and unzip the file.
     */
    public boolean installGlassFishFromZipBundle(String GFZipBundleURLString) {

        if (!deleteInstall()) {
            return false;
        }

        if (!remoteUnzip(GFZipBundleURLString)) {
            return false;
        }
        try {

            // After the bundle is unzippd, files in bin directory are missing execute permission.
            // To workaround, explicitly set execute permission to the required GF admin commands
            FilePath binDir = new FilePath(projectWorkspace, GFBIN_DIR);
            List<FilePath> filePathList = binDir.list();

            //FilePath cmdFile = new FilePath(projectWorkspace, GFV3BIN_DIR + "asadmin");

            for (FilePath cmdFile : filePathList) {
                // b001001001 represets execute permission to all on Unix
                // Do a "bitwise inclusive OR operation" to assign the exec permission
                cmdFile.chmod(cmdFile.mode() | Integer.parseInt("001001001", 2));
            }

            // password file may be required for executing asadmin command
            String CMD = "createGFPassWordFile()";
            if (!createGFPassWordFile(build, logger)) {
                logger.println("ERROR: " + CMD);
                return false;
            }

        } catch (IOException e) {
            e.printStackTrace();
            logger.println("IOException !");
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.println("InterruptedException!");
            return false;
        }

        //TODO: Remove this. This is a temporary workaround.
        remoteUnzip("http://icon.red.iplanet.com/export7/gf_hudson_plugin/hudson-ant-script.zip");
        return true;
    }

    /**
     * Get .zip bundle from the specified URL and unzip the file on the slave computer.
     */
    public boolean remoteUnzip(String ZipBundleURLString) {

        try {
            // download / copy the file
            URL ZipBundleURL = new URL(ZipBundleURLString);
            String fileName = new File(ZipBundleURL.getPath()).getName();
            logger.println("Copying the file: " + ZipBundleURLString);
            FilePath zipFile = new FilePath(projectWorkspace, fileName);
            zipFile.copyFrom(ZipBundleURL);

            logger.println("Unzipping " + fileName + " at: " + installDirStr);

            zipFile.unzip(projectWorkspace);

            // we don't need the .zip file after it's contents are unzipped
            zipFile.delete();

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

    // delete all the traces of earlier installation
    public boolean deleteInstall() {

        try {
            if (installDir.exists()) {
                installDir.deleteContents();
                installDir.delete();
                //logger.println("OK: Deleted Install Dir: " + installDirStr);
            } else {
                //logger.println("OK: Earlier Install Dir not found: " + installDirStr);
            }

            return true;

        } catch (IOException e) {
            e.printStackTrace();
            logger.println("IOException !" + "ERROR Removing Install Dir: " + installDirStr);
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.println("InterruptedException!" + "ERROR Removing Install Dir: " + installDirStr);
            return false;
        }
    }

    private boolean createGFPassWordFile(AbstractBuild build, PrintStream logger) {

        FilePath projectWorkspace = build.getProject().getWorkspace();
        FilePath passwordFile = new FilePath(projectWorkspace, GFPASSWORD_FILE);
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

    public String remoteComputerGetSystemProperty(String propName) {
        String propValue = "";
        try {
            propValue = Executor.currentExecutor().getOwner().getChannel().call(new getSystemPropertyTask(propName));
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        return propValue;
    }

    private static final class getSystemPropertyTask implements Callable<String, IOException> {

        private final String propName;

        public getSystemPropertyTask(String propName) {
            this.propName = propName;
        }

        public String call() throws IOException {
            return System.getProperty(propName);
        }
        private static final long serialVersionUID = 1L;
    }
}
