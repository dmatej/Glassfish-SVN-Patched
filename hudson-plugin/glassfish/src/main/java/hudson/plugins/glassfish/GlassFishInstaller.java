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
import java.util.List;

/**
 * GlassFish Application Server .zip Installer.
 * @author Harshad Vilekar
 */
@SuppressWarnings("deprecation")
public final class GlassFishInstaller {

    //define PATHs relative to project workspace
    String GFHOME_DIR, GFDOMAIN1_LOGS_DIR;
    String GFBIN_DIR, GFV3BIN_DIR;
    String GFPASSWORD_FILE;
    String GFADMIN_CMD;
    String USER_PASSWORD_FLAGS;
    String OS_NAME;
    private AbstractBuild build;
    private PrintStream logger;
    private FilePath projectWorkSpace, installDir;
    FilePath binDir, gfv3binDir, domain1LogsDir, GFHomeDir;
    private String installDirStr;
    GlassFishClusterNode clusterNode;
    final boolean Verbose = true;

    public GlassFishInstaller(GlassFishClusterNode clusterNode, AbstractBuild build, PrintStream logger) {
        this.build = build;
        this.logger = logger;
        this.clusterNode = clusterNode;

        OS_NAME = clusterNode.getOS();
        logger.println(clusterNode.getNodeName() + ", os.name=" + OS_NAME);

        projectWorkSpace = clusterNode.getWorkDir();
        String GFHOME_DIR_REL = "";
        if (isWindows()) {
            GFHOME_DIR_REL = "glassfishv3";
            GFHOME_DIR = projectWorkSpace.toString() + "\\" + GFHOME_DIR_REL;
            GFBIN_DIR = GFHOME_DIR + "\\glassfish\\bin\\";
            GFV3BIN_DIR = GFHOME_DIR + "\\bin\\";
            GFADMIN_CMD = GFBIN_DIR + "asadmin.bat";
            GFDOMAIN1_LOGS_DIR = GFHOME_DIR_REL + "\\glassfish\\domains\\domain1\\logs\\";

            GFPASSWORD_FILE = GFHOME_DIR + "\\glassfish\\config\\passwordfile";
            USER_PASSWORD_FLAGS = " --passwordfile " + GFPASSWORD_FILE + " --user admin ";
        } else {
            GFHOME_DIR_REL = "glassfishv3";
            GFHOME_DIR = projectWorkSpace.toString() + "/" + GFHOME_DIR_REL;
            GFBIN_DIR = GFHOME_DIR + "/glassfish/bin/";
            GFV3BIN_DIR = GFHOME_DIR + "/bin/";
            GFADMIN_CMD = GFBIN_DIR + "asadmin";
            GFDOMAIN1_LOGS_DIR = GFHOME_DIR_REL + "/glassfish/domains/domain1/logs/";

            GFPASSWORD_FILE = GFHOME_DIR + "/glassfish/config/passwordfile";
            USER_PASSWORD_FLAGS = " --passwordfile " + GFPASSWORD_FILE + " --user admin ";
        }

        installDir = new FilePath(projectWorkSpace, "glassfishv3");
        GFHomeDir = new FilePath(projectWorkSpace, GFHOME_DIR_REL);
        binDir = new FilePath(projectWorkSpace, GFBIN_DIR);
        gfv3binDir = new FilePath(projectWorkSpace, GFV3BIN_DIR);
        domain1LogsDir = new FilePath(projectWorkSpace, GFDOMAIN1_LOGS_DIR);
        installDirStr = installDir.toString();
    }

    String getAdminCmd() {
        return GFADMIN_CMD;
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

        if (!remoteUnzip(false, GFZipBundleURLString)) {
            return false;
        }
        // After the bundle is unzippd, files in bin directory are missing execute permission.
        // To workaround, explicitly set execute permission for all the commands in all the bin directories
        // found inside current installation. This includes: ./bin, glassfish/bin, mq/bin, javadb/bin etc.
        boolean result = true;
        String dir1, dir2, dir3, dir4;
        if (isWindows()) {
            dir1 = "bin";
            dir2 = "glassfish\\bin";
            dir3 = "mq\\bin";
            dir4 = "javadb\\bin";
        } else {
            dir1 = "bin";
            dir2 = "glassfish/bin";
            dir3 = "mq/bin";
            dir4 = "javadb/bin";
        }

        result &= assignExecPermissions(new FilePath(installDir, dir1));
        result &= assignExecPermissions(new FilePath(installDir, dir2));
        result &= assignExecPermissions(new FilePath(installDir, dir3));
        result &= assignExecPermissions(new FilePath(installDir, dir4));

        if (!result) {
            return false;
        }
        // password file may be required for executing asadmin command
        String CMD = "createGFPassWordFile()";
        if (!createGFPassWordFile(build, logger)) {
            logger.println("ERROR: " + CMD);
            return false;
        }

        return true;
    }

    // assigns executable permission flag to all the files in the given directory
    boolean assignExecPermissions(FilePath thisDir) {
        try {
            List<FilePath> fileList = thisDir.list();
            if (fileList == null || fileList.isEmpty()) {
                return true;
            }
            for (FilePath cmdFile : fileList) {
                // b001001001 represets execute permission to all on Unix
                // Do a "bitwise inclusive OR operation" to assign the exec permission
                cmdFile.chmod(cmdFile.mode() | Integer.parseInt("001001001", 2));
                //logger.println("assignExecPermissions:" + cmdFile.toString());
            }
        } catch (IOException e) {
            e.printStackTrace(logger);
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace(logger);
            return false;
        }
        return true;
    }

    /**
     * Get .zip bundle from the specified URL and unzip the file on the slave computer.
     */
    public boolean remoteUnzip(boolean verbose, String ZipBundleURLString) {

        try {
            // download / copy the file
            URL ZipBundleURL = new URL(ZipBundleURLString);
            String fileName = new File(ZipBundleURL.getPath()).getName();
            println(verbose, "Copying the file: " + ZipBundleURLString);
            FilePath zipFile = new FilePath(projectWorkSpace, fileName);
            zipFile.copyFrom(ZipBundleURL);

            println(verbose, "Unzipping " + fileName + " at: " + installDirStr);

            zipFile.unzip(projectWorkSpace);

            // we don't need the .zip file after it's contents are unzipped
            zipFile.delete();

        } catch (IOException e) {
            e.printStackTrace(logger);
            //logger.println("IOException !");
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace(logger);
            //logger.println("InterruptedException!");
            return false;
        }
        return true;
    }

    /**
     * Get the file from the specified URL and save on the slave computer.
     */
    public boolean remoteCopyFile(boolean verbose, String fileURLString) {

        try {
            // download / copy the file
            URL fileURL = new URL(fileURLString);
            String fileName = new File(fileURL.getPath()).getName();
            println(verbose, "Copying the file: " + fileURLString);
            FilePath file = new FilePath(projectWorkSpace, fileName);
            file.copyFrom(fileURL);
        } catch (IOException e) {
            e.printStackTrace(logger);
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace(logger);
            return false;
        }
        return true;
    }

    void println(boolean verbose, String msg) {
        if (verbose) {
            logger.println(msg);
        }
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
            e.printStackTrace(logger);
            logger.println("IOException !" + "ERROR Removing Install Dir: " + installDirStr);
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace(logger);
            logger.println("InterruptedException!" + "ERROR Removing Install Dir: " + installDirStr);
            return false;
        }
    }

    private boolean createGFPassWordFile(AbstractBuild build, PrintStream logger) {

        FilePath passwordFile = new FilePath(projectWorkSpace, GFPASSWORD_FILE);
        String passwd = "AS_ADMIN_PASSWORD=adminadmin\n"
                + "AS_ADMIN_MASTERPASSWORD=changeit\n";

        try {
            passwordFile.write(passwd, null);

        } catch (IOException e) {
            e.printStackTrace(logger);
            //logger.println("IOException !");
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace(logger);
            //logger.println("InterruptedException!");
            return false;
        }

        return true;
    }
}
