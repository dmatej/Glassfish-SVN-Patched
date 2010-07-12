/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package hudson.plugins.glassfish;

import hudson.model.BuildListener;
import hudson.FilePath;
import hudson.model.Computer;
import hudson.model.Node;
import hudson.Proc;
import hudson.Launcher;
import hudson.model.AbstractBuild;
import java.util.Map;
import java.io.IOException;
import java.io.PrintStream;

/**
 *
 * @author Harshad Vilekar
 */
@SuppressWarnings("deprecation")
public class GlassFishClusterNode {

    Node node;
    Launcher launcher;
    BuildListener listener;
    PrintStream logger;
    AbstractBuild build;
    FilePath workDir;
    GlassFishInstaller gfi;

    GlassFishClusterNode(Node node, AbstractBuild build, PrintStream logger, BuildListener listener) {
        this.node = node;
        this.build = build;
        this.logger = logger;
        this.listener = listener;
        try {
            String rootPath = Computer.currentComputer().getNode().getRootPath().toString();
            String wsPath = build.getWorkspace().toString();
            String relativePath = "";
            // this is expected to be always true
            if (wsPath.startsWith(rootPath)) {
                // remove directory name, plus "/" character - hence length() + 1
                relativePath = wsPath.substring(rootPath.length() + 1);
            }

            //workDir = node.getRootPath().createTempDir("gfcluster", null);
            workDir = new FilePath(node.getRootPath(), relativePath);
            workDir.mkdirs();

        } catch (IOException e) {
            e.printStackTrace();


        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        launcher = node.createLauncher(listener);

        gfi = new GlassFishInstaller(this, build, logger);

    }

    FilePath getWorkDir() {
        return workDir;
    }

    Node getNode() {
        return node;
    }

    GlassFishInstaller getInstaller() {
        return gfi;
    }

    String getNodeName() {
        return getNode().getNodeName();
    }

    /**
     * Specified cmd is deployed on the node.
     * @param node
     * @param cmd
     * @return
     */
    boolean execCommand(Map additionaEnvVars, String cmd) {
        try {
            //Map envVars = build.getEnvVars();
            Map envVars = getNode().toComputer().getEnvVars();
            if (additionaEnvVars != null) {
                envVars.putAll(additionaEnvVars);
            }
            logger.println(getNodeName() + " executing: ");

            Proc proc = launcher.launch(cmd, envVars, logger, workDir);
            int exitCode = proc.join();

            if (exitCode == 0) {
                return true;
            } else {
                logger.println("ERROR: " + cmd);
                return false;
            }
        } catch (IOException e) {
            e.printStackTrace();
            logger.println("ERROR (IOException): " + cmd);
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.println("ERROR (InterruptedException): " + cmd);
            return false;
        }
    }

    boolean execCommand(String cmd) {
        return execCommand(null, cmd);
    }

    /**
     * Gives a hint about availability of the specified "basePort".
     * Checks if the specified basePort is available on the current computer.
     * If the port is available - return the port number.
     * If the port is not available, return randomly selected "available" port.
     * Note: This simply indicates that the port is available at this moment.
     * The port is is not actually reserved - and may be unavailable when the
     * GlassFish instance tries to bind to it
     */
    public int getAvailablePort(int basePort, String portName) {
        int localPort;
        final Computer cur = node.toComputer();
        org.jvnet.hudson.plugins.port_allocator.PortAllocationManager pam =
                org.jvnet.hudson.plugins.port_allocator.PortAllocationManager.getManager(cur);

        try {

            localPort = pam.allocateRandom(build, basePort);
            if (localPort != basePort) {
                logger.println(node.getNodeName() + ": Updated " + portName + "=" + localPort
                        + " (" + basePort + " is not available!)");
            }
        } catch (IOException e) {
            e.printStackTrace();
            logger.println("IOException !");
            return -1;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.println("InterruptedException!");
            return -1;
        }
        return localPort;
    }
}
