/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package hudson.plugins.glassfish;

import hudson.remoting.Callable;

import hudson.model.BuildListener;
import hudson.FilePath;
import hudson.model.Computer;
import hudson.model.Node;
import hudson.Proc;
import hudson.Launcher;
import hudson.model.AbstractBuild;
import hudson.EnvVars;
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
    static PrintStream logger;
    static AbstractBuild build;
    FilePath workDir;
    GlassFishInstaller gfi;

    GlassFishClusterNode(Node node, AbstractBuild build, PrintStream logger, BuildListener listener) {
        this.node = node;
        this.build = build;
        this.logger = logger;
        this.listener = listener;       

        setWorkDir();

        launcher = node.createLauncher(listener);
        gfi = new GlassFishInstaller(this, build, logger);

    }

    boolean setWorkDir() {
        try {
            // Use Same WS directory - relative to the rootpath - on Slave Computer
            // as the build computer
            String rootPath = Computer.currentComputer().getNode().getRootPath().toString();
            String wsPath = build.getWorkspace().toString();
            String relativePath = "";

            String nodeRootPath = node.getRootPath().toString() ;
            String nodeWsPath = "" ;

            // this is expected to be always true
            if (wsPath.startsWith(rootPath)) {
                // remove root path, plus dir separator ("\" on windows, "/" on Unix)
                // character - hence length() + 1
                relativePath = wsPath.substring(rootPath.length() + 1);
            }
            
            if (isWindows()) {
                relativePath = relativePath.replace('/', '\\');
            } else {
                relativePath = relativePath.replace('\\', '/');
            }
            workDir = new FilePath(node.getRootPath(), relativePath);
            
            workDir.mkdirs();

        } catch (IOException e) {
            e.printStackTrace(logger);
            return false ;


        } catch (InterruptedException e) {
            e.printStackTrace(logger);
            return false ;
        }
        return true ;
    }

    FilePath getWorkDir() {
        return workDir;
    }

    Node getNode() {
        return node;
    }

    final GlassFishInstaller getInstaller() {
        return gfi;
    }

    String getNodeName() {
        return getNode().getNodeName();
    }

    boolean isWindows() {
        return getOS().startsWith("windows");
    }

    String getOS() {
        return getSystemProperty("os.name").toLowerCase();
    }
    
     public String getSystemProperty(String propName) {
        String propValue = "";
        try {
            propValue = getNode().getChannel().call(new getSystemPropertyTask(propName));
        } catch (IOException e) {
            e.printStackTrace(logger);
        } catch (InterruptedException e) {
            e.printStackTrace(logger);
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

            // as of 3.1-b09, command replication framework is not yet enabled by default
            // to workaround, explicitly set the environment variable
            String key = "ENABLE_REPLICATION", value = "true";
            EnvVars clusterEnv = new EnvVars(key, value);
            envVars.putAll(clusterEnv);

            logger.println(GlassFishPluginUtils.getLogDate() + getNodeName() + " executing: ");

            Proc proc = launcher.launch(cmd, envVars, logger, workDir);
            int exitCode = proc.join();

            if (exitCode == 0) {
                return true;
            } else {
                logger.println("ERROR: " + cmd);
                return false;
            }
        } catch (IOException e) {
            e.printStackTrace(logger);
            logger.println("ERROR (IOException): " + cmd);
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace(logger);
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
    static int getAvailablePort(Node node, int basePort, String portName) {
        int localPort;
        Computer cur = node.toComputer();
        org.jvnet.hudson.plugins.port_allocator.PortAllocationManager pam =
                org.jvnet.hudson.plugins.port_allocator.PortAllocationManager.getManager(cur);

        try {

            localPort = pam.allocateRandom(build, basePort);
            if (localPort != basePort) {
                logger.println(node.getNodeName() + ": Updated " + portName + "=" + localPort
                        + " (" + basePort + " is not available!)");
            }
        } catch (IOException e) {
            e.printStackTrace(logger);
            //logger.println("IOException !");
            return -1;
        } catch (InterruptedException e) {
            e.printStackTrace(logger);
            //logger.println("InterruptedException!");
            return -1;
        }
        return localPort;
    }

    int getAvailablePort(int basePort, String portName) {
        return getAvailablePort(node, basePort, portName);
    }
}
