package com.foo.bar;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.glassfish.logging.annotation.LogMessageInfo;
import org.glassfish.logging.annotation.LoggerInfo;

/**
 * There is no resource bundle specified.
 * 
 */
public class Cocoa {

    // The resourceBundle name to be used for the module's log messages
    // @LogMessagesResourceBundle
    public static final String LOGMESSAGES_RB = "com.foo.bar.EJBLogMessages";

    @LoggerInfo(subsystem="EJB", description="Main EJB Logger", publish=true)
    public static final String EJB_LOGGER_NAME = "javax.enterprise.ejb";
    
    private static final Logger EJB_LOGGER =
      Logger.getLogger(EJB_LOGGER_NAME, LOGMESSAGES_RB);

    // Define the log message
    @LogMessageInfo(
        message = "EJB subsystem initialized.",
        comment = "This message indicates that the EJB container initialized successfully.",
        level = "INFO")
    public static final String EJB_SYSTEM_INITIALIZED = "AS-EJB-00001";

    @LogMessageInfo(
        message = "EJB module {0} failed to deploy.",
        comment = "This log message indicates a failure to deploy the given EJB module.",
        level = "SEVERE",
        cause = "Could not deploy an EJB module",
        action = "Verify the deployment descriptor.")
    public static final String EJB_DEPLOYMENT_FAILED = "AS-EJB-00002";

    public void initialize() {
        EJB_LOGGER.log(Level.INFO, EJB_SYSTEM_INITIALIZED);
    }

    public void deploy(String module) {
        EJB_LOGGER.log(Level.SEVERE, EJB_DEPLOYMENT_FAILED, module);
    }
    
    
}
