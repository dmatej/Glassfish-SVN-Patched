package com.foo.bar;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.glassfish.logging.annotation.LogMessageInfo;
import org.glassfish.logging.annotation.LogMessagesResourceBundle;
import org.glassfish.logging.annotation.LoggerInfo;

public class Chocolate {
    // The resourceBundle name to be used for the module's log messages
    @LogMessagesResourceBundle
    public static final String LOGMESSAGES_RB = "com.foo.bar.LogMessages";

    @LoggerInfo(subsystem="EJB", description="Main EJB Logger", publish=true)
    public static final String EJB_LOGGER_NAME = "javax.enterprise.ejb";
    
    private static final Logger EJB_LOGGER =
      Logger.getLogger(EJB_LOGGER_NAME, LOGMESSAGES_RB);

    // Define the log message
    @LogMessageInfo(
        message = "EJB subsystem has been shutdown.",
        comment = "This message indicates that the EJB container has shutdown successfully.",
        level = "INFO")
    public static final String EJB_SYSTEM_SHUTDOWN = "AS-EJB-00003";

    public void shutdown() {
        EJB_LOGGER.log(Level.INFO, EJB_SYSTEM_SHUTDOWN);
    }

    
}
