package example.where.service.impl;

import static org.eclipse.persistence.config.PersistenceUnitProperties.*;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.spi.PersistenceUnitTransactionType;

import org.eclipse.persistence.config.BatchWriting;
import org.eclipse.persistence.config.TargetServer;

public class EclipseLinkServiceFactory extends ServiceFactory {

    public Map getSEProperties(boolean logSQL) {
        Map props = new HashMap();

        // Ensure RESOURCE_LOCAL transactions is used.
        props.put(TRANSACTION_TYPE, PersistenceUnitTransactionType.RESOURCE_LOCAL.name());

        // Configure the internal EclipseLink connection pool
        props.put(JDBC_DRIVER, "oracle.jdbc.OracleDriver");
        props.put(JDBC_URL, "jdbc:oracle:thin:@localhost:1521:ORCL");
        props.put(JDBC_USER, "scott");
        props.put(JDBC_PASSWORD, "tiger");
        props.put(JDBC_READ_CONNECTIONS_MIN, "1");
        props.put(JDBC_WRITE_CONNECTIONS_MIN, "1");

        props.put(BATCH_WRITING, BatchWriting.OracleJDBC);

        // Configure logging. FINE ensures all SQL is shown
        props.put(LOGGING_LEVEL, logSQL ? "FINE" : "CONFIG");
        props.put(LOGGING_TIMESTAMP, "false");
        props.put(LOGGING_THREAD, "false");
        props.put(LOGGING_SESSION, "false");

        // Ensure that no server-platform is configured
        props.put(TARGET_SERVER, TargetServer.None);

        return props;
    }

}
