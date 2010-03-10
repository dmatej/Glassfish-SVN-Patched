package testing;

import org.eclipse.persistence.config.SessionCustomizer;
import org.eclipse.persistence.sessions.DatabaseLogin;
import org.eclipse.persistence.sessions.Session;
import org.eclipse.persistence.sessions.server.ConnectionPolicy;
import org.eclipse.persistence.sessions.server.Server;

/**
 * This is a session customizer that sets up the connector for proxy
 * authentication usage with an non-JTA OCI connection pool.
 * 
 * In the persistence.xml or properties passed to the createEntitymanagerFactory
 * call the following must be provided:
 * 
 * <property name="eclipselink.session-customizer"
 * value="testing.SessionCustomizer"/>
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class ProxyCustomizer implements SessionCustomizer {

	public void customize(Session session) throws Exception {
		Server server = (Server) session;
		DatabaseLogin login = server.getLogin();

		// Make sure that external connection pooling is used
		login.setUsesExternalConnectionPooling(true);
		// Wrap JNDIConnector with OracleJDBC10_1_0_2ProxyConnector
		// TODO:
		// login.setConnector(new OracleJDBC10_1_0_2ProxyConnector(
		// ((JNDIConnector) login.getConnector()).getName()));
		ConnectionPolicy policy = server.getDefaultConnectionPolicy();
		policy.setPoolName(null);
		policy.setLogin(login);

		// If using Oracle VPD support,set the connection policy to exclusive
		policy.setShouldUseExclusiveConnection(true);
	}

}
