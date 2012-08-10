/*******************************************************************************
 * Copyright (c) 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     Mike Norman - DBWS-running-under-OSGi Proof-of-concept
 ******************************************************************************/
package simpletable;

//javase imports
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.sql.Connection;
import java.sql.Driver;

//java eXtension imports
import javax.servlet.ServletContext;
import javax.xml.namespace.QName;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.Endpoint;
import javax.xml.ws.Provider;
import javax.xml.ws.Service;
import javax.xml.ws.WebServiceProvider;
import javax.xml.ws.ServiceMode;
import static javax.xml.ws.Service.Mode.MESSAGE;
import static javax.xml.ws.soap.SOAPBinding.SOAP11HTTP_BINDING;

//EclipseLink imports
import org.eclipse.persistence.internal.databaseaccess.Platform;
import org.eclipse.persistence.internal.dbws.ProviderHelper;
import org.eclipse.persistence.internal.helper.ConversionManager;
import org.eclipse.persistence.internal.xr.ProjectHelper;
import org.eclipse.persistence.internal.xr.XRDynamicClassLoader;
import org.eclipse.persistence.logging.SessionLog;
import org.eclipse.persistence.oxm.XMLContext;
import org.eclipse.persistence.oxm.XMLLogin;
import org.eclipse.persistence.sessions.DatabaseLogin;
import org.eclipse.persistence.sessions.DatabaseSession;
import org.eclipse.persistence.sessions.DatasourceLogin;
import org.eclipse.persistence.sessions.Project;
import org.eclipse.persistence.sessions.Session;
import org.eclipse.persistence.sessions.factories.XMLProjectReader;
import org.eclipse.persistence.tools.dbws.DBWSBuilder;
import org.eclipse.persistence.tools.dbws.JavasePackager;
import org.eclipse.persistence.tools.dbws.TableOperationModel;
import static org.eclipse.persistence.logging.AbstractSessionLog.translateStringToLoggingLevel;
import static org.eclipse.persistence.tools.dbws.DBWSBuilder.NO_SESSIONS_FILENAME;
import static org.eclipse.persistence.tools.dbws.DBWSBuilder.SESSIONS_FILENAME_KEY;
import static org.eclipse.persistence.tools.dbws.DBWSPackager.ArchiveUse.noArchive;
import static org.eclipse.persistence.tools.dbws.XRPackager.__nullStream;

//OSGi/PDE imports
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

//test imports
import static simpletable.Constants.DATABASE_DRIVER;
import static simpletable.Constants.DATABASE_PLATFORM;
import static simpletable.Constants.DATABASE_URL;
import static simpletable.Constants.ENDPOINT_ADDRESS;
import static simpletable.Constants.TEST_PORT;
import static simpletable.Constants.TEST_SERVICE_NAMESPACE;
import static simpletable.Constants.TEST_SERVICE;
import static simpletable.Constants.CREATE_TABLE;
import static simpletable.Constants.INS1;
import static simpletable.Constants.INS2;
import static simpletable.Constants.INS3;

@WebServiceProvider(
    targetNamespace = TEST_SERVICE_NAMESPACE,
    serviceName = TEST_SERVICE,
    portName = TEST_PORT
)
@ServiceMode(MESSAGE)
public class Activator extends ProviderHelper implements BundleActivator, Provider<SOAPMessage> {

	private static BundleContext context;
	private static Endpoint endpoint = null;
	private static QName portQName = null;
	private static Service testService = null;
	private static DBWSBuilder builder = new DBWSBuilder();
    public static ByteArrayOutputStream DBWS_SERVICE_STREAM = new ByteArrayOutputStream();
    public static ByteArrayOutputStream DBWS_SCHEMA_STREAM = new ByteArrayOutputStream();
    public static ByteArrayOutputStream DBWS_OR_STREAM = new ByteArrayOutputStream();
    public static ByteArrayOutputStream DBWS_OX_STREAM = new ByteArrayOutputStream();
    public static ByteArrayOutputStream DBWS_WSDL_STREAM = new ByteArrayOutputStream();

	static BundleContext getContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext bundleContext) throws Exception {
		Activator.context = bundleContext;
        
        builder.quiet = true;
        builder.setProjectName("simpletable");
        builder.setLogLevel(SessionLog.FINE_LABEL);
        builder.setDriver(DATABASE_DRIVER);
        builder.setPlatformClassname(DATABASE_PLATFORM);
        builder.setUrl(DATABASE_URL);
        builder.getProperties().put(SESSIONS_FILENAME_KEY, NO_SESSIONS_FILENAME);
        TableOperationModel tModel = new TableOperationModel();
        tModel.setName("simpleTable");
        tModel.setTablePattern("SIMPLETABLE");
        builder.getOperations().add(tModel);
        JavasePackager packager = new JavasePackager() {
			@Override
			public void start() {
				// normally start() checks for the existence of a temp directory -
				// no need since all DBWSBuilder artifacts are in-memory
			}
        };
        packager.setArchiveUse(noArchive);
        builder.setPackager(packager);
        try {
        	// can't go thru java.sql.DriverManager 'cause it uses Class.forName() - not OSGi-friendly
			Driver driver = new org.apache.derby.jdbc.EmbeddedDriver();
			Connection conn = driver.connect(DATABASE_URL, null);
			builder.setConnection(conn);
		}
        catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
        builder.build(DBWS_SCHEMA_STREAM, __nullStream, DBWS_SERVICE_STREAM, DBWS_OR_STREAM,
            DBWS_OX_STREAM, __nullStream, __nullStream, DBWS_WSDL_STREAM, __nullStream, __nullStream,
            __nullStream, __nullStream, null);
        super.init(this.getClass().getClassLoader(), null, false);
        
        endpoint = Endpoint.create(this);
        endpoint.publish(ENDPOINT_ADDRESS);
        QName serviceQName = new QName(TEST_SERVICE_NAMESPACE, TEST_SERVICE);
        portQName = new QName(TEST_SERVICE_NAMESPACE, TEST_PORT);
        testService = Service.create(serviceQName);
        testService.addPort(portQName, SOAP11HTTP_BINDING, ENDPOINT_ADDRESS);
		System.out.println("Hello, " + testService.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		Activator.context = null;
        if (endpoint != null) {
            endpoint.stop();
        }
        super.destroy();
		System.out.println("Goodbye");
	}

	@Override
	protected InputStream initXRServicestream(ClassLoader parentClassLoader,
		ServletContext sc) {
		return new ByteArrayInputStream(DBWS_SERVICE_STREAM.toByteArray());
	}

	@Override
	protected InputStream initXRSchemaStream(ClassLoader parentClassLoader,
		ServletContext sc) {
		return new ByteArrayInputStream(DBWS_SCHEMA_STREAM.toByteArray());
	}

	@Override
	protected InputStream initWSDLInputStream(ClassLoader parentClassLoader,
		ServletContext sc) {
		return new ByteArrayInputStream(DBWS_WSDL_STREAM.toByteArray());
	}
	
	@Override
	public void buildSessions() {
		XRDynamicClassLoader xrdecl = new XRDynamicClassLoader(parentClassLoader);
	    Project oxProject = XMLProjectReader.read(new StringReader(DBWS_OX_STREAM.toString()),
	    	xrdecl);
	    ((XMLLogin)oxProject.getDatasourceLogin()).setEqualNamespaceResolvers(false);
	    Project orProject = XMLProjectReader.read(new StringReader(DBWS_OR_STREAM.toString()),
	    	xrdecl);
	    DatasourceLogin login = orProject.getLogin();
	    ((DatabaseLogin)login).setConnectionString(builder.getUrl());
	    ((DatabaseLogin)login).setDriverClassName(builder.getDriver());
	    Platform platform = builder.getDatabasePlatform();
	    ConversionManager cm = platform.getConversionManager();
	    cm.setLoader(xrdecl);
	    login.setDatasourcePlatform(platform);
	    ((DatabaseLogin)login).bindAllParameters();
	    orProject.setDatasourceLogin(login);
	    ProjectHelper.fixOROXAccessors(orProject, oxProject);
	    DatabaseSession databaseSession = orProject.createDatabaseSession();
	    int logLevel = translateStringToLoggingLevel(builder.getLogLevel());
	    if (SessionLog.OFF == logLevel) {
	    	databaseSession.dontLogMessages();
	    }
	    else {
	    	databaseSession.setLogLevel(logLevel); 
	    }
	    xrService.setORSession(databaseSession);
	    xrService.setXMLContext(new XMLContext(oxProject));
	    xrService.setOXSession(xrService.getXMLContext().getSession(0));
	}
	
	@Override
	public void loginSessions() {
		super.loginSessions();
		Session orSession = xrService.getORSession();
		try {
			orSession.executeNonSelectingSQL(CREATE_TABLE);
			orSession.executeNonSelectingSQL(INS1);
			orSession.executeNonSelectingSQL(INS2);
			orSession.executeNonSelectingSQL(INS3);
		}
		catch (Exception e) {
			// table & rows already exist
			//e.printStackTrace();
		}
	}
}