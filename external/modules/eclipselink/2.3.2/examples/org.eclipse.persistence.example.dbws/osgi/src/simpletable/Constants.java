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

public class Constants {

    //DBWS Properties
    public static final String TEST_PROJECT = 
    	"simpletable";
	//JAX-WS properties
    public static final String TEST_NAMESPACE = 
    	"urn:" + TEST_PROJECT;
    public static final String TEST_SERVICE = 
    	TEST_PROJECT + "Service";
    public static final String TEST_SERVICE_NAMESPACE = 
    	"urn:" + TEST_SERVICE;
    public static final String TEST_PORT = 
    	TEST_SERVICE + "Port";
    public static final String ENDPOINT_ADDRESS = 
    	"http://localhost:9999/" + TEST_PROJECT;

	//database properties
    public static final String DATABASE_URL =
    	"jdbc:derby:test;create=true";
    public static final String DATABASE_DRIVER = 
    	"org.apache.derby.jdbc.EmbeddedDriver";
    public static final String DATABASE_PLATFORM = 
    	"org.eclipse.persistence.platform.database.DerbyPlatform";
    
	//database smts
    public static final String CREATE_TABLE = 
    	"CREATE TABLE SIMPLETABLE (\n" +
    	"  id NUMERIC NOT NULL,\n" +
    	"  name VARCHAR(25),\n" +
    	"  since DATE,\n" +
    	"  PRIMARY KEY (id)\n" +
    	")";
    public static final String INS1 =
    	"INSERT INTO SIMPLETABLE (id, name, since) VALUES (1, 'mike', '2001-12-25')";
    public static final String INS2 =
		"INSERT INTO SIMPLETABLE (id, name, since) VALUES (2, 'blaise','2001-12-25')";
    public static final String INS3 =
		"INSERT INTO SIMPLETABLE (id, name, since) VALUES (3, 'rick','2001-12-25')";
}
