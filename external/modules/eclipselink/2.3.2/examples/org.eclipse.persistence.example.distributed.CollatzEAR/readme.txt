/*******************************************************************************
 * Copyright (c) 2010, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     16/02/2011 2.3  Michael O'Brien 
 *          - 337037: initial API and implementation platform to be used for 
 *             distributed EE application research, development and architecture
 ******************************************************************************/  

/**
 * This class is part of a distributed application framework used to simulate and research
 * concurrency, analytics, management, performance and exception handling.
 * The focus is on utilizing JPA 2.0 as the persistence layer for scenarios involving
 * multicore, multithreaded and multiuser distributed memory L1 persistence applications.
 * The secondary focus is on exercising Java EE6 API to access the results of this distributed application.
 * 
 * @see http://bugs.eclipse.org/337037
 * @see http://wiki.eclipse.org/EclipseLink/Examples/Distributed
 * @see http://glassfish.java.net/javaee5/ejb/EJB_FAQ.html#StandaloneRemoteEJB
 * @author Michael O'Brien
 * @since EclipseLink 2.3
 */


Refer to 
http://wiki.eclipse.org/EclipseLink/Examples/Distributed

1) Install Eclipse 3.6 Helios EE edition
2) Add WebLogic Server Tools in "add new server"
3) create a server configuration to a WebLogic 10.3.4.0 domain
4) (you may need to go into the WAR and enable the JSF 2.0 component (choose the JSF 2.0/1.0.0.0_2-0-2 version))
   -this will setup enable JSF 2.0 (on top of the out of the box JSF 1.2 in WebLogic)
   see
http://wiki.eclipse.org/Image:Eclipse_36_helios_for_weblogic_1034_jsf2_facet.JPG

5) start Derby before starting WebLogic
c:\opt\derby10530\bin\startNetworkServer.bat
6) start weblogic in Eclipse
7) Run on server
8) Run SEClient from the current machine as is from eclipse, or
 a remote JVM using the following command line if running remotely
- copy the remote interface CollatzFacadeRemote and all the model classes or jar to the client bin directory (in packages)
run
C:\_experiment\0\org.eclipse.persistence.example.distributed.CollatzSE\bin>java 
-cp .;../resource/wlfullclient.jar 
org.eclipse.persistence.example.distributed.collatz.presentation.SEClient 
your_client_name_id 
t3://127.0.0.1:7001

9) After the first run (you should turn off DDL generation in persistence.xml so the database is not cleared on each redeploy)
      <!--property name="eclipselink.ddl-generation" value="drop-and-create-tables"/>
      <property name="eclipselink.ddl-generation.output-mode" value="both"/-->

>you should see the following in the client after about 10 sec when the first unitOfWork is processed
collatz: Remote Object: ClusterableRemoteRef(2112014917424158770S:127.0.0.1:[7001,7001,-1,-1,-1,-1,-1]:base_domain:AdminServer [2112014917424158770S:127.0.0.1:[7001,7001,-1,-1,-1,-1,-1]:base_domain:AdminServer/308])/308
_collatz: results sent to server after 5610 ms
_collatz: process UnitOfWork: org.eclipse.persistence.example.distributed.collatz.model.UnitOfWork@66( id: 66) ID#66 1048579-2097154 from: gx660a

>you should see the following on the server
_collatz: requestUnitOfWork(1048578-2097154) for processor org.eclipse.persistence.example.distributed.collatz.model.ActiveProcessor@1( id: 1)
[EL Fine]: 2011-02-16 16:33:28.411--ClientSession(155963)--Connection(4029435)--Thread(Thread[[ACTIVE] ExecuteThread: '20' for queue: 'weblogic.kernel.Default (self-tuning)',5,Pooled Threads])--INSERT INTO UNITOFWORK (ID, ENDTIMESTAMP, EXTENT, INITIAL, MAXPATH, MAXVALUE, RETRIES, STARTTIMESTAMP, VERSION, KNOWNMAX_ID, KNOWNPATH_ID, PROCESSOR_ID) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
	bind => [66, null, 2097154, 1048579, 523, 90239155648, 0, 1297890208411, 1, null, null, 1]


