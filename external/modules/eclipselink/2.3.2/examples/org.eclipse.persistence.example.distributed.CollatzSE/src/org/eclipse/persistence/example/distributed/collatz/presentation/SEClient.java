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
 *     16/02/2011 2.3  Add multithreaded capability (default is hard + soft (HT) cores = ~90% multicore CPU utilization)  
 ******************************************************************************/  
package org.eclipse.persistence.example.distributed.collatz.presentation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

//import javax.ejb.EJBException;
import javax.naming.CommunicationException;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NameNotFoundException;
import javax.rmi.PortableRemoteObject;

import org.eclipse.persistence.example.distributed.collatz.business.CollatzFacadeRemote;
import org.eclipse.persistence.example.distributed.collatz.model.UnitOfWork;
//import weblogic.utils.net.SocketResetException;
//import javax.persistence.OptimisticLockException;

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
public class SEClient {
    /** Maximum BigInteger that can be stored in SQL field NUMERIC = 0x7fffffffffffffffL or
     * 2^63 or 10^19 or 9,223,372,036,854,775,808 or 9 Quintillion.
     * Numbers greater than this are encountered in scientific, cryptographic and nanosecond time sensitive calculations. 
     */
    private static final Long MAX_BIGINTEGER_IN_SQL = Long.MAX_VALUE;
    /** Get number of (hyperthreaded + real) cores.  IE: p630 with HT=2, Core2 E8400=2 and Core i7-920 = 8 */
    public static final int CORES = Runtime.getRuntime().availableProcessors() << 0;

    // TODO: we need to move all this to a properties file
    /** this is the index of the current server - for use in the maps and lists below */
    public static int serverInUse = 0;
    public static String serverDNS[] = {"xps435"};
    /** RMI t3 URL */
    //public static String serverT3[] = {"iiop://127.0.0.1:3700"};
    //public static String serverT3[] = {"t3://192.168.0.193:7001"};
    public static String serverT3[] = {"t3://127.0.0.1:7001"};
    private int numberServers = serverDNS.length;
    /** list of server names from above arrays */
    private List<String> serverNames = new ArrayList<String>();
    /** Input context map hashtable entries - For JNDI we are forced to use Hashtable instead of HashMap*/
    private Map<String, Hashtable<String, String>> contextHashtableMap  = new HashMap<String, Hashtable<String, String>>();
    /** output cached context maps for each remote server */
    private Map<String, Context> rmiCachedContextMap = new HashMap<String, Context>();
    /** $Proxy remote objects */
    private List<Map<String, CollatzFacadeRemote>> remoteObjects = new ArrayList<Map<String, CollatzFacadeRemote>>(CORES);
    //private Map<String, CollatzFacadeRemote> remoteObjects = new HashMap<String, CollatzFacadeRemote>();
    /** How many processors are available (real + hyperthreaded) */
    private Map<String, Integer> availableProcessors = new HashMap<String, Integer>();
    /** whether the node is accepting requests or not */
    private Map<String, Boolean> nodeUnavailable = new HashMap<String, Boolean>();
    /** map of t3 protocol URLs */
    private Map<String, String>  serverIPMap = new HashMap<String, String>();

    //private List<CollatzRunnable> runnables = new ArrayList<CollatzRunnable>();
    
    private static final String DEFAULT_CLIENT_NAME = "default";
    // WebLogic
    // verify that all EE libraries available via http://download.oracle.com/docs/cd/E12840_01/wls/docs103/client/jarbuilder.html
    private static final String CONTEXT_FACTORY_NAME = "weblogic.jndi.WLInitialContextFactory";
    private static final String SESSION_BEAN_REMOTE_NAME = "ejb/CollatzFacade#org.eclipse.persistence.example.distributed.collatz.business.CollatzFacadeRemote"; 

    public SEClient () {
        // initialize state
    	initialize();
    }

    private void initialize() {
        for(int i=0;i<numberServers;i++) {
            // For each server add the name key and corresponding RMI URL
            serverNames.add(serverDNS[i]);
            serverIPMap.put(serverDNS[i], serverT3[i]);
            nodeUnavailable.put(serverDNS[i], false);
            availableProcessors.put(serverDNS[i], Runtime.getRuntime().availableProcessors());
            Hashtable<String, String> aTable =  new Hashtable<String, String>();
            contextHashtableMap.put(serverDNS[i],aTable);
            aTable.put(Context.INITIAL_CONTEXT_FACTORY,CONTEXT_FACTORY_NAME);
            aTable.put(Context.PROVIDER_URL, serverT3[i]);
        }
        for(int i=0;i<CORES;i++) {
            connect(i);
        }
    }
    
    public void connect(int threadID) {
        // Setup RMI Objects
        // Establish RMI connections to the session beans
        //for(String aServer : serverNames) {
        String aServer = serverNames.get(0);
            Context aContext = null;
            try {
                // no need to set the host if on same machine
                aContext = new InitialContext(contextHashtableMap.get(aServer));
                rmiCachedContextMap.put(aServer, aContext);
                System.out.println("_collatz: " + System.currentTimeMillis() + ": Context for " + (aServer + threadID) + " : " + aContext);
                // For qualified name look for weblogic log "EJB Deployed EJB with JNDI name"
                Object aRemoteReference = aContext.lookup(SESSION_BEAN_REMOTE_NAME);
                System.out.println("_collatz: " + System.currentTimeMillis() + ": Remote Object: " + aRemoteReference);
                // narrow the $proxy remote bean
                CollatzFacadeRemote aNode = (CollatzFacadeRemote) PortableRemoteObject.narrow(aRemoteReference, CollatzFacadeRemote.class);
                Map<String, CollatzFacadeRemote> remoteObject = new HashMap<String, CollatzFacadeRemote>();
                remoteObject.put(aServer, aNode);
                remoteObjects.add(remoteObject);
            } catch (Exception ce) {
                // server down throws a javax.naming.CommunicationException inside a java.net.ConnectException
                ce.printStackTrace();
                // mark the current node as down, clear the flag in 5 min
            }
        //}
    }

    private CollatzFacadeRemote lookupRemoteBean(String aServer, int threadID)  {
        CollatzFacadeRemote remoteBean = null;
        try {
            Context aContext = rmiCachedContextMap.get(aServer);
            // For qualified name look for weblogic log "EJB Deployed EJB with JNDI name"
            Object aRemoteReference = null;
            boolean remoteLookupSuccess = false;
            int lookupIterations = 0;
            while(!remoteLookupSuccess && aRemoteReference == null && lookupIterations < 50) {
                System.out.println("_collatz: " + System.currentTimeMillis() + ": Thread: " + threadID + " : Context lookup for " + SESSION_BEAN_REMOTE_NAME + " from: " + aContext);
                try {
                    aRemoteReference = aContext.lookup(SESSION_BEAN_REMOTE_NAME);
                } catch (NameNotFoundException nnfe) {
                    System.out.println(nnfe.getMessage());
                    System.out.println("_collatz: " + System.currentTimeMillis() + ": retry session bean lookup - possible redeploy in progress on central server: " + lookupIterations);
                    Thread.sleep(1000);
                } catch (CommunicationException ce) {//SocketResetException sre) {
                    // Network was temporarily disconnected - or server went down
                    System.out.println(ce.getMessage());
                    System.out.println("_collatz: " + System.currentTimeMillis() + ": retry session bean lookup - Network or server is temporarily down: " + lookupIterations);
                    Thread.sleep(1000);
                }
                lookupIterations++;
            }
            System.out.println("_collatz: " + System.currentTimeMillis() + ": Remote Object: " + aRemoteReference);
            // narrow the $proxy remote bean
            remoteBean = (CollatzFacadeRemote) PortableRemoteObject.narrow(aRemoteReference, CollatzFacadeRemote.class);
            Map<String, CollatzFacadeRemote> remoteObject = new HashMap<String, CollatzFacadeRemote>();
            remoteObject.put(aServer, remoteBean);
            setRemoteObjects(threadID, remoteObject);
        /*} catch (ConnectException rmice) {//CommunicationException ce) {//SocketResetException sre) {
            // Network was temporarily disconnected - or server went down
            System.out.println(rmice.getMessage());
            System.out.println("_collatz: " + System.currentTimeMillis() + ": retry session bean lookup - Network or server is temporarily down: " + lookupIterations);
            Thread.sleep(1000);*/
        } catch (Exception e) {
            e.printStackTrace();
        }
        return remoteBean;
     }
    
    public void processUnitOfWork(int threadID) {
        // ask for a work packet
        UnitOfWork uow = null;
        CollatzFacadeRemote collatzFacade = null;
        StringBuffer aBuffer = new StringBuffer();
        String threadName;
        // Endlessly generate RMI requests
        for(;;) {
            try {
                // Send messages to entire grid in parallel if we connect to more than one server
                // TODO: create Threads for each remoteObject
                for(String remoteServer : remoteObjects.get(threadID).keySet()) {
                    threadName = serverDNS[serverInUse] + threadID;
                    collatzFacade = remoteObjects.get(threadID).get(remoteServer);
                    try {
                        // Issue: One JVM halt will affect the entire distributed app.
                        // don't let a node failure halt the host
                        // this remote call can throw an EJBException wrapping a java.rmi.ConnectException                            
                        uow = collatzFacade.requestUnitOfWork(threadName,availableProcessors.get(remoteServer)); 
                        if(null == uow) {
                            // possible redeploy
                            System.out.println("_collatz: " + System.currentTimeMillis() + ": persistence not functioning on server " + serverDNS[serverInUse]);
                        } else {
                            aBuffer = new StringBuffer("_collatz: ");
                            aBuffer.append(System.currentTimeMillis());
                            aBuffer.append(": processing UnitOfWork: ");
                            aBuffer.append(uow);
                            aBuffer.append(" ID#");
                            aBuffer.append(uow.getId());
                            aBuffer.append(" ");
                            aBuffer.append(uow.getInitial());
                            aBuffer.append("-");
                            aBuffer.append(uow.getExtent());
                            aBuffer.append(" for: ");
                            aBuffer.append(threadName);
                            System.out.println(aBuffer.toString());
                        }
                    } catch (Exception e) {//(EJBException e) {
                        //  weblogic.transaction.internal.TimedOutException: Transaction timed out after 29 seconds
                        // or SQLException on constraint violation
                        // EJBException wrapping a java.rmi.ConnectException if the server is not running
                        e.printStackTrace();
                        // mark the current node as down, clear the flag in 5 min
                        //nodeUnavailable.put(remoteServer, true);
                    }
                    // compute collatz for the sequence
                    uow.processInterval();
                    Thread.yield(); // 
                    // return the results to the server
                    // don't cache the remote bean (it may be GC'd or redeployed)
                    collatzFacade = lookupRemoteBean(remoteServer, threadID);
                    boolean retry = true;
                    while(retry) {
                        try {
                            collatzFacade.postUnitOfWork(uow,retry); // possible EJBException or OptimisticLockException
                            retry = false;
                        } catch (Exception ole) {//OptimisticLockException ole) {
                            //System.out.println(ole.getMessage());
                            retry = true;
                            Thread.sleep(1000);
                        }
                    }
                    aBuffer = new StringBuffer("_collatz: ");
                    aBuffer.append(System.currentTimeMillis());
                    aBuffer.append(": results sent to server after ");
                    aBuffer.append(uow.getEndTimestamp() - uow.getStartTimestamp());
                    aBuffer.append(" ms @ ");
                    aBuffer.append(uow.getMIPS());
                    aBuffer.append(" MIPS");
                    System.out.println(aBuffer.toString());
                }
            } catch (Exception e) {
                e.printStackTrace();
                try {
                    Thread.sleep(10000);
                } catch (Exception ex) { }
            }
        }
    }

    protected void startThreads(int numberOfThreads) {
        threadSafetyPrivate(numberOfThreads);
    }
    
    protected void threadSafetyPrivate(int numberOfThreads) {
        List<Thread> threadList = new ArrayList<Thread>();
        for(int i=0; i<numberOfThreads; i++) {
            Thread aThread = new Thread(new CollatzRunnable(i));
            threadList.add(aThread);
            // stagger the threads so they are not in lockstep
            try {
                Thread.sleep(3000);
            } catch (Exception e) {           }
            aThread.start();
        }

        // Wait for [threadNumber] threads to complete before ending 
        for(Thread aThread : threadList) {
            try {
                synchronized (aThread) {
                    aThread.join();
                }
            } catch (InterruptedException ie_Ignored) {
                ie_Ignored.printStackTrace();
            } // The InterruptedException can be ignored 
        }
    }
    
    // Inner class implements Runnable instead of extending Thread directly
    class CollatzRunnable implements Runnable {
        protected int id;
        
        public CollatzRunnable(int anId) {
            id = anId;
        }
        
        public void run() {
            //connect(id);
            // We loop an arbitrary number of iterations inside each thread
            processUnitOfWork(id);
        }
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        String clientName = null;
        String ip = null;
        if(null != args && args.length > 0) {
            clientName = args[0];
            ip = args[1];
        } else {
            clientName = DEFAULT_CLIENT_NAME;
            ip = "t3://127.0.0.1:7001";  // weblogic specific                       
        }
        serverDNS[serverInUse] = clientName;
        serverT3[serverInUse] = ip;

        SEClient client = new SEClient();
        // Create and start threads
        client.startThreads(CORES);
        //client.connect();
        //client.processUnitOfWork();
    }

    public Map<String, Hashtable<String, String>> getContextMap() {        return contextHashtableMap;    }
    public void setContextMap(Map<String, Hashtable<String, String>> contextMap) {        this.contextHashtableMap = contextMap;    }
    public Map<String, Context> getRmiContextMap() {        return rmiCachedContextMap;    }
    public void setRmiContextMap(Map<String, Context> rmiContextMap) {        this.rmiCachedContextMap = rmiContextMap;    }
    public Map<String, CollatzFacadeRemote> getRemoteObjects(int threadID) {        return remoteObjects.get(threadID);    }
    public void setRemoteObjects(int threadID, Map<String, CollatzFacadeRemote> remoteObjects) {        this.remoteObjects.set(threadID, remoteObjects);    }
    public Map<String, Boolean> getNodeUnavailable() {        return nodeUnavailable;    }
    public void setNodeUnavailable(Map<String, Boolean> nodeUnavailable) {        this.nodeUnavailable = nodeUnavailable;    }
    public Map<String, String> getServerIPMap() {        return serverIPMap;    }
    public void setServerIPMap(Map<String, String> aServerIPMap) {        serverIPMap = aServerIPMap;    }
    public List<String> getServernames() {        return serverNames;    }
    public void setServerNames(List<String> serverNames) {        this.serverNames = serverNames;    }
    public int getNumberServers() {        return numberServers;    }
    public void setNumberServers(int numberServers) {        this.numberServers = numberServers;    }
    public Map<String, Integer> getAvailableProcessors() {        return availableProcessors;    }
    public void setAvailableProcessors(Map<String, Integer> availableProcessors) {        this.availableProcessors = availableProcessors;    }
}
