package com.sun.s1asdev.ejb.mdb.cmt.client;

/*
 * Copyright 2002 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
import java.io.*;
import java.util.*;
import javax.ejb.EJBHome;
import javax.naming.*;
import javax.jms.*;

import com.sun.ejte.ccl.reporter.SimpleReporterAdapter;

public class Client {

    private static SimpleReporterAdapter stat =
        new SimpleReporterAdapter("appserv-tests");

    public static void main (String[] args) {
        Client client = new Client(args);

        stat.addDescription("ejb-mdb-cmt");
        client.doTest();
        stat.printSummary("ejb-mdb-cmtID");
        System.exit(0);
    }

    private InitialContext context;
    private QueueConnection queueCon;
    private QueueSession queueSession;
    private QueueSender queueSender;
    private QueueReceiver queueReceiver;
    private javax.jms.Queue clientQueue;

    private TopicConnection topicCon;
    private TopicSession topicSession;
    private TopicPublisher topicPublisher;

    private int numMessages = 2;
    public Client(String[] args) {
        
        if( args.length == 1 ) {
            numMessages = new Integer(args[0]).intValue();
        }
    }

    public void doTest() {
        try {
            setup();
            doTest("java:comp/env/jms/MsgBeanQueue", numMessages);
            stat.addStatus("cmt main", stat.PASS);
        } catch(Throwable t) {
            stat.addStatus("cmt main", stat.FAIL);
            t.printStackTrace();
        } finally {
            cleanup();
        }
    }

    public void setup() throws Exception {
        context = new InitialContext();
        
        QueueConnectionFactory queueConFactory = 
            (QueueConnectionFactory) context.lookup
            ("java:comp/env/FooCF");
            
        queueCon = queueConFactory.createQueueConnection();

        queueSession = queueCon.createQueueSession
            (false, Session.AUTO_ACKNOWLEDGE); 

        // Producer will be specified when actual msg is sent.
        queueSender = queueSession.createSender(null);        

        queueCon.start();

        /*
        TopicConnectionFactory topicConFactory = 
            (TopicConnectionFactory) context.lookup
            ("jms/TopicConnectionFactory");
                
        topicCon = topicConFactory.createTopicConnection();

        topicSession = 
            topicCon.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            
        // Producer will be specified when actual msg is published.
        topicPublisher = topicSession.createPublisher(null);
        */
    }

    public void cleanup() {
        try {
            if( queueCon != null ) {
                queueCon.close();
            }
        } catch(Throwable t) {
            t.printStackTrace();
        }
    }

    public void sendMsgs(javax.jms.Queue queue, Message msg, int num) 
        throws JMSException {
        for(int i = 0; i < num; i++) {
            System.out.println("Sending message " + i + " to " + queue + 
                               " at time " + System.currentTimeMillis());
            queueSender.send(queue, msg);
            System.out.println("Sent message " + i + " to " + queue + 
                               " at time " + System.currentTimeMillis());
        }
    }

    public void sendMsgs(Topic topic, Message msg, int num) 
        throws JMSException {
        for(int i = 0; i < num; i++) {
            //            System.out.println("Publishing message " + i + " to " + queue);
            System.out.println("Publishing message " + i + " to " + topic);
            topicPublisher.publish(topic, msg);
        }
    }

    public void doTest(String destName, int num) 
        throws Exception {

        Destination dest = (Destination) context.lookup(destName);
            
        Message message = queueSession.createTextMessage(destName);
        //        Message message = topicSession.createTextMessage(destName);
        message.setBooleanProperty("flag", true);
        message.setIntProperty("num", 2);
        sendMsgs((javax.jms.Queue) dest, message, num);
    }
}

