/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     James Sutherland - initial impl
 ******************************************************************************/  
 package example;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.rmi.PortableRemoteObject;

import model.*;

/**
 * This example processes updates to the same employee on different nodes in the cluster.
 */
public class Transactions {
    private List<String> serverURLs;
    private int server = 0;

    public static void main(String[] args) throws Exception {
        Transactions transactions = new Transactions();
        transactions.updateEmployee();
    }
    
    public Transactions() {
        this.serverURLs = new ArrayList<String>();
        // localhost may not work, machine name/ip may be required.
        this.serverURLs.add("t3://localhost:7021/");
        this.serverURLs.add("t3://localhost:7031/");
        this.serverURLs.add("t3://localhost:7041/");
    }

    /**
     * Return the next server index to use.
     * This cycles through the servers.
     */
    public int nextServer() {
        this.server++;
        if (this.server >= this.serverURLs.size()) {
            this.server = 0;
        }
        return this.server;
    }

    public EmployeeService nextEmployeeService() {
        EmployeeService service = null;
        int server = nextServer();
        Properties properties = new Properties();
        String url = this.serverURLs.get(server);
        properties.put("java.naming.provider.url", url);
        System.out.println("Connecting to server:" + url);
        try {
            Context context = new InitialContext(properties);
            service = (EmployeeService)PortableRemoteObject.narrow(context.lookup("EmployeeService#model.EmployeeService"), EmployeeService.class);
        } catch (Exception notFoundException) {
            throw new Error("Lookup failed, verify lookup string is correct for your JEE server, and that deploy was successful.", notFoundException);
        }
        return service;
    }

    /**
     * Update the same employee on each server.
     */
    public void updateEmployee() throws Exception {
        try {
            Employee employee = null;
            System.out.println("Populating database.");
            EmployeeService service = nextEmployeeService();
            service.setup();
            System.out.println("Preloading cache on all servers.");
            for (int index = 0; index < 3; index++) {
                System.out.println("Looking up next EmployeeServer.");
                service = nextEmployeeService();
                System.out.println("Loading all Employees into cache.");
                employee = service.findAll().get(0);
            }
            System.out.println("Preloading done.");
            Thread.sleep(5000);
            System.out.println("Updating employee on each server.");
            for (int index = 0; index < 10; index++) {
                System.out.println("Looking up next EmployeeServer.");
                service = nextEmployeeService();
                int random = (int)(Math.random() * 1000000);
                System.out.println("Finding employee: " + employee.getId());
                employee = service.findById(employee.getId());
                System.out.println("Employee name was: " + employee.getFirstName() + " " + employee.getLastName());
                employee.setFirstName(String.valueOf(random));
                employee.setLastName(String.valueOf(random));
                employee.setSalary(random);
                try {
                    System.out.println("Updating employee: " + employee.getId());
                    service.update(employee);
                    System.out.println("Employee name updated to: " + employee.getFirstName() + " " + employee.getLastName());
                    Thread.sleep(500);
                } catch (Exception exception) {
                    System.out.println("Updating failed (cache coordination has not yet occurred, increase sleep time).");
                    System.out.println(exception.toString());
                }
            }
        } catch (Exception error) {
            error.printStackTrace();
            throw error;
        }
    }
}
