/*
 * $Id: FactoryFinder.java,v 1.2 2003/03/27 00:38:25 ofung Exp $
 * $Revision: 1.2 $
 * $Date: 2003/03/27 00:38:25 $
 */

/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc;

import java.io.InputStream;
import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;

import java.util.Properties;
import java.io.BufferedReader;
import java.io.InputStreamReader;

class FactoryFinder {

    /**
     * Creates an instance of the specified class using the specified 
     * <code>ClassLoader</code> object.
     *
     * @exception ServiceException if the given class could not be found
     *            or could not be instantiated
     */
    private static Object newInstance(String className,
                                      ClassLoader classLoader) throws ServiceException
    {
        try {
            Class spiClass;
            if (classLoader == null) {
                spiClass = Class.forName(className);
            } else {
                spiClass = classLoader.loadClass(className);
            }
            return spiClass.newInstance();
        } catch (ClassNotFoundException x) {
            throw new ServiceException(
                "Provider " + className + " not found", x);
        } catch (Exception x) {
            throw new ServiceException(
                "Provider " + className + " could not be instantiated: " + x,
                x);
        }
    }

    /**
     * Finds the implementation <code>Class</code> object for the given
     * factory name, or if that fails, finds the <code>Class</code> object
     * for the given fallback class name. The arguments supplied must be
     * used in order. If using the first argument is successful, the second
     * one will not be used.
     * <P>
     * This method is package private so that this code can be shared.
     *
     * @return the <code>Class</code> object of the specified message factory;
     *         may not be <code>null</code>
     *
     * @param factoryId             the name of the factory to find, which is
     *                              a system property
     * @param fallbackClassName     the implementation class name, which is
     *                              to be used only if nothing else
     *                              is found; <code>null</code> to indicate that
     *                              there is no fallback class name
     * @exception ServiceException if there is an error
     */
    static Object find(String factoryId, String fallbackClassName) throws ServiceException
    {
        ClassLoader classLoader;
        try {
            classLoader = Thread.currentThread().getContextClassLoader();
        } catch (Exception x) {
            throw new ServiceException(x.toString(), x);
        }

        // Use the system property first
        try {
            String systemProp =
                System.getProperty( factoryId );
            if( systemProp!=null) {
                return newInstance(systemProp, classLoader);
            }
        } catch (SecurityException se) {
        }

        // try to read from $java.home/lib/jaxrpc.properties
        try {
            String javah=System.getProperty( "java.home" );
            String configFile = javah + File.separator +
                "lib" + File.separator + "jaxrpc.properties";
            File f=new File( configFile );
            if( f.exists()) {
                Properties props=new Properties();
                props.load( new FileInputStream(f));
                String factoryClassName = props.getProperty(factoryId);
                return newInstance(factoryClassName, classLoader);
            }
        } catch(Exception ex ) {
        }

        String serviceId = "META-INF/services/" + factoryId;
        // try to find services in CLASSPATH
        try {
            InputStream is=null;
            if (classLoader == null) {
                is=ClassLoader.getSystemResourceAsStream(serviceId);
            } else {
                is=classLoader.getResourceAsStream(serviceId);
            }
        
            if( is!=null ) {
                BufferedReader rd =
                    new BufferedReader(new InputStreamReader(is, "UTF-8"));
        
                String factoryClassName = rd.readLine();
                rd.close();

                if (factoryClassName != null &&
                    ! "".equals(factoryClassName)) {
                    return newInstance(factoryClassName, classLoader);
                }
            }
        } catch( Exception ex ) {
        }

        if (fallbackClassName == null) {
            throw new ServiceException(
                "Provider for " + factoryId + " cannot be found", null);
        }

        return newInstance(fallbackClassName, classLoader);
    }
}
