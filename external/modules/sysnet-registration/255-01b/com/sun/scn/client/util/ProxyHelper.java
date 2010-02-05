package com.sun.scn.client.util;

import java.lang.reflect.Method;

import java.io.FileInputStream;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URISyntaxException;
import java.net.URI;

import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

public class ProxyHelper {
    private static Logger log =
        Logger.getLogger(ProxyHelper.class.getName());

    /**
     * Returns the proxy to use for the destination URL.
     *
     * If no proxy is needed, a proxy of Proxy.NO_PROXY will be returned.
     *
     * @return the proxy to use.
     */
    public static Proxy getProxy(Properties props, String destUrl) {
        boolean useJWSProxySettings = getJwsProxy(props);
        if (useJWSProxySettings && isRunningUnderJWS()) {
            ProxySelector ps = ProxySelector.getDefault();
            try {
                List<Proxy> proxies = ps.select(new URI(destUrl));
                if (proxies.size() > 0) {
                    return proxies.get(0);
                }
            } catch (URISyntaxException e) {
                e.printStackTrace();
            }
            /*

            boolean runningUnderJWS = isRunningUnderJWS();
            if (runningUnderJWS) {
                // if we're running as a web start app
                ProxySelector ps = ProxySelector.getDefault();
                try {
                    List<Proxy> proxies = ps.select(new URI(destUrl));
                    if (proxies.size() > 0) {
                        return proxies.get(0);
                    }
                } catch (URISyntaxException e) {
                    e.printStackTrace();
                }
            } else {
                // if we're here, then we can assume this is a standalone invocation
                // so we'll need to try to obtain the proxy slightly differently
                Properties props = new Properties();
                String file = null;
                String userHome = System.getProperty("user.home");
                String os = System.getProperty("os.name");
                if (os.startsWith("Windows")) {
                    String user = System.getProperty("user.name");
                    file =
                        "C:\\Documents and Settings\\" + user + "\\Application Data"
                        + "\\Sun\\Java\\Deployment\\deployment.properties";

                } else {
                    file = userHome + "/.java/deployment/deployment.properties";
                }
                try {
                    props.load(new FileInputStream(file));
                    String useProxyType = props.getProperty("deployment.proxy.type");
                    if (useProxyType == null || useProxyType.equals("0")) {
                        return Proxy.NO_PROXY;
                    } else if (useProxyType.equals("1")) {
                    } else if (useProxyType.equals("2")) {
                    } else if (useProxyType.equals("3")) {
                    } else {
                        return Proxy.NO_PROXY;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            */
        } else {
            String proxyHost = getProxyHost(props);
            int proxyPort = getProxyPortValue(props);
            if (proxyHost == null || proxyHost.equals("") || proxyPort == -1) {
                return Proxy.NO_PROXY;
            } else {
                SocketAddress addr = new InetSocketAddress(proxyHost, proxyPort);
                return new Proxy(Proxy.Type.HTTP, addr);
            }
        }
        return Proxy.NO_PROXY;
    }

    public static boolean isRunningUnderJWS() {
        try {
            Class serviceManagerClass = Class.forName("javax.jnlp.ServiceManager");
            Method lookupMethod = serviceManagerClass.getMethod("lookup",
                new Class[] { String.class });
            lookupMethod.invoke(null, new Object[] { "javax.jnlp.BasicService" });
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    private static boolean getJwsProxy(Properties props) {
        String s = props.getProperty("jwsProxy", "true");
        return Boolean.valueOf(s).booleanValue();
    }

    private static String getProxyHost(Properties props) {
        return props.getProperty("proxyHost", "");
    }

    private static int getProxyPortValue(Properties props) {
        String s = props.getProperty("proxyPort", "");
        int port = 0;

        try {
            port = Integer.parseInt(s);
        } catch (NumberFormatException nfe) {
            port = -1;
        }

        return port;
    }
}
