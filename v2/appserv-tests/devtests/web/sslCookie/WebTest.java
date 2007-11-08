import java.io.*;
import java.util.Properties;
import java.net.*;
import java.security.KeyStore;
import javax.net.*;
import javax.net.ssl.*;
import com.sun.ejte.ccl.reporter.*;

public class WebTest{


    static SimpleReporterAdapter stat=
           new SimpleReporterAdapter("appserv-tests");
    
    private static int count = 0;
    private static int EXPECTED_COUNT = 3;
    private static boolean firstConnection = true;
    private static String requestedSessionId="";
    private static String requestUri = "" ;
    private static String sessionFalseId = "";

    public static void main(String args[]) throws Exception{
        String host = args[0];
        String port = args[1];
        String contextRoot = args[2];
        String trustStorePath = args[3];

        stat.addDescription("Cookie under SSL");

        try {
            SSLSocketFactory ssf = getSSLSocketFactory(trustStorePath);

            HttpsURLConnection connection = doSSLHandshake(
                            "https://" + host  + ":" + port + "/" + contextRoot
                            + "/ServletTest;jsessionid=01A960C22480CE9F445CDE48DE333F31", ssf);

            parseResponse(connection);
            
            firstConnection = false;
            connection = doSSLHandshake(
                "https://" + host  + ":" + port + "/" + contextRoot 
                + "/ServletTest;jsessionid=" + sessionFalseId, ssf);
            parseResponse(connection);
            
        } catch (Throwable t) {
            t.printStackTrace();
        } finally {
            if (count != EXPECTED_COUNT){
                stat.addStatus("web-sslCookie", stat.FAIL);
            }           
        }


        stat.printSummary("web/sslCookie ---> expect 3 PASS");
    }

    private static SSLSocketFactory getSSLSocketFactory(String trustStorePath)
                    throws Exception {
        SSLContext sc = SSLContext.getInstance("SSL");
        sc.init(null, getTrustManagers(trustStorePath), null);
        return sc.getSocketFactory();
    }

    private static HttpsURLConnection doSSLHandshake(String urlAddress,
                                                     SSLSocketFactory ssf)
                    throws Exception{

        URL url = new URL(urlAddress);
        HttpsURLConnection.setDefaultSSLSocketFactory(ssf);
        HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();

        connection.setHostnameVerifier(
            new HostnameVerifier() {
                public boolean verify(String rserver, SSLSession sses) {
                    return true;
                }
        });
        connection.setDoOutput(true);
        return connection;
    }

    private static int lineNum = 0;

    private static void parseResponse(HttpsURLConnection connection)
                    throws Exception{

       BufferedReader in = new BufferedReader(
                new InputStreamReader(connection.getInputStream()));
            
        String line= ""; 
        int index = 0;
        while ((line = in.readLine()) != null) {
            index = line.indexOf("::");
            System.out.println(lineNum + ":  " + line);
            if (index != -1) {
                String status = line.substring(index+2);
                String context = line.substring(0, index);
                System.out.println("context: " + context + " status: " + status);
                if (firstConnection){
                    if ( context.equalsIgnoreCase("getRequestSessionId") )
                        requestedSessionId = status; 
                    else if ( context.equalsIgnoreCase("getSession(false).getId")) {
                        sessionFalseId = status;
                    } else if ( context.equalsIgnoreCase("getRequestURI"))
                        requestUri = status;
                } else {
                    if ( context.equalsIgnoreCase("getRequestSessionId") ) {
                        System.out.println(requestedSessionId + " (1) " + status);
                        if (sessionFalseId.equalsIgnoreCase(status)) {
                            stat.addStatus("web-sslCookie: " + context,
                                           stat.PASS);
                        } else {
                            stat.addStatus("web-sslCookie: getRequestSessionId", stat.FAIL);
                        }
                    } else if ( context.equalsIgnoreCase("getSession(false).getId")){
                        System.out.println(sessionFalseId + " (2) " + status);
                        if (sessionFalseId.equalsIgnoreCase(status)) {
                            stat.addStatus("web-sslCookie: " + context,
                                           stat.PASS);
                        } else {
                            stat.addStatus("web-sslCookie: getSession(false).getId", stat.FAIL);
                        }
                    } else if ( context.equalsIgnoreCase("getRequestURI")) {
                        System.out.println(requestUri + " (3) " + status);
                        if (requestUri.equalsIgnoreCase(status)) {
                            stat.addStatus("web-sslCookie: " + context,
                                           stat.PASS);
                        } else {
                            stat.addStatus("web-sslCookie: getRequestURI", stat.FAIL);
                        }
                    }
                    count++;
                } 
            }
            lineNum++;
        }

        in.close();
    }

    private static TrustManager[] getTrustManagers(String path)
                    throws Exception {

        TrustManager[] tms = null;
        InputStream istream = null;

        try {
            KeyStore trustStore = KeyStore.getInstance("JKS");
            istream = new FileInputStream(path);
            trustStore.load(istream, null);
            istream.close();
            istream = null;
            TrustManagerFactory tmf = TrustManagerFactory.getInstance("SunX509");
            tmf.init(trustStore);
            tms = tmf.getTrustManagers();

        } finally {
            if (istream != null) {
                try {
                    istream.close();
                } catch (IOException ioe) {
                    // Do nothing
                }
            }
        }

        return tms;
    }

}
