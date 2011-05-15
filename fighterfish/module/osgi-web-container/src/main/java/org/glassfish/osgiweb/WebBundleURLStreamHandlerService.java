/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2009-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

package org.glassfish.osgiweb;

import org.glassfish.osgijavaeebase.JarHelper;
import org.osgi.service.url.AbstractURLStreamHandlerService;

import java.io.*;
import java.net.URL;
import java.net.URLConnection;
import java.nio.ByteBuffer;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.glassfish.osgiweb.Constants.WEB_BUNDLE_SCHEME;

/**
 * A {@link org.osgi.service.url.URLStreamHandlerService} for webbundle scheme.
 * It is responsible for not only adding necessary OSGi headers to transform a plain
 * vanilla web app to a Web App Bundle (WAB), but also setting appropriate
 * parameters in the URL object to meet spec's requirement as described below:
 * <p/>
 * The java.net.URL object for a webbundle URL must return the String webbundle
 * when the getProtocol method is called. The embedded URL must be returned in
 * full from the getPath method. The parameters for processing manifest must
 * be returned from the getQuery() method.
 * <p/>
 * Some form of embedded URL also contain query parameters and this must be
 * supported. Thus the value returned from getPath may contain a URL query.
 * Any implementation must take care to preserve both the query parameters for
 * the embedded URL as well as the webbundle URL. A question mark must
 * always follow the embedded URL to simplify this processing. The following example shows
 * an HTTP URL with some query parameter:
 * <p/>
 * webbundle:https://localhost:1234/some/path/?war=example.war?Web-ContextPath=/foo
 * <p/>
 * In this case getPath method of the webbundle URL must return:
 * https://localhost:1234/some/path/?war=example.war
 * <p/>
 * All the parameters in the webbundle: URL are optional except for the Web-ContextPath parameter.
 * The parameter names are case insensitive, but their values must be treated as case sensitive.
 * Since Web-ContextPath url parameter is mandatory, the query component can never be empty:
 * webbundle:http://www.acme.com:8021/sales.war?
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class
        WebBundleURLStreamHandlerService
        extends AbstractURLStreamHandlerService
{
    private static final Logger logger = Logger.getLogger(
            WebBundleURLStreamHandlerService.class.getPackage().getName());

    public URLConnection openConnection(final URL u) throws IOException
    {
        assert (WEB_BUNDLE_SCHEME.equals(u.getProtocol()));
        final URL embeddedURL = new URL(u.getPath());
        final URLConnection con = embeddedURL.openConnection();
        return new URLConnection(embeddedURL)
        {
            private Manifest m;

            public void connect() throws IOException
            {
                con.connect();
            }

            @Override
            public InputStream getInputStream() throws IOException
            {
                connect();
                m = WARManifestProcessor.processManifest(embeddedURL, u.getQuery());
                final PipedOutputStream pos = new PipedOutputStream();
                final PipedInputStream pis = new PipedInputStream(pos);

                // It is a common practice to spawn a separate thread
                // to write to PipedOutputStream so that the reader
                // and writer are not deadlocked.
                new Thread()
                {
                    @Override
                    public void run()
                    {
                        try {
                            writeWithoutSignedFiles(con, pos, m);
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    }

                }.start();
                return pis;
            }
        };
    }

    @Override
    protected void setURL(URL u,
                          String proto,
                          String host,
                          int port,
                          String auth,
                          String user,
                          String path,
                          String query,
                          String ref)
    {
        logger.logp(Level.INFO, "WebBundleURLStreamHandlerService",
                "setURL() called with",
                "u = [{0}], proto = [{1}], host = [{2}], port = [{3}], " +
                        "auth = [{4}], user = [{5}], path = [{6}], " +
                        "query = [{7}], ref = [{7}]",
                new Object[]{u, proto, host, port, auth, user, path, query, ref});

        // Since Web-ContextPath url param must be present, the query can't be null.
        if (query == null || query.isEmpty()) {
            throw new IllegalArgumentException("The query component can't be null. It must at least contain Web-ContextPath parameter.");
        }
        // Let's see if there are two parts in the query. If there are two parts, then the first part belongs to embedded URL, else
        // the query entirely belongs to the outer URL.
        int sep = query.indexOf("?");
        if (sep != -1) { // two parts in the query
            String query1 = query.substring(0, sep);
            String query2 = query.substring(sep+1);
            path = path.concat("?").concat(query1);
            query = query2;
        }
        logger.logp(Level.INFO, "WebBundleURLStreamHandlerService", "setURL ",
                "new path = [{0}], new query = [{1}]",
                new Object[]{path, query});

        // Let's also validate here that query contains Web-ContextPath param
        Map<String, String> queryParameters = WARManifestProcessor.readQueryParams(query);
        boolean containsContextPathKey = false;
        for(String key : queryParameters.keySet()) {
            if (Constants.WEB_CONTEXT_PATH.equalsIgnoreCase(key)) {
                containsContextPathKey = true;
                break;
            }
        }
        if (!containsContextPathKey) {
            throw new IllegalArgumentException("Query [" + query + "] does not contain Web-ContextPath parameter.");
        }
        super.setURL(u, proto, host, port, auth, user, path, query, ref);
    }

    private void writeWithoutSignedFiles(URLConnection con, PipedOutputStream os, Manifest m) throws IOException {
        JarInputStream jis = null;
        JarOutputStream jos = null;
        try {
            jis = new JarInputStream(con.getInputStream());
            jos = new JarOutputStream(os, m);
            writeWithoutSignedFiles(jis, jos);
        } finally {
            if (jis != null) try { jis.close(); } catch (IOException e) {}
            if (jos != null) try { jos.close(); } catch (IOException e) {}
        }
    }

    private void writeWithoutSignedFiles(final JarInputStream jis, final JarOutputStream jos) throws IOException {
        // Ideally we should enhance JarHelper.write() method to accept a filter that it could use to exclude files
        // from being written out. But, since such a method does not exist, we will write it here itself.
//                        JarHelper.write(con, pos, m);
        JarHelper.accept(jis, new JarHelper.Visitor() {
            ByteBuffer buffer = ByteBuffer.allocate(1024);
            @Override
            public void visit(JarEntry je) {
                try {
                    final String name = je.getName();
                    // Refer to http://download.oracle.com/javase/6/docs/technotes/guides/jar/jar.html#Signed%20JAR%20File
                    if (name.startsWith("META-INF/") && !name.substring("META-INF/".length()).contains("/") &&
                            (name.endsWith(".SF") || name.endsWith(".RSA") || name.endsWith(".DSA") || name.startsWith("SIG-"))) {
                        logger.logp(Level.INFO, "WebBundleURLStreamHandlerService", "visit",
                                                    "Skipping writing of singature file {0}", new Object[]{name});
                        return;
                    }
                    logger.logp(Level.FINE, "WebBundleURLStreamHandlerService", "visit", "Writing jar entry = {0}",
                            new Object[]{je});
                    jos.putNextEntry(je);
                    JarHelper.copy(jis, jos, buffer);
                    jos.closeEntry();
                } catch (IOException e) {
                    throw new RuntimeException(e); // TODO(Sahoo): Proper Exception Handling
                }
            }
        });
    }
}
