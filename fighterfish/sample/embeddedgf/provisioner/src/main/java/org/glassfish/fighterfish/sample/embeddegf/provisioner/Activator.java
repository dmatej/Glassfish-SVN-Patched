/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
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


package org.glassfish.fighterfish.sample.embeddegf.provisioner;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * A simple bundle that can download GlassFish zip from a URL specified using {@link GLASSFISH_ARCHIVE_URL}
 * into a location specified in {@link INSTALLATION_DIR} property. It then bootstraps GlassFish in the same
 * JVM.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class Activator implements BundleActivator {

    private final String GLASSFISH_ARCHIVE_URL = "fighterfish.provisioner.url";
    private final String INSTALLATION_DIR = "fighterfish.provisioner.destination";

    private File dest;
    private Logger logger = Logger.getLogger(getClass().getPackage().getName());

    @Override
    public void start(BundleContext context) throws Exception {
        try {
            String out = context.getProperty(INSTALLATION_DIR);
            if (out != null) {
                dest = new File(out);
            } else {
                dest = context.getDataFile(""); // get base dir
            }
            if (needToExplode()) {
                if(dest.mkdirs());
                explode(context);
            }
            startGlassFishBundle(context);
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }

    private void startGlassFishBundle(BundleContext context) throws BundleException {
        Bundle bundle = context.installBundle(new File(dest, "glassfish3/glassfish/modules/glassfish.jar").toURI().toString());
        System.setProperty("com.sun.aas.installRoot", new File(dest, "glassfish3/glassfish/").getAbsolutePath());
        System.setProperty("com.sun.aas.instanceRoot", new File(dest, "glassfish3/glassfish/domains/domain1/").getAbsolutePath());
        bundle.start(Bundle.START_TRANSIENT);
    }

    private boolean needToExplode() throws Exception {
        if (new File(dest, "glassfish3").isDirectory()) {
            return false;
        }
        if (dest.isFile()) {
            throw new Exception(dest.getAbsolutePath() + " is a file");
        }
        return true;
    }

    private void explode(BundleContext context) throws Exception {
        String in = context.getProperty(GLASSFISH_ARCHIVE_URL);
        logger.info("Provisioning URL = " + in);
        if (in != null) {
            URL url = new URL(in);
            logger.info("Opening stream");
            InputStream is = url.openStream();
            ZipInputStream zis = new ZipInputStream(url.openStream());
            try {
                extractZip(zis);
            } finally {
                zis.close();
            }
        } else {
            throw new Exception("Pl specify GlassFish archive URL in a property called " + GLASSFISH_ARCHIVE_URL);
        }
    }

    @Override
    public void stop(BundleContext context) throws Exception {
    }

    public void extractZip(ZipInputStream zis) throws IOException {
        logger.logp(Level.INFO, "Activator", "extractZip", "dest = {0}", new Object[]{dest});
        ZipEntry ze;
        int n = 0;
        int size = 0;
        while ((ze = zis.getNextEntry()) != null) {
            logger.logp(Level.FINER, "Activator", "extractZip", "ZipEntry name = {0}, size = {1}", new Object[]{ze.getName(), ze.getSize()});
            java.io.File f = new java.io.File(dest + java.io.File.separator + ze.getName());
            if (ze.isDirectory()) {
                if (!f.exists()) {
                    if (!f.mkdirs()) {
                        throw new IOException("Unable to create dir " + f.getAbsolutePath());
                    }
                } else if (f.isFile()) { // exists, but not a file. not sure how this can happen
                    throw new IOException("Unable to create dir " + f.getAbsolutePath() + " because it already exists as a file.");
                }
                continue;
            } else if (f.exists()) {
                continue;
            }
            FileOutputStream fos = null;
            try {
                fos = new FileOutputStream(f);
                int totalcount = 0;
                int count = 0;
                byte[] buffer = new byte[8192];
                while ((count = zis.read(buffer, 0, buffer.length)) != -1) {
                    fos.write(buffer, 0, count);
                    totalcount += count;
                }
                logger.logp(Level.FINER, "Activator", "extractZip", "totalcount for this zip entry = {0}", new Object[]{totalcount});
            } finally {
                try {
                    if (fos != null) {
                        fos.close();
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            zis.closeEntry();
            n++;
            size += ze.getSize();
        }
        logger.logp(Level.INFO, "Activator", "extractZip", "Extracted {0} of entries of total size {1} bytes.", new Object[]{n, size});

    }

}
