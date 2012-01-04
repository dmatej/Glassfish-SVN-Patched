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


package org.glassfish.fighterfish.test.util;

import org.osgi.framework.BundleContext;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * A simple utility to extract a zip input stream. This is used to install GlassFish when user does not have
 * an installation.
 *
 * @author sanjeeb.sahoo@oracle.com
 */
public class ZipUtil {

    private static Logger logger = Logger.getLogger(ZipUtil.class.getPackage().getName());

    private static boolean needToExplode(File dest) throws Exception {
        if (new File(dest, "glassfish3").isDirectory()) {
            return false;
        }
        if (dest.isFile()) {
            throw new Exception(dest.getAbsolutePath() + " is a file");
        }
        return true;
    }

    public static void explode(URI in, File out) throws Exception {
        assert (in != null);
        logger.entering("ZipUtil", "explode", new Object[]{in, out});
        if (!needToExplode(out)) {
            logger.logp(Level.FINE, "ZipUtil", "explode", "Skipping exploding at {0}", new Object[]{out});
        }
        if (in != null) {
            ZipInputStream zis = new ZipInputStream(in.toURL().openStream());
            try {
                extractZip(zis, out);
            } finally {
                zis.close();
            }
        }
    }

    public static void extractZip(ZipInputStream zis, File destDir) throws IOException {
        logger.logp(Level.FINE, "ZipUtil", "extractZip", "destDir = {0}", new Object[]{destDir});
        ZipEntry ze;
        int n = 0;
        int size = 0;
        while ((ze = zis.getNextEntry()) != null) {
            logger.logp(Level.FINER, "ZipUtil", "extractZip", "ZipEntry name = {0}, size = {1}", new Object[]{ze.getName(), ze.getSize()});
            java.io.File f = new java.io.File(destDir + java.io.File.separator + ze.getName());
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
                logger.logp(Level.FINER, "ZipUtil", "extractZip", "totalcount for this zip entry = {0}", new Object[]{totalcount});
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
        logger.logp(Level.INFO, "ZipUtil", "extractZip", "Extracted {0} of entries of total size {1} bytes to {2}.",
                new Object[]{n, size, destDir.getAbsolutePath()});
    }
}
