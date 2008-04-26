/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
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
 *
 */

package org.glassfish.embed.impl;

import org.glassfish.api.deployment.archive.ReadableArchive;
import org.jvnet.hk2.annotations.Service;

import java.io.InputStream;
import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;
import java.net.URI;
import java.net.URL;
import java.util.Enumeration;
import java.util.Collection;
import java.util.jar.Manifest;

/**
 * @author Kohsuke Kawaguchi
 */
@Service
public class ScatteredWarArchive implements ReadableArchive {
    public final File resources;

    /**
     * Location of <tt>web.xml</tt>
     */
    public final File webXml;

    public final Collection<URL> classes;

    /**
     *
     * @param webXml
     *      if null, defaults to {@code WEB-INF/web.xml} under {@code resources}.
     */
    public ScatteredWarArchive(File resources, File webXml, Collection<URL> classes) {
        this.resources = resources;
        if(webXml==null)
            webXml = new File(resources,"WEB-INF/web.xml");
        this.webXml = webXml;
        this.classes = classes;
    }

    public InputStream getEntry(String name) throws IOException {
        File f = new File(resources, name);
        if(f.exists())  return new FileInputStream(f);
        return null;
    }

    public boolean exists(String name) throws IOException {
        return new File(resources,name).exists();
    }

    public long getEntrySize(String name) {
        // TODO
        throw new UnsupportedOperationException();
    }

    public void open(URI uri) throws IOException {
        // TODO
        throw new UnsupportedOperationException();
    }

    public ReadableArchive getSubArchive(String name) throws IOException {
        // TODO
        throw new UnsupportedOperationException();
    }

    public boolean exists() {
        // TODO
        throw new UnsupportedOperationException();
    }

    public boolean delete() {
        // TODO
        throw new UnsupportedOperationException();
    }

    public boolean renameTo(String name) {
        // TODO
        throw new UnsupportedOperationException();
    }

    public void close() throws IOException {
        // TODO
        throw new UnsupportedOperationException();
    }

    public Enumeration<String> entries() {
        // TODO
        throw new UnsupportedOperationException();
    }

    public Enumeration entries(String prefix) {
        // TODO
        throw new UnsupportedOperationException();
    }

    public Manifest getManifest() throws IOException {
        // TODO
        throw new UnsupportedOperationException();
    }

    public URI getURI() {
        // TODO
        throw new UnsupportedOperationException();
    }

    public long getArchiveSize() throws SecurityException {
        // TODO
        throw new UnsupportedOperationException();
    }

    public String getName() {
        // TODO
        throw new UnsupportedOperationException();
    }
}
