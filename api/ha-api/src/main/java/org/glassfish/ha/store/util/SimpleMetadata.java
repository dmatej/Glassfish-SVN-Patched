/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 1997-2010 Oracle and/or its affiliates. All rights reserved.
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

package org.glassfish.ha.store.util;

import org.glassfish.ha.store.annotations.Attribute;
import org.glassfish.ha.store.api.Storeable;

import java.io.*;


public class SimpleMetadata implements Storeable {

    private long version = -1;

    private long lastAccessTime;

    private long maxInactiveInterval;

    private byte[] state;

    private static final String[] attributeNames = new String[] {"state"};

    private static final boolean[] dirtyStatus = new boolean[] {true};

    //Default No arg constructor required for BackingStore

    public SimpleMetadata() {

    }

    /**
     * Construct a SimpleMetadata object
     *
     * @param version             The version of the data. A freshly created state has a version == 0
     * @param lastAccesstime      the last access time of the state. This must be used in
     *                            conjunction with getMaxInactiveInterval to determine if the
     *                            state is idle enough to be removed.
     * @param maxInactiveInterval the maximum time that this state can be idle in the store
     *                            before it can be removed.
     */
    public SimpleMetadata(long version, long lastAccesstime,
                          long maxInactiveInterval, byte[] state)

    {
        this.version = version;
        this.lastAccessTime = lastAccesstime;
        this.maxInactiveInterval = maxInactiveInterval;
        this.state = state;
    }

    /**
     * Get the verion of the state. A freshly created state has a version == 0
     *
     * @return the version.
     */
    public long getVersion() {
        return version;
    }

    public void setVersion(long version) {
        this.version = version;
    }

    /**
     * Get the last access time of the state. This must be used in conjunction
     * with getMaxInactiveInterval to determine if the state is idle enough to
     * be removed.
     *
     * @return The time when the state was accessed last
     */
    public long getLastAccessTime() {
        return lastAccessTime;
    }

    public void setLastAccessTime(long lastAccessTime) {
        this.lastAccessTime = lastAccessTime;
    }

    /**
     * Get the maximum time that this state can be idle in the store before it
     * can be removed.
     *
     * @return the maximum idle time. If zero or negative, then the component
     *         has no idle timeout limit
     */
    public long getMaxInactiveInterval() {
        return this.maxInactiveInterval;
    }

    public void setMaxInactiveInterval(long maxInactiveInterval) {
        this.maxInactiveInterval = maxInactiveInterval;
    }

    public byte[] getState() {
        return this.state;
    }

    @Attribute("state")
    public void setState(byte[] state) {
        this.state = state;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("SimpleMetadata->state");
        if (state != null) {
            for (byte b : state) {
                sb.append(b + "_");
            }
        } else {
            sb.append("null");
        }
        return "SimpleMetadata{" +
                "version=" + version +
                ", lastAccessTime=" + lastAccessTime +
                ", maxInactiveInterval=" + maxInactiveInterval +
                ", state.length=" + (state == null ? 0 : state.length) +
                ", state=" + sb.toString() +
                '}';
    }

    public long _storeable_getVersion() {
        return this.version;
    }

    public void _storeable_setVersion(long val) {
        this.version = val;
    }

    public long _storeable_getLastAccessTime() {
        return this.lastAccessTime;
    }

    public void _storeable_setLastAccessTime(long val) {
        this.lastAccessTime = val;
    }

    public long _storeable_getMaxIdleTime() {
        return this.maxInactiveInterval;
    }

    public void _storeable_setMaxIdleTime(long val) {
        this.maxInactiveInterval = val;
    }

    public String[] _storeable_getAttributeNames() {
        return attributeNames;
    }

    public boolean[] _storeable_getDirtyStatus() {
        return dirtyStatus;
    }

    public void _storeable_writeState(OutputStream os) throws IOException {
        ObjectOutputStream oos = new ObjectOutputStream(os);
        try {
            oos.writeInt(state.length);
            oos.write(state);
        } finally {
            try {
                oos.close();
            } catch (Exception ex) {
            }
        }
    }

    public void _storeable_readState(InputStream is) throws IOException {
        ObjectInputStream ois = new ObjectInputStream(is);
        try {
            int len = ois.readInt();
            state = new byte[len];
            ois.readFully(state);
        } finally {
            try {
                ois.close();
            } catch (Exception ex) {
            }
        }
    }
}
