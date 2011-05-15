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

import org.glassfish.osgijavaeebase.OSGiApplicationInfo;
import org.glassfish.osgijavaeebase.OSGiContainer;
import org.osgi.framework.*;
import org.osgi.util.tracker.BundleTracker;
import org.osgi.util.tracker.BundleTrackerCustomizer;
import org.osgi.util.tracker.ServiceTracker;

import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.glassfish.osgiweb.Util.getContextPath;

/**
 * Detects collision in Web-ContextPath
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
class ContextPathCollisionDetector implements BundleListener {

    // This class is an asynchronous bundle listener, because it uses the events for clean up purpose. Since such clean up is considered
    // more of a book keeping rather than essential, we don't want to do it synchronously.


    private static ContextPathCollisionDetector _me = new ContextPathCollisionDetector();

    private static Logger logger = Logger.getLogger(ContextPathCollisionDetector.class.getPackage().getName());

    /**
     *  What is the currently deployed bundle for a given context path
     */
    private Map<String, Long> contextPath2WabMap = new HashMap<String, Long>();

    /**
     *  What are the colliding WABs for a given context path in addition to the currently dpeloying/deployed bundle.
     */
    private Map<String, List<Long>> contextPath2CollidingWabsMap = new HashMap<String, List<Long>>();

    private ServiceTracker osgiContainerTracker = new ServiceTracker(getBundle().getBundleContext(),
            OSGiContainer.class.getName(), null);
    private boolean stopped;

    private ContextPathCollisionDetector() {
        osgiContainerTracker.open();
        getBundle().getBundleContext().addBundleListener(this);
    }

    public static ContextPathCollisionDetector get() {
        return _me;
    }

    synchronized void stop() {
        getBundle().getBundleContext().removeBundleListener(this);
        osgiContainerTracker.close();
        stopped = true;
    }

    public synchronized void preDeploy(Bundle bundle) throws ContextPathCollisionException {
        if (stopped) return;
        String contextPath = getContextPath(bundle);
        Long deployedBundle = getCurrentlyDeployedBundle(contextPath);
        final Long bundleId = bundle.getBundleId();
        if (deployedBundle == null) {
            assert(getCollidingWabs(contextPath).isEmpty());
            setCurrentlyDeployedBundle(contextPath, bundleId);
        } else {
            // There are two possibilities, viz:
            // a. we are called from postUndeploy() of this CollisionDetector. In this case, skip any check.
            // b. it's a fresh deploy.
            if (deployedBundle.equals(bundleId)) { // case #a
                // This happens when collision detector attempts to deploy a bundle from the colliding WABs list.
                // See postUndeploy() for more details. // Skip any check and return.
                return;
            } else { // case #b
                addCollidingWab(contextPath, bundleId);
                throw new ContextPathCollisionException(contextPath, getAllWabs(contextPath).toArray(new Long[0]));
            }
        }
    }

    public synchronized void postUndeploy(Bundle bundle) {
        if (stopped) return;
        Long bundleId = bundle.getBundleId();
        String contextPath = getContextPath(bundle);
        Long deployedBundle = getCurrentlyDeployedBundle(contextPath);
        assert(bundleId.equals(deployedBundle));
        unsetCurrentlyDeployedBundle(contextPath);
        List<Long> collidingWabs = getCollidingWabs(contextPath);

        // attempt to deploy bundle with lowest bundle id having same context path
        // Although the spec does not require us to attempt to deploy more than the first candidate, we try to deploy
        // other WABs in case the first candidate does not get deployed for whatever reason like it's state has changed, e.g.
        Collections.sort(collidingWabs);
        ListIterator<Long> li = collidingWabs.listIterator(); // use an iterator as we are removing entries
        while (li.hasNext()) {
            Long nextBundleInList = li.next();
            logger.logp(Level.INFO, "CollisionDetector", "postUndeploy",
                    "Collision detector is attempting to deploy bundle {0} with context path {1} ",
                    new Object[]{nextBundleInList, contextPath});
            try {
                final Bundle nextBundle = getBundle(nextBundleInList);
                // Important protocol:
                // remove it from colliding wab and set it as currently deployed wab
                // By setting it in contextPath2WabMap, we inform the preDeploy() method that it should not detect this as a collision.
                li.remove();
                if (nextBundle == null) {
                    // can happen if bundle has been uninstalled and we have not managed to clean ourselves up due to inherent timing issues
                    logger.logp(Level.INFO, "ContextPathCollisionDetector", "postUndeploy",
                            "Collision detector is skipping bundle [{0}], for it has been uninstalled.",
                            new Object[]{nextBundle});
                    continue;
                }
                setCurrentlyDeployedBundle(contextPath, nextBundleInList);
                OSGiApplicationInfo osgiApplicationInfo = getOSGiContainer().deploy(nextBundle);
                if (osgiApplicationInfo != null) {
                    break; // break after first successful deploy
                }
            } catch (Exception e) {
                unsetCurrentlyDeployedBundle(contextPath); // clean up, as we can't rely on cleanUp() being called by container
                logger.logp(Level.WARNING, "CollisionDetector", "postUndeploy",
                        "Collision detector got exception while trying to deploy the bundle with lowest id", e);
            }
        }
    }

    public synchronized void cleanUp(Bundle bundle) {
        String contextPath = getContextPath(bundle);
        final Long bundleId = bundle.getBundleId();
        final Long deployedBundle = getCurrentlyDeployedBundle(contextPath);
        assert(bundleId.equals(deployedBundle));
        unsetCurrentlyDeployedBundle(contextPath);
        logger.logp(Level.INFO, "CollisionDetector", "cleanUp",
                "Removed bundle {0} against context path {1} ", new Object[]{bundleId, contextPath});
    }

    private synchronized Long getCurrentlyDeployedBundle(String contextPath) {
        return contextPath2WabMap.get(contextPath);
    }

    private synchronized void setCurrentlyDeployedBundle(String contextPath, Long bundleId) {
        assert(bundleId != null);
        contextPath2WabMap.put(contextPath, bundleId);
    }

    private synchronized void unsetCurrentlyDeployedBundle(String contextPath) {
        contextPath2WabMap.put(contextPath, null);
    }
    /**
     *  Get list of colliding bundles with given context path. This method does not return the currently deployed bundle.
     * @param contextPath
     * @return
     */
    private synchronized List<Long> getCollidingWabs(final String contextPath) {
        List<Long> bundleIds = contextPath2CollidingWabsMap.get(contextPath);
        if (bundleIds == null) {
            bundleIds = new ArrayList<Long>();
            contextPath2CollidingWabsMap.put(contextPath, bundleIds);
        }
        return bundleIds;
    }

    private synchronized List<Long> addCollidingWab(String contextPath, Long bundleId) {
        final List<Long> bundleIds = getCollidingWabs(contextPath);
        bundleIds.add(bundleId);
        return bundleIds;
    }

    private synchronized boolean removeCollingWab(String contextPath, final long bundleId) {
        return getCollidingWabs(contextPath).remove(bundleId);
    }

    private synchronized List<Long> getAllWabs(String contextPath) {
        List<Long> result = new ArrayList<Long>(getCollidingWabs(contextPath));
        final Long deployedBundle = getCurrentlyDeployedBundle(contextPath);
        if (deployedBundle != null) {
            result.add(0, deployedBundle); // add it at the beginning
        }
        return result;
    }

    private Bundle getBundle(Long bundleId) {
        return getBundle().getBundleContext().getBundle(bundleId);
    }

    private OSGiContainer getOSGiContainer() {
        return (OSGiContainer) osgiContainerTracker.getService();
    }

    private Bundle getBundle() {
        return BundleReference.class.cast(getClass().getClassLoader()).getBundle();
    }

    @Override
    public void bundleChanged(BundleEvent event) {
        if (BundleEvent.STOPPED == event.getType()) {
            Bundle bundle = event.getBundle();
            String contextPath = getContextPath(bundle);
            if (contextPath != null && removeCollingWab(contextPath, bundle.getBundleId())) {
                logger.logp(Level.INFO, "CollisionDetector", "bundleChanged",
                        "Removed bundle [{0}] from colliding bundles list for contextPath {1}",
                        new Object[]{bundle.getBundleId(), contextPath});
            }
        }
    }
}
