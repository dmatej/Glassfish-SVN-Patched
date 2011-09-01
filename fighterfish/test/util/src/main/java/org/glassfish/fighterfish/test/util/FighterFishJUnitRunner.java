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

import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.Statement;
import org.ops4j.pax.exam.*;
import org.ops4j.pax.exam.junit.ExamFactory;
import org.ops4j.pax.exam.junit.ExamReactorStrategy;
import org.ops4j.pax.exam.junit.ProbeBuilder;
import org.ops4j.pax.exam.spi.*;
import org.ops4j.pax.exam.spi.reactors.EagerSingleStagedReactorFactory;

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.junit.Assert.fail;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class FighterFishJUnitRunner extends BlockJUnit4ClassRunner {

    private Logger LOG = Logger.getLogger(getClass().getPackage().getName());

    final private StagedExamReactor m_reactor;
    final private Map<TestAddress, FrameworkMethod> m_map = new HashMap<TestAddress, FrameworkMethod>();
    final private Map<FrameworkMethod, TestAddress> m__childs = new HashMap<FrameworkMethod, TestAddress>();

    private ExamSystem m_system;

    public FighterFishJUnitRunner(Class<?> klass)
            throws Exception {
        super(klass);

        m_reactor = prepareReactor();
    }

    @Override
    public void run(RunNotifier notifier) {
        try {
            super.run(notifier);
        } catch (Exception e) {
            throw new TestContainerException("Problem interacting with reactor.", e);
        } finally {
            m_reactor.tearDown();
            m_system.clear();
        }
    }

    /**
     * We overwrite those with reactor content
     */
    @Override
    protected List<FrameworkMethod> getChildren() {
        if (m__childs.isEmpty()) {
            fillChildren();
        }
        return Arrays.asList(m__childs.keySet().toArray(new FrameworkMethod[m__childs.size()]));
    }

    private void fillChildren() {
        Set<TestAddress> targets = m_reactor.getTargets();
        for (final TestAddress address : targets) {
            final FrameworkMethod frameworkMethod = m_map.get(address.root());

            // now, someone later may refer to that artificial FrameworkMethod. We need to be able to tell the address.
            FrameworkMethod method = new FrameworkMethod(frameworkMethod.getMethod()) {
                @Override
                public String getName() {
                    return frameworkMethod.getName() + ":" + address.caption();
                }

                @Override
                public boolean equals(Object obj) {
                    return address.equals(obj);
                }

                @Override
                public int hashCode() {
                    return address.hashCode();
                }
            };

            m__childs.put(method, address);
        }
    }

    @Override
    protected void collectInitializationErrors
            (List<Throwable> errors) {
        // do nothing
    }

    private synchronized StagedExamReactor prepareReactor()
            throws Exception {
        m_system = PaxExamRuntime.createTestSystem();
        Class testClass = getTestClass().getJavaClass();
        Object testClassInstance = testClass.newInstance();
        ExxamReactor reactor = getReactor(testClass);

        addConfigurationsToReactor(reactor, testClass, testClassInstance);
        addTestsToReactor(reactor, testClass, testClassInstance);
        return reactor.stage(getFactory(testClass));
    }

    private void addConfigurationsToReactor(ExxamReactor reactor, Class testClass, Object testClassInstance)
            throws IllegalAccessException, InvocationTargetException, IllegalArgumentException, IOException {
        reactor.addConfiguration(TestsConfiguration.getInstance().getPaxExamConfiguration());
    }

    private void addTestsToReactor(ExxamReactor reactor, Class testClass, Object testClassInstance)
            throws IOException, ExamConfigurationException {
        TestProbeBuilder probe = m_system.createProbe();
        probe = overwriteWithUserDefinition(testClass, testClassInstance, probe);

        //probe.setAnchor( testClass );
        for (FrameworkMethod s : super.getChildren()) {
            // record the method -> adress matching
            TestAddress address = delegateTest(testClassInstance, probe, s);
            if (address == null) {
                address = probe.addTest(testClass, s.getMethod().getName());
            }
            m_map.put(address, s);
        }
        reactor.addProbe(probe.build());
    }

    private TestAddress delegateTest(Object testClassInstance, TestProbeBuilder probe, FrameworkMethod s) {
        try {
            Class<?>[] types = s.getMethod().getParameterTypes();
            if (types.length == 1 && types[0].isAssignableFrom(TestProbeBuilder.class)) {
                // do some backtracking:
                return (TestAddress) s.getMethod().invoke(testClassInstance, probe);

            } else {
                return null;
            }
        } catch (Exception e) {
            throw new TestContainerException("Problem delegating to test.", e);
        }
    }

    @SuppressWarnings("unchecked")
    private StagedExamReactorFactory getFactory(Class testClass)
            throws InstantiationException, IllegalAccessException {
        ExamReactorStrategy strategy = (ExamReactorStrategy) testClass.getAnnotation(ExamReactorStrategy.class);

        StagedExamReactorFactory fact;
        if (strategy != null) {
            fact = strategy.value()[0].newInstance();
        } else {
            // default:
            fact = new EagerSingleStagedReactorFactory();
        }
        return fact;
    }

    private DefaultExamReactor getReactor(Class testClass)
            throws InstantiationException, IllegalAccessException {
        return new DefaultExamReactor(m_system, getExamFactory(testClass));
    }

    @SuppressWarnings("unchecked")
    private TestContainerFactory getExamFactory(Class testClass)
            throws IllegalAccessException, InstantiationException {
        ExamFactory f = (ExamFactory) testClass.getAnnotation(ExamFactory.class);

        TestContainerFactory fact;
        if (f != null) {
            fact = f.value().newInstance();
        } else {
            // default:
            fact = PaxExamRuntime.getTestContainerFactory();
        }
        return fact;
    }

    protected synchronized Statement methodInvoker(final FrameworkMethod method, final Object test) {
        return new Statement() {

            @Override
            public void evaluate()
                    throws Throwable {
                TestAddress address = m__childs.get(method);
                TestAddress root = address.root();

                LOG.fine("Invoke " + method.getName() + " @ " + address + " Arguments: " + root.arguments());
                try {
                    m_reactor.invoke(address);
                } catch (Exception e) {
                    Throwable t = ExceptionHelper.unwind(e);
                    LOG.log(Level.SEVERE, "Exception", e);
                    fail(t.getMessage());
                }
            }
        };

    }

    @Override
    protected void validatePublicVoidNoArgMethods(Class<? extends Annotation> annotation, boolean isStatic, List<Throwable> errors) {

    }

    private TestProbeBuilder overwriteWithUserDefinition(Class testClass, Object instance, TestProbeBuilder probe)
            throws ExamConfigurationException {
        Method[] methods = testClass.getMethods();
        for (Method m : methods) {
            ProbeBuilder conf = m.getAnnotation(ProbeBuilder.class);
            if (conf != null) {
                // consider as option, so prepare that one:
                LOG.fine("User defined probe hook found: " + m.getName());
                TestProbeBuilder probeBuilder;
                try {
                    probeBuilder = (TestProbeBuilder) m.invoke(instance, probe);
                } catch (Exception e) {
                    throw new ExamConfigurationException("Invoking custom probe hook " + m.getName() + " failed", e);
                }
                if (probeBuilder != null) {
                    return probe;
                } else {
                    throw new ExamConfigurationException("Invoking custom probe hook " + m.getName() + " succeeded but returned null");
                }

            }
        }
        LOG.fine("No User defined probe hook found");
        return probe;
    }
}