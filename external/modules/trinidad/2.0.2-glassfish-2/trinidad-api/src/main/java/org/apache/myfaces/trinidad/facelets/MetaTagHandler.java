/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidad.facelets;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;

import java.beans.Introspector;
import java.beans.PropertyDescriptor;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.logging.Level;

import javax.faces.view.facelets.FaceletContext;
import javax.faces.view.facelets.MetaRule;
import javax.faces.view.facelets.MetaRuleset;
import javax.faces.view.facelets.Metadata;
import javax.faces.view.facelets.MetadataTarget;
import javax.faces.view.facelets.Tag;
import javax.faces.view.facelets.TagAttribute;
import javax.faces.view.facelets.TagAttributeException;
import javax.faces.view.facelets.TagConfig;
import javax.faces.view.facelets.TagException;
import javax.faces.view.facelets.TagHandler;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * A base tag for wiring state to an object instance based on rules populated at
 * the time of creating a MetaRuleset.
 *
 * Implementation copied from Facelets 1.1.14, as it got hidden by JSF 2.0
 * 
 * @author Jacob Hookom
 */

public abstract class MetaTagHandler extends TagHandler {
    private Class lastType = Object.class;

    private Metadata mapper;

    public MetaTagHandler(TagConfig config) {
        super(config);
    }

    /**
     * Extend this method in order to add your own rules.
     *
     * @param type
     * @return
     */
    protected MetaRuleset createMetaRuleset(Class type) {
        assert (type != null);
        return new MetaRulesetImpl(this.tag, type);
    }

    /**
     * Invoking/extending this method will cause the results of the created
     * MetaRuleset to auto-wire state to the passed instance.
     *
     * @param ctx
     * @param instance
     */
    protected void setAttributes(FaceletContext ctx, Object instance) {
        if (instance != null) {
            Class type = instance.getClass();
            if (mapper == null || !this.lastType.equals(type)) {
                this.lastType = type;
                this.mapper = this.createMetaRuleset(type).finish();
            }
            this.mapper.applyMetadata(ctx, instance);
        }
    }


    private static class BeanPropertyTagRule extends MetaRule {

        final static class LiteralPropertyMetadata extends Metadata {

            private final Method method;

            private final TagAttribute attribute;

            private Object[] value;

            public LiteralPropertyMetadata(Method method,
                                           TagAttribute attribute) {
                this.method = method;
                this.attribute = attribute;
            }

            public void applyMetadata(FaceletContext ctx, Object instance) {
                if (value == null) {
                    String str = this.attribute.getValue();
                    value = new Object[] { ctx.getExpressionFactory().coerceToType(str,
                                                       method.getParameterTypes()[0]) };
                }
                try {
                    method.invoke(instance, this.value);
                } catch (InvocationTargetException e) {
                    throw new TagAttributeException(this.attribute,
                                                    e.getCause());
                } catch (Exception e) {
                    throw new TagAttributeException(this.attribute, e);
                }
            }

        }

        final static class DynamicPropertyMetadata extends Metadata {

            private final Method method;

            private final TagAttribute attribute;

            private final Class type;

            public DynamicPropertyMetadata(Method method,
                                           TagAttribute attribute) {
                this.method = method;
                this.type = method.getParameterTypes()[0];
                this.attribute = attribute;
            }

            public void applyMetadata(FaceletContext ctx, Object instance) {
                try {
                    this.method.invoke(instance,
                                       new Object[] { this.attribute.getObject(ctx,
                                                                               this.type) });
                } catch (InvocationTargetException e) {
                    throw new TagAttributeException(this.attribute,
                                                    e.getCause());
                } catch (Exception e) {
                    throw new TagAttributeException(this.attribute, e);
                }
            }
        }

        public final static BeanPropertyTagRule Instance =
            new BeanPropertyTagRule();

        public Metadata applyRule(String name, TagAttribute attribute,
                                  MetadataTarget meta) {
            Method m = meta.getWriteMethod(name);

            // if the property is writable
            if (m != null) {
                if (attribute.isLiteral()) {
                    return new LiteralPropertyMetadata(m, attribute);
                } else {
                    return new DynamicPropertyMetadata(m, attribute);
                }
            }

            return null;
        }

    }

    private static class MetaRulesetImpl extends MetaRuleset {

        private final static WeakHashMap metadata = new WeakHashMap();

        static final private TrinidadLogger log =
            TrinidadLogger.createTrinidadLogger(MetaRulesetImpl.class);

        private final Tag tag;

        private final Class type;

        private final Map attributes;

        private final List mappers;

        private final List rules;

        public MetaRulesetImpl(Tag tag, Class type) {
            this.tag = tag;
            this.type = type;
            this.attributes = new HashMap();
            this.mappers = new ArrayList();
            this.rules = new ArrayList();

            // setup attributes
            TagAttribute[] attrs = this.tag.getAttributes().getAll();
            for (int i = 0; i < attrs.length; i++) {
                attributes.put(attrs[i].getLocalName(), attrs[i]);
            }

            // add default rules
            this.rules.add(BeanPropertyTagRule.Instance);
        }

        public MetaRuleset ignore(String attribute) {
            assert (attribute != null);
            this.attributes.remove(attribute);
            return this;
        }

        public MetaRuleset alias(String attribute, String property) {
            assert (attribute != null);
            assert (property != null);
            TagAttribute attr =
                (TagAttribute)this.attributes.remove(attribute);
            if (attr != null) {
                this.attributes.put(property, attr);
            }
            return this;
        }

        public MetaRuleset add(Metadata mapper) {
            assert (mapper != null);
            if (!this.mappers.contains(mapper)) {
                this.mappers.add(mapper);
            }
            return this;
        }

        public MetaRuleset addRule(MetaRule rule) {
            assert (rule != null);
            this.rules.add(rule);
            return this;
        }

        private final MetadataTarget getMetadataTarget() {
            String key = this.type.getName();
            MetadataTarget meta = (MetadataTarget)metadata.get(key);
            if (meta == null) {
                try {
                    meta = new MetadataTargetImpl(type);
                } catch (IntrospectionException e) {
                    throw new TagException(this.tag, "Error Creating TargetMetadata", e);
                }
                metadata.put(key, meta);
            }
            return meta;
        }

        public Metadata finish() {
            if (!this.attributes.isEmpty()) {
                if (this.rules.isEmpty()) {
                    if (log.isLoggable(Level.SEVERE)) {
                        for (Iterator itr =
                             this.attributes.values().iterator();
                             itr.hasNext(); ) {
                            log.severe(itr.next() +
                                       " Unhandled by MetaTagHandler for type " +
                                       this.type.getName());
                        }
                    }
                } else {
                    MetadataTarget target = this.getMetadataTarget();
                    // now iterate over attributes
                    Map.Entry entry;
                    MetaRule rule;
                    Metadata data;
                    int ruleEnd = this.rules.size() - 1;
                    for (Iterator itr = this.attributes.entrySet().iterator();
                         itr.hasNext(); ) {
                        entry = (Map.Entry)itr.next();
                        data = null;
                        int i = ruleEnd;
                        while (data == null && i >= 0) {
                            rule = (MetaRule)this.rules.get(i);
                            data = rule.applyRule((String)entry.getKey(), (TagAttribute)entry.getValue(), target);
                            i--;
                        }
                        if (data == null) {
                            if (log.isLoggable(Level.SEVERE)) {
                                log.severe(entry.getValue() +
                                           " Unhandled by MetaTagHandler for type " +
                                           this.type.getName());
                            }
                        } else {
                            this.mappers.add(data);
                        }
                    }
                }
            }

            if (this.mappers.isEmpty()) {
                return NONE;
            } else {
                return new MetadataImpl((Metadata[])this.mappers.toArray(new Metadata[this.mappers.size()]));
            }
        }

        public MetaRuleset ignoreAll() {
            this.attributes.clear();
            return this;
        }

        private final static Metadata NONE = new Metadata() {
            public void applyMetadata(FaceletContext ctx, Object instance) {
                // do nothing
            }
        };

        final static class MetadataImpl extends Metadata {

            private final Metadata[] mappers;
            private final int size;

            public MetadataImpl(Metadata[] mappers) {
                this.mappers = mappers;
                this.size = mappers.length;
            }

            public void applyMetadata(FaceletContext ctx, Object instance) {
                for (int i = 0; i < size; i++) {
                    this.mappers[i].applyMetadata(ctx, instance);
                }
            }

        }
    }

    private static class MetadataTargetImpl extends MetadataTarget {

        private final Map pd;
        private final Class type;


        public MetadataTargetImpl(Class type) throws IntrospectionException {
            this.type = type;
            this.pd = new HashMap();
            BeanInfo info = Introspector.getBeanInfo(type);
            PropertyDescriptor[] pda = info.getPropertyDescriptors();
            for (int i = 0; i < pda.length; i++) {
                this.pd.put(pda[i].getName(), pda[i]);
            }
        }

        public PropertyDescriptor getProperty(String name) {
            return (PropertyDescriptor)this.pd.get(name);
        }

        public boolean isTargetInstanceOf(Class type) {
            return type.isAssignableFrom(this.type);
        }

        public Class getTargetClass() {
            return this.type;
        }

        public Class getPropertyType(String name) {
            PropertyDescriptor pd = this.getProperty(name);
            if (pd != null) {
                return pd.getPropertyType();
            }
            return null;
        }

        public Method getWriteMethod(String name) {
            PropertyDescriptor pd = this.getProperty(name);
            if (pd != null) {
                return pd.getWriteMethod();
            }
            return null;
        }

        public Method getReadMethod(String name) {
            PropertyDescriptor pd = this.getProperty(name);
            if (pd != null) {
                return pd.getReadMethod();
            }
            return null;
        }

    }

}

