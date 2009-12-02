/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Written as part of Dynamic JPA Example
 ******************************************************************************/
package org.eclipse.persistence.extension.dynamic;

import static org.eclipse.persistence.internal.libraries.asm.Constants.*;

import java.io.*;
import java.lang.reflect.*;
import java.net.URL;
import java.util.*;

import org.eclipse.persistence.internal.libraries.asm.*;
import org.eclipse.persistence.internal.libraries.asm.Type;
import org.eclipse.persistence.sessions.Session;

/**
 * This custom class loader is used to generate dynamic entity types for
 * persistent types that are not found on its wrapper class-loader. This
 * functionality enables dynamic persistence without having concrete classes.
 * <p>
 * One of the goals of this style of implementation is that it is minimally
 * intrusive into EclipseLink and allows an application to replace a dynamic entity
 * with a static one simply by having the class on the parent loader's class
 * path.
 * 
 * @author Doug Clarke
 * @since EclipseLink 1.0
 */
public class DynamicClassLoader extends ClassLoader {
	private ClassLoader delegateLoader;
	private Class parentClass;

	private Map<String, Class> dynamicEntityClasses = new HashMap<String, Class>();

	public DynamicClassLoader(ClassLoader delegate, Class parentClass) {
		this.parentClass = parentClass;
		if (delegate == null) {
			this.delegateLoader = Thread.currentThread()
					.getContextClassLoader();
		} else {
			this.delegateLoader = delegate;
		}
	}

	protected ClassLoader getDelegateLoader() {
		return this.delegateLoader;
	}

	public Class getParentsClass() {
		return this.parentClass;
	}

	@Override
	public URL getResource(String name) {
		return getDelegateLoader().getResource(name);
	}

	@Override
	public InputStream getResourceAsStream(String name) {
		return getDelegateLoader().getResourceAsStream(name);
	}

	@Override
	public Enumeration<URL> getResources(String name) throws IOException {
		return getDelegateLoader().getResources(name);
	}

	public Class<?> loadClass(String name) throws ClassNotFoundException {
		Class javaClass = null;

		try {
			javaClass = getDelegateLoader().loadClass(name);
		} catch (ClassNotFoundException cnfe) {
			javaClass = createDynamicClass(name);
			if (javaClass == null) {
				throw cnfe;
			}
		}
		return javaClass;
	}

	private Map<String, Class> getDynamicEntityClasses() {
		return this.dynamicEntityClasses;
	}

	public Class getDynamicEntityClass(String className) {
		return getDynamicEntityClasses().get(className);
	}

	/**
	 * Create a dynamic subclass if one does not already exist and register the
	 * created class for subsequent use.
	 */
	public Class createDynamicClass(String className) {
		Class javaClass = getDynamicEntityClass(className);

		if (javaClass != null) {
			return javaClass;
		}

		if (className == null) {
			return null;
		}
		byte[] bytes = buildClassBytes(getParentsClass(), className);
		javaClass = defineClass(className, bytes, 0, bytes.length);

		getDynamicEntityClasses().put(className, javaClass);

		return javaClass;
	}

	private static final String INIT = "<init>";
	private static final String WRITE_REPLACE = "writeReplace";

	public byte[] buildClassBytes(Class parentClass, String className) {
		if (parentClass == null || parentClass.isPrimitive()
				|| parentClass.isArray() || parentClass.isEnum()
				|| parentClass.isInterface()) {
			throw new IllegalArgumentException(
					"DynamicEntityClassWriter can not create "
							+ "subclass for class: " + parentClass);
		}

		if (Modifier.isFinal(parentClass.getModifiers())) {
			throw new IllegalArgumentException(
					"DynamicEntityClassWriter can not create "
							+ "subclass for final class: " + parentClass);
		}

		ClassWriter cw = new ClassWriter(true);
		cw.visit(V1_2, ACC_PUBLIC + ACC_SUPER, className.replace('.', '/'),
				Type.getType(parentClass).getInternalName(), null, null);
		addConstructors(cw, parentClass);
		addWriteReplace(cw, parentClass);
		cw.visitEnd();
		return cw.toByteArray();
	}

	private void addConstructors(ClassWriter cw, Class parentClass) {
		Constructor[] constructors = parentClass.getDeclaredConstructors();

		for (int index = 0; index < constructors.length; index++) {
			if (Modifier.isPublic(constructors[index].getModifiers())
					|| Modifier.isProtected(constructors[index].getModifiers())) {
				addConstructor(cw, constructors[index]);
			}
		}
	}

	private void addConstructor(ClassWriter cw, Constructor constructor) {
		Type[] types = new Type[constructor.getParameterTypes().length];

		for (int index = 0; index < constructor.getParameterTypes().length; index++) {
			types[index] = Type.getType(constructor.getParameterTypes()[index]);
		}

		String consDesc = Type.getMethodDescriptor(Type.VOID_TYPE, types);
		CodeVisitor mv = cw.visitMethod(ACC_PUBLIC, INIT, consDesc, null, null);
		mv.visitVarInsn(ALOAD, 0);

		for (int param = 1; param <= constructor.getParameterTypes().length; param++) {
			mv.visitVarInsn(ALOAD, param);
		}

		mv.visitMethodInsn(INVOKESPECIAL, Type.getType(
				constructor.getDeclaringClass()).getInternalName(), INIT,
				consDesc);
		mv.visitInsn(RETURN);
		mv.visitMaxs(0, 0);
	}

	private void addWriteReplace(ClassWriter cw, Class parentClass) {
		boolean parentHasWriteReplace = false;

		try {
			parentClass.getDeclaredMethod(WRITE_REPLACE, new Class[0]);
			parentHasWriteReplace = true;
		} catch (NoSuchMethodException e) {
			parentHasWriteReplace = false;
		}

		if (Serializable.class.isAssignableFrom(parentClass)
				&& parentHasWriteReplace) {
			Method method;
			try {
				method = parentClass.getDeclaredMethod(WRITE_REPLACE,
						new Class[0]);
			} catch (NoSuchMethodException e) {
				return;
			}
			CodeVisitor mv = cw.visitMethod(4, method.getName(), Type
					.getMethodDescriptor(method), new String[] { Type.getType(
					ObjectStreamException.class).getInternalName() }, null);
			mv.visitVarInsn(25, 0);
			mv.visitMethodInsn(183, Type.getInternalName(parentClass), method
					.getName(), Type.getMethodDescriptor(method));
			mv.visitInsn(176);
			mv.visitMaxs(0, 0);
		}
	}

	/**
	 * Retrieve the dynamic class loader out of the session.
	 * 
	 * @throws IllegalStateException
	 *             When the sessions' platform does not hold a dynamic loader.
	 */
	public static DynamicClassLoader getLoader(Session session) {
		ClassLoader platformLoader = session.getPlatform()
				.getConversionManager().getLoader();

		if (platformLoader.getClass() != DynamicClassLoader.class) {
			throw new IllegalArgumentException(
					"DynamicClassLoader.getLoader::Session's platform does not use DynamiClassLoader: "
							+ session);
		}
		return (DynamicClassLoader) platformLoader;
	}
}
