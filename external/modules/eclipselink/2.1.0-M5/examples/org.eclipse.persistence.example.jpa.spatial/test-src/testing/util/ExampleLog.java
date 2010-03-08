package testing.util;

import java.text.DateFormat;
import java.util.*;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.indirection.*;
import org.eclipse.persistence.internal.helper.Helper;
import org.eclipse.persistence.mappings.DatabaseMapping;
import org.eclipse.persistence.sessions.Session;

/**
 * <b>Purpose</b>: To demonstrate some common functions through example.
 */
public final class ExampleLog {
    public static void log(String message) {
        System.out.println(message);
    }

    public static void log(Collection results, String typeName) {
        log("\tResults contain " + results.size() + " " + typeName + (results.size() > 1 ? "s" : ""));
        int index = 1;
        for (Iterator i = results.iterator(); i.hasNext();) {
            log("\t\t" + (index++) + "> \t" + i.next());
        }
    }

    public static void log(Map results, String typeName) {
        if (results instanceof IndirectMap && !((IndirectMap) results).isInstantiated()) {
            log("\tResults - uninstantiated indirect container");
        } else {
            log("\tResults contain " + results.size() + " " + typeName + (results.size() > 1 ? "s" : ""));

            for (Iterator i = results.keySet().iterator(); i.hasNext();) {
                Object key = i.next();
                log("\t\t" + key + "> \t" + results.get(key));
            }
        }
    }

    public static void logStartExample(Session session, Object example) {
        log("\n\n\nSTART: " + Helper.getShortClassName(example));
    }

    public static void logEndExample(Object example) {
        log("END: " + Helper.getShortClassName(example));
    }

    /**
     * 
     * @param object
     * @param session
     */
    public static void printObject(Object object, Session session) {
        printObject("", object, session);
    }

    private static void printObject(String prefix, Object object, Session session) {
        ClassDescriptor desc = session.getClassDescriptor(object);
        String className = Helper.getShortClassName(desc.getJavaClass());
        System.out.println(prefix + "Object Type: " + className + (desc.isAggregateDescriptor() ? " - Aggregate" : ""));
        for (Iterator i = desc.getMappings().iterator(); i.hasNext();) {
            DatabaseMapping mapping = (DatabaseMapping) i.next();
            Object value = mapping.getAttributeValueFromObject(object);
            if (mapping.isAggregateObjectMapping()) {
                printObject(prefix + "   ", value, session);
            } else {
                printValue(prefix + "   ", mapping.getAttributeName(), value);
            }
        }
    }

    private static void printValue(String prefix, String name, Object value) {
        if (value instanceof ValueHolderInterface) {
            ValueHolderInterface vhi = (ValueHolderInterface) value;
            if (vhi.isInstantiated()) {
                printValue(prefix, name, vhi.getValue());
            } else {
                System.out.println(prefix + name + " - Uninstantiated");
            }
        } else if (value instanceof IndirectContainer) {
            IndirectContainer container = (IndirectContainer) value;
            if (container.isInstantiated()) {
                printValue(prefix, name, container.getValueHolder().getValue());
            }
            System.out.println(prefix + name + " - " + container.getClass().getSuperclass());
        } else if (value instanceof Collection) {
            System.out.println(prefix + name + " - " + Helper.getShortClassName(value.getClass()));
            for (Iterator i = ((Collection) value).iterator(); i.hasNext();) {
                printValue(prefix + prefix, "> ", i.next());
            }
        } else if (value instanceof Object[]) {
            System.out.println(prefix + name + " - Array");
            for (int index = 0; index < ((Object[]) value).length; index++) {
                printValue(prefix + prefix, "[" + index + "]> ", ((Object[]) value)[index]);
            }
        } else {
            Object attrValue = value;
            if (value instanceof Calendar) {
                attrValue = DateFormat.getInstance().format(((Calendar) value).getTime());
            }
            System.out.println(prefix + name + " = " + attrValue);
        }
    }
}
