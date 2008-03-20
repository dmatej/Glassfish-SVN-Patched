/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2008 Sun Microsystems, Inc. All rights reserved.
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
 */
package com.sun.enterprise.v3.admin;

import com.sun.enterprise.module.impl.Utils;
import com.sun.enterprise.util.LocalStringManagerImpl;
import com.sun.logging.LogDomains;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.glassfish.api.ActionReport;
import org.glassfish.api.Async;
import org.glassfish.api.I18n;
import org.glassfish.api.Param;
import org.glassfish.api.admin.AdminCommand;
import org.glassfish.api.admin.AdminCommandContext;
import org.jvnet.hk2.annotations.Inject;
import org.jvnet.hk2.annotations.Service;
import org.jvnet.hk2.component.ComponentException;
import org.jvnet.hk2.component.Habitat;
import org.jvnet.hk2.component.InjectionManager;
import org.jvnet.hk2.component.UnsatisfiedDepedencyException;

/**
 * Encapsulates the logic needed to execute a server-side command (for example,  
 * a descendant of AdminCommand) including injection of argument values into the 
 * command.  
 * 
 * @author dochez
 * @author tjquinn
 */
@Service
public class CommandRunner {
    
    public final static LocalStringManagerImpl adminStrings = new LocalStringManagerImpl(CommandRunner.class);
    public final static Logger logger = LogDomains.getLogger(LogDomains.ADMIN_LOGGER);

    @Inject
    Habitat habitat;

    /**
     * Executes a command by name.
     * <p>
     * The commandName parameter value should correspond to the name of a 
     * command that is a service with that name.
     * @param commandName the command to execute
     * @param parameters name/value pairs to be passed to the command
     * @param report will hold the result of the command's execution
     */
    public void doCommand(final String commandName, final Properties parameters, final ActionReport report) {

        final AdminCommand handler = getCommand(commandName, report, logger);
        if (handler==null) {
            return;
        }
        doCommand(commandName, handler, parameters, report);
    }

    /**
     * Executes the provided command object.
     * @param commandName name of the command (used for logging and reporting)
     * @param handler the command service to execute
     * @param parameters name/value pairs to be passed to the command
     * @param report will hold the result of the command's execution
     */
    public void doCommand(
            final String commandName, 
            final AdminCommand command, 
            final Properties parameters, 
            final ActionReport report) {
        
        if (parameters.size()==1 && parameters.get("help")!=null) {
            usage(commandName, command, report);
            return;
        }
        report.setActionDescription(commandName + " AdminCommand");

        final AdminCommandContext context = new AdminCommandContext(
                LogDomains.getLogger(LogDomains.ADMIN_LOGGER),
                report, parameters);                                                 

        // initialize the injector.
        InjectionManager injectionMgr =  new InjectionManager<Param>() {

            @Override
            protected boolean isOptional(Param annotation) {
                return annotation.optional();
            }

            protected Object getValue(Object component, AnnotatedElement target, Class type) throws ComponentException {
                // look for the name in the list of parameters passed.
                Param param = target.getAnnotation(Param.class);
                if (param.primary()) {
                    // this is the primary parameter for the command
                    String value = parameters.getProperty("DEFAULT");
                    if (value!=null) {
                        // let's also copy this value to the command with a real name.
                        parameters.setProperty(getParamName(param, target), value);
                        return value;
                    }
                }
                String paramValueStr = getParamValueString(parameters, param,
                                                           target);

                if (paramValueStr != null) {
                    return convertStringToObject(type, paramValueStr);
                }
                //return default value
                return getParamField(component, target);
            }
        };

        LocalStringManagerImpl localStrings = new LocalStringManagerImpl(command.getClass());

        // Let's get the command i18n key
        I18n i18n = command.getClass().getAnnotation(I18n.class);
        String i18n_key = "";
        if (i18n!=null) {
            i18n_key = i18n.value();
        }

        // inject
        try {
            injectionMgr.inject(command, Param.class);
        } catch (UnsatisfiedDepedencyException e) {
            Param param = e.getUnsatisfiedElement().getAnnotation(Param.class);
            String paramName = getParamName(param, e.getUnsatisfiedElement());
            String paramDesc = getParamDescription(localStrings, i18n_key, paramName, e.getUnsatisfiedElement());

            String errorMsg;
            if (paramDesc!=null) {
                errorMsg = adminStrings.getLocalString("admin.param.missing",
                        "{0} command requires the {1} parameter : {2}", commandName, paramName, paramDesc);
            } else {
                errorMsg = adminStrings.getLocalString("admin.param.missing.nodesc",
                        "{0} command requires the {1} parameter", commandName, paramName);
            }
            logger.severe(errorMsg);
            report.setActionExitCode(ActionReport.ExitCode.FAILURE);
            report.setMessage(errorMsg);
            report.setFailureCause(e);
            return;
        } catch (ComponentException e) {
            logger.severe(e.getMessage());
            report.setActionExitCode(ActionReport.ExitCode.FAILURE);
            report.setMessage(e.getMessage());
            report.setFailureCause(e);
        }

        // the command may be an asynchronous command, so we need to check
        // for the @Async annotation.
        Async async = command.getClass().getAnnotation(Async.class);
        if (async==null) {
            try {
                command.execute(context);
            } catch(Throwable e) {
                logger.log(Level.SEVERE,
                        adminStrings.getLocalString("adapter.exception","Exception in command execution : ", e), e);
            }
        } else {
            Thread t = new Thread() {
                public void run() {
                    try {
                        command.execute(context);
                    } catch (RuntimeException e) {
                        logger.log(Level.SEVERE,e.getMessage(), e);
                    }
                }
            };
            t.setPriority(async.priority());
            t.start();
            report.setActionExitCode(ActionReport.ExitCode.SUCCESS);
            report.setMessage(
                    adminStrings.getLocalString("adapter.command.launch", "{0} launch successful", commandName));
        }
    }

    protected String getParamDescription(LocalStringManagerImpl localStrings, String i18nKey, String paramName, AnnotatedElement annotated) {

        I18n i18n = annotated.getAnnotation(I18n.class);
        String paramDesc;
        if (i18n==null) {
            paramDesc = localStrings.getLocalString(i18nKey+"."+paramName, null);
        } else {
            paramDesc = localStrings.getLocalString(i18n.value(), null);
        }
        if (paramDesc==null) {
            paramDesc = adminStrings.getLocalString("adapter.nodesc", "no description provided");
        }
        return paramDesc;        
    }

        /**
         * get the Param name.  First it checks if the annotated Param
         * includes a the name, if not then get the name from the field.
         *
         * @param - Param class annotation
         * @annotated - annotated element
         * @returns the name of the param
         */
    String getParamName(Param param, AnnotatedElement annotated) {
        if (param.name().equals("")) {
            if (annotated instanceof Field) {
                return ((Field) annotated).getName();
            }
            if (annotated instanceof Method) {
                return ((Method) annotated).getName().substring(3).toLowerCase();
            }
        } else {
            return param.name();
        }
        return "";
    }


        /**
         * get the param value.  checks if the param (option) value
         * is defined on the command line (URL passed by the client)
         * by calling getPropertiesValue method.  If not, then check
         * for the shortName.  If param value is not given by the
         * shortName (short option) then if the default valu is
         * defined.
         * 
         * @param parameters - parameters from the command line.
         * @param param - from the annotated Param
         * @param target - annotated element
         *
         * @returns param value
         */
    String getParamValueString(final Properties parameters,
                               final Param param,
                               final AnnotatedElement target) {
        String paramValueStr = getPropertiesValue(parameters,
                                                  getParamName(param, target),
                                                  true);
        if (paramValueStr == null) {
                //check for shortName
            paramValueStr = parameters.getProperty(param.shortName());
        }
            //if paramValueStr is still null, then check to
            //see if the defaultValue is defined
        if (paramValueStr == null) {
            final String defaultValue = param.defaultValue();
            paramValueStr = (defaultValue.equals(""))?null:defaultValue;
        }
        return paramValueStr;
    }


        /**
         * get the value of the field.  This value is defined in the
         * annotated Param declaration.  For example:
         * <code>
         * @Param(optional=true)
         * String name="server"
         * </code>
         * The Field, name's value, "server" is returned.
         *
         * @param component - command class object
         * @param annotated - annotated element
         *
         * @returns the annotated Field value
         */
    Object getParamField(final Object component,
                         final AnnotatedElement annotated) {
        try {
            if (annotated instanceof Field) {
                Field field = (Field)annotated;
                field.setAccessible(true);
                return ((Field) annotated).get(component);
            }
        }
        catch (Exception e) {
                //unable to get the field value, may not be defined
                //return null instead.
            return null;
        }
        return null;
    }

        /**
         * convert the String parameter to the specified type.
         * For example if type is Properties and the String
         * value is: name1=value1:name2=value2:...
         * then this api will convert the String to a Properties
         * class with the values {name1=name2, name2=value2, ...}
         *
         * @param type - the type of class to convert
         * @param paramValStr - the String value to convert
         *
         * @return Object
         */
    Object convertStringToObject(Class type, String paramValStr)  {
        Object paramValue = paramValStr;
        if (type.isAssignableFrom(String.class)) {
           paramValue = paramValStr;
        } else if (type.isAssignableFrom(Properties.class)) {
           paramValue = convertStringToProperties(paramValStr);
        } else if (type.isAssignableFrom(List.class)) {
           paramValue = convertStringToList(paramValStr);
        } else if (type.isAssignableFrom((new String[]{}).getClass())) {
           paramValue = convertStringToStringArray(paramValStr);
        }
       return paramValue;
    }

    
        /**
         *  Searches for the property with the specified key in this property list.
         *  The method returns null if the property is not found.
         *  @see java.util.Properties#getProperty(java.lang.String)
         *
         *  @param props - the property to search in
         *  @param key - the property key
         *  @param ignoreCase - true to search the key ignoring case
         *                      false otherwise
         *  @return the value in this property list with the specified key value.
         */
    String getPropertiesValue(final Properties props, final String key,
                              final boolean ignoreCase) {
        if (ignoreCase) {
            for (Object propObj : props.keySet()) {
                final String propName = (String)propObj;
                if (propName.equalsIgnoreCase(key)) {
                    return props.getProperty(propName);
                }
            }
        }
        return props.getProperty(key);
    }

    
    /**
     * Return Command handlers from the lookup or if not found in the lookup,
     * look at META-INF/services implementations and add them to the lookup
     * @param commandName the request handler's command name
     * @param report the reporting facility
     * @return the admin command handler if found
     *
     */
    private AdminCommand getCommand(String commandName, ActionReport report, Logger logger) {

        AdminCommand command = null;
        try {
            command = habitat.getComponent(AdminCommand.class, commandName);
        } catch(ComponentException e) {
           e.printStackTrace();
        }
        if (command==null) {
            String msg;
            
            if(!ok(commandName))
                msg = adminStrings.getLocalString("adapter.command.nocommand", "No command was specified.");
            else
                msg = adminStrings.getLocalString("adapter.command.notfound", "Command {0} not found", commandName);
            
            report.setMessage(msg);
            Utils.getDefaultLogger().info(msg);
        }
        return command;
    }

    public void usage(String commandName, AdminCommand command, ActionReport report) {
        
        report.setActionDescription(commandName + " help");
        LocalStringManagerImpl localStrings = new LocalStringManagerImpl(command.getClass());

        // Let's get the command i18n key
        I18n i18n = command.getClass().getAnnotation(I18n.class);
        String i18nKey = "";
        if (i18n!=null) {
            i18nKey = i18n.value();
        }
        report.setMessage(localStrings.getLocalString(i18nKey, null));

        for (Field f : command.getClass().getDeclaredFields()) {
            addParamUsage(report, localStrings, i18nKey, f);
        }
        for (Method m : command.getClass().getDeclaredMethods()) {
            addParamUsage(report, localStrings, i18nKey, m);
        }
        report.setActionExitCode(ActionReport.ExitCode.SUCCESS);
    }

    private void addParamUsage(ActionReport report, LocalStringManagerImpl localStrings, String i18nKey, AnnotatedElement annotated) {

        Param param = annotated.getAnnotation(Param.class);
        if (param!=null) {
            // this is a param.
            String paramName = getParamName(param, annotated);
            report.getTopMessagePart().addProperty(paramName, getParamDescription(localStrings, i18nKey, paramName, annotated));
        }
    }
    
    private boolean ok(String s) {
        return s != null && s.length() > 0;
    }

        /**
         * convert a String with the following format to Properties:
         * name1=value1:name2=value2:name3=value3:...
         * The Properties object contains elements:
         * {name1=value1, name2=value2, name3=value3, ...}
         *
         * @param listString - the String to convert
         * @return Properties containing the elements in String
         */
    Properties convertStringToProperties(String propsString) {
        final Properties properties = new Properties();
        if (propsString != null) {
            ParamTokenizer stoken = new ParamTokenizer(propsString, ":");
            while (stoken.hasMoreTokens()) {
                String token = stoken.nextToken();
                if (token.indexOf("=")==-1)
                    continue;
                final ParamTokenizer nameTok = new ParamTokenizer(token, "=");
                if (nameTok.countTokens() == 2) {
                    properties.setProperty(nameTok.nextTokenWithoutEscapeAndQuoteChars(),
                                       nameTok.nextTokenWithoutEscapeAndQuoteChars());
                } else {
                    throw new IllegalArgumentException(adminStrings.getLocalString("InvalidPropertySyntax", "Invalid property syntax."));
                }
            }
        }
        return properties;
    }

        /**
         * convert a String with the following format to List<String>:
         * string1:string2:string3:...
         * The List object contains elements: string1, string2, string3, ...
         *
         * @param listString - the String to convert
         * @return List containing the elements in String
         */
    List<String> convertStringToList(String listString) {
        List<String> list = new java.util.ArrayList();
        if (listString != null) {
            final ParamTokenizer ptoken = new ParamTokenizer(listString, ":");
            while (ptoken.hasMoreTokens()) {
                String token = ptoken.nextTokenWithoutEscapeAndQuoteChars();
                list.add(token);
            }
        }
        return list;
    }

        /**
         * convert a String with the following format to String Array:
         * string1,string2,string3,...
         * The String Array contains: string1, string2, string3, ...
         *
         * @param arrayString - the String to convert
         * @returns String[] containing the elements in String
         */
    String[] convertStringToStringArray(String arrayString) {
        final ParamTokenizer paramTok = new ParamTokenizer(arrayString,",");
        String[] strArray = new String[paramTok.countTokens()];
        int ii=0;
        while (paramTok.hasMoreTokens()) 
        {
            strArray[ii++] = paramTok.nextTokenWithoutEscapeAndQuoteChars();
        }
        return strArray;
    }
    
}

