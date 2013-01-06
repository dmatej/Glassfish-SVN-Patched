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


package org.glassfish.fighterfish.test.app5;

import javax.el.ValueExpression;
import javax.faces.FactoryFinder;
import javax.faces.application.Application;
import javax.faces.application.ApplicationFactory;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * @author sanjeeb.sahoo@oracle.com
 *
 */
public class MessageFactory {
    public static FacesMessage getMessage(String messageId, Object[] params)
    {
      Locale locale = null;
      FacesContext context = FacesContext.getCurrentInstance();

      if ((context != null) && (context.getViewRoot() != null)) {
        locale = context.getViewRoot().getLocale();
        if (locale == null)
          locale = Locale.getDefault();
      }
      else {
        locale = Locale.getDefault();
      }

      return getMessage(locale, messageId, params);
    }

    public static FacesMessage getMessage(Locale locale, String messageId, Object[] params)
    {
      FacesMessage result = null;

      String summary = null;
      String detail = null;
      String bundleName = null;
      ResourceBundle bundle = null;

      if ((null != (bundleName = getApplication().getMessageBundle())) && 
        (null != (bundle = ResourceBundle.getBundle(bundleName, locale, getCurrentLoader(bundleName)))))
      {
        try
        {
          summary = bundle.getString(messageId);
          detail = bundle.getString(messageId + "_detail");
        }
        catch (MissingResourceException e)
        {
        }

      }

      if (null == summary)
      {
        bundle = ResourceBundle.getBundle("javax.faces.Messages", locale, getCurrentLoader(bundleName));

        if (null == bundle) {
          throw new NullPointerException();
        }
        try
        {
          summary = bundle.getString(messageId);
          detail = bundle.getString(messageId + "_detail");
        }
        catch (MissingResourceException e)
        {
        }
      }

      if (null == summary) {
        return null;
      }

      if ((null == summary) || (null == bundle)) {
        throw new NullPointerException(" summary " + summary + " bundle " + bundle);
      }

      return new MessageFactory.BindingFacesMessage(locale, summary, detail, params);
    }

    public static FacesMessage getMessage(FacesContext context, String messageId)
    {
      return getMessage(context, messageId, null);
    }

    public static FacesMessage getMessage(FacesContext context, String messageId, Object[] params)
    {
      if ((context == null) || (messageId == null)) {
        throw new NullPointerException(" context " + context + " messageId " + messageId);
      }

      Locale locale = null;

      if ((context != null) && (context.getViewRoot() != null))
        locale = context.getViewRoot().getLocale();
      else {
        locale = Locale.getDefault();
      }
      if (null == locale) {
        throw new NullPointerException(" locale " + locale);
      }
      FacesMessage message = getMessage(locale, messageId, params);
      if (message != null) {
        return message;
      }
      locale = Locale.getDefault();
      return getMessage(locale, messageId, params);
    }

    public static FacesMessage getMessage(FacesContext context, String messageId, Object param0)
    {
      return getMessage(context, messageId, new Object[] { param0 });
    }

    public static FacesMessage getMessage(FacesContext context, String messageId, Object param0, Object param1)
    {
      return getMessage(context, messageId, new Object[] { param0, param1 });
    }

    public static FacesMessage getMessage(FacesContext context, String messageId, Object param0, Object param1, Object param2)
    {
      return getMessage(context, messageId, new Object[] { param0, param1, param2 });
    }

    public static FacesMessage getMessage(FacesContext context, String messageId, Object param0, Object param1, Object param2, Object param3)
    {
      return getMessage(context, messageId, new Object[] { param0, param1, param2, param3 });
    }

    public static Object getLabel(FacesContext context, UIComponent component)
    {
      Object o = component.getAttributes().get("label");
      if (o == null) {
        o = component.getValueExpression("label");
      }

      if (o == null) {
        o = component.getClientId(context);
      }
      return o;
    }

    protected static Application getApplication() {
      FacesContext context = FacesContext.getCurrentInstance();
      if (context != null) {
        return FacesContext.getCurrentInstance().getApplication();
      }
      ApplicationFactory afactory = (ApplicationFactory)FactoryFinder.getFactory("javax.faces.application.ApplicationFactory");

      return afactory.getApplication();
    }

    protected static ClassLoader getCurrentLoader(Object fallbackClass) {
      ClassLoader loader = Thread.currentThread().getContextClassLoader();

      if (loader == null) {
        loader = fallbackClass.getClass().getClassLoader();
      }
      return loader;
    }
    
    static class BindingFacesMessage extends FacesMessage
    {
      private Locale locale;
      private Object[] parameters;
      private Object[] resolvedParameters;

      BindingFacesMessage(Locale locale, String messageFormat, String detailMessageFormat, Object[] parameters)
      {
        super(messageFormat, detailMessageFormat);
        this.locale = locale;
        this.parameters = parameters;
        if (parameters != null)
          this.resolvedParameters = new Object[parameters.length];
      }

      public String getSummary()
      {
        String pattern = super.getSummary();
        resolveBindings();
        return getFormattedString(pattern, this.resolvedParameters);
      }

      public String getDetail() {
        String pattern = super.getDetail();
        resolveBindings();
        return getFormattedString(pattern, this.resolvedParameters);
      }

      private void resolveBindings() {
        FacesContext context = null;
        if (this.parameters != null)
          for (int i = 0; i < this.parameters.length; i++) {
            Object o = this.parameters[i];
            if ((o instanceof ValueExpression)) {
              if (context == null) {
                context = FacesContext.getCurrentInstance();
              }
              o = ((ValueExpression)o).getValue(context.getELContext());
            }

            if (o == null) {
              o = "";
            }
            this.resolvedParameters[i] = o;
          }
      }

      private String getFormattedString(String msgtext, Object[] params)
      {
        String localizedStr = null;

        if ((params == null) || (msgtext == null)) {
          return msgtext;
        }
        StringBuffer b = new StringBuffer(100);
        MessageFormat mf = new MessageFormat(msgtext);
        if (this.locale != null) {
          mf.setLocale(this.locale);
          b.append(mf.format(params));
          localizedStr = b.toString();
        }
        return localizedStr;
      }
    }
}
