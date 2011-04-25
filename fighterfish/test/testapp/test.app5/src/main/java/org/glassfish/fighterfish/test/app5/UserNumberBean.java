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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

import java.util.Random;

/**
 * @author sanjeeb.sahoo@oracle.com
 * 
 */
public class UserNumberBean {
    Integer userNumber = null;
    Integer randomInt = null;
    String response = null;

    protected String[] status = null;

    private int maximum = 0;
    private boolean maximumSet = false;

    private int minimum = 0;
    private boolean minimumSet = false;

    public UserNumberBean() {
        Random randomGR = new Random();
        do
            this.randomInt = new Integer(randomGR.nextInt(10));
        while (this.randomInt.intValue() == 0);
        System.out.println("Duke's number: " + this.randomInt);
    }

    public void setUserNumber(Integer user_number) {
        this.userNumber = user_number;
        System.out.println("Set userNumber " + this.userNumber);
    }

    public Integer getUserNumber() {
        System.out.println("get userNumber " + this.userNumber);
        return this.userNumber;
    }

    public String getResponse() {
        if ((this.userNumber != null)
                && (this.userNumber.compareTo(this.randomInt) == 0))
            return "Yay! You got it!";
        if (this.userNumber == null) {
            return "Sorry, " + this.userNumber
                    + " is incorrect. Try a larger number.";
        }

        int num = this.userNumber.intValue();
        if (num > this.randomInt.intValue()) {
            return "Sorry, " + this.userNumber
                    + " is incorrect. Try a smaller number.";
        }

        return "Sorry, " + this.userNumber
                + " is incorrect. Try a larger number.";
    }

    public String[] getStatus() {
        return this.status;
    }

    public void setStatus(String[] newStatus) {
        this.status = newStatus;
    }

    public int getMaximum() {
        return this.maximum;
    }

    public void setMaximum(int maximum) {
        this.maximum = maximum;
        this.maximumSet = true;
    }

    public int getMinimum() {
        return this.minimum;
    }

    public void setMinimum(int minimum) {
        this.minimum = minimum;
        this.minimumSet = true;
    }

    public void validate(FacesContext context, UIComponent component,
            Object value) throws ValidatorException {
        if ((context == null) || (component == null)) {
            throw new NullPointerException();
        }
        if (value != null)
            try {
                int converted = intValue(value);
                if ((this.maximumSet) && (converted > this.maximum)) {
                    if (this.minimumSet) {
                        throw new ValidatorException(
                                MessageFactory
                                        .getMessage(
                                                context,
                                                "javax.faces.validator.LongRangeValidator.NOT_IN_RANGE",
                                                new Object[] {
                                                        new Integer(
                                                                this.minimum),
                                                        new Integer(
                                                                this.maximum),
                                                        MessageFactory
                                                                .getLabel(
                                                                        context,
                                                                        component) }));
                    }

                    throw new ValidatorException(
                            MessageFactory
                                    .getMessage(
                                            context,
                                            "javax.faces.validator.LongRangeValidator.MAXIMUM",
                                            new Object[] {
                                                    new Integer(this.maximum),
                                                    MessageFactory.getLabel(
                                                            context, component) }));
                }

                if ((this.minimumSet) && (converted < this.minimum)) {
                    if (this.maximumSet) {
                        throw new ValidatorException(
                                MessageFactory
                                        .getMessage(
                                                context,
                                                "javax.faces.validator.LongRangeValidator.NOT_IN_RANGE",
                                                new Object[] {
                                                        new Double(this.minimum),
                                                        new Double(this.maximum),
                                                        MessageFactory
                                                                .getLabel(
                                                                        context,
                                                                        component) }));
                    }

                    throw new ValidatorException(
                            MessageFactory
                                    .getMessage(
                                            context,
                                            "javax.faces.validator.LongRangeValidator.MINIMUM",
                                            new Object[] {
                                                    new Integer(this.minimum),
                                                    MessageFactory.getLabel(
                                                            context, component) }));
                }

            } catch (NumberFormatException e) {
                throw new ValidatorException(MessageFactory.getMessage(context,
                        "javax.faces.validator.LongRangeValidator.TYPE",
                        new Object[] { MessageFactory.getLabel(context,
                                component) }));
            }
    }

    private int intValue(Object attributeValue) throws NumberFormatException {
        if ((attributeValue instanceof Number)) {
            return ((Number) attributeValue).intValue();
        }
        return Integer.parseInt(attributeValue.toString());
    }
}
