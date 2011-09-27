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
package org.apache.myfaces.trinidadinternal.validator;

import java.util.Locale;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;

import org.apache.myfaces.trinidadbuild.test.AbstractBaseTestCase;
import org.jmock.Mock;

import junit.framework.Test;
import junit.framework.TestSuite;


/**
 * Tests for ByteLenghtValidator which renderes the client side scripts.
 * the ByteLengthValidator class.
 */
public class ByteLengthValidatorTest extends AbstractBaseTestCase
{
  public ByteLengthValidatorTest(String name)
  {
    super(name);
  }
  
  @Override
  protected void setUp() throws Exception
  {
    super.setUp();
  }
  
  @Override
  protected void tearDown() throws Exception
  {
    super.tearDown();
  }
  
  public static Test suite()
  {
    return new TestSuite(ByteLengthValidatorTest.class);
  }

  public void testPickedUpClientByteLengthValidator()
  {
    _doTestPickedUpClientByteLengthValidator("ISO-8859-1","SBFormat");
    _doTestPickedUpClientByteLengthValidator("ms_kanji","CjkFormat");
    _doTestPickedUpClientByteLengthValidator("UTF-8","Utf8Format");
  }

  private void _doTestPickedUpClientByteLengthValidator(
    String encoding,
    String expectedConstructorName
    )
  {

    Mock mockComponent = buildMockUIComponent();
    UIComponent component = (UIComponent) mockComponent.proxy();

    UIViewRoot uiRoot = new UIViewRoot();
    uiRoot.setLocale(Locale.US);
    for (int i = 0; i < 4; i++)
      facesContext.setViewRoot(uiRoot);

    ByteLengthValidator blv = new ByteLengthValidator();
    blv.setEncoding(encoding);

    String libKey = (String)(blv.getClientImportNames().toArray()[0]);
    String constructorInfo = blv.getClientValidation(facesContext, component);
    String clientScript = blv.getClientScript(facesContext, component);

    assertEquals(null, clientScript);
    assertEquals(true, libKey.equals(expectedConstructorName + "()"));
    assertEquals(true, constructorInfo.startsWith("new "
                                                  + expectedConstructorName));

  }

  /**
   * @todo Do we have any way to check whether for the given
   * ByteLenght.VALIDATOR_ID if the expected client side ByteLengthValidator
   * is picked.
   */
}
