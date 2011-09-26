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
package org.apache.myfaces.trinidadinternal.el;

import junit.textui.TestRunner;
import org.apache.myfaces.trinidadinternal.el.Tokenizer.Token;
import junit.framework.TestCase;

/**
 * Unit tests for Tokenizer
 *
 */
public class TokenizerTest extends TestCase
{
  public TokenizerTest(String testName)
  {
    super(testName);
  }

  public void testNonExpression()
  {
    String exp = "this isn't an {expression}";
    Tokenizer tok = new Tokenizer(exp);
    _testToken(tok, Tokenizer.TEXT_TYPE, exp);
    assertFalse(tok.hasNext());
  }

  public void testSimpleExpression()
  {
    Tokenizer tok = new Tokenizer("#{1+2.4}");
    _testToken(tok, Tokenizer.EXP_START_TYPE, "#{");
    _testToken(tok, Tokenizer.NUMBER_TYPE, "1");
    _testToken(tok, Tokenizer.OPER_TYPE, "+");
    _testToken(tok, Tokenizer.NUMBER_TYPE, "2.4");
    _testToken(tok, Tokenizer.EXP_END_TYPE, "}");
    assertFalse(tok.hasNext());
  }

  public void testSimpleVarExpression()
  {
    Tokenizer tok = new Tokenizer("#{row+x.y.z}");
    _testToken(tok, Tokenizer.EXP_START_TYPE, "#{");
    _testToken(tok, Tokenizer.VAR_TYPE, "row");
    _testToken(tok, Tokenizer.OPER_TYPE, "+");
    _testToken(tok, Tokenizer.VAR_TYPE, "x.y.z");
    _testToken(tok, Tokenizer.EXP_END_TYPE, "}");
    assertFalse(tok.hasNext());
  }

  public void testWhiteSpaceExpression()
  {
    Tokenizer tok = new Tokenizer("#{ row  + x.y.z   }");
    _testToken(tok, Tokenizer.EXP_START_TYPE, "#{");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, " ");
    _testToken(tok, Tokenizer.VAR_TYPE, "row");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, "  ");
    _testToken(tok, Tokenizer.OPER_TYPE, "+");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, " ");
    _testToken(tok, Tokenizer.VAR_TYPE, "x.y.z");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, "   ");
    _testToken(tok, Tokenizer.EXP_END_TYPE, "}");
    assertFalse(tok.hasNext());
  }

  public void testQuotedExpression()
  {
    Tokenizer tok = new Tokenizer("#{'pre\"post'}#{\"pre'post\"}");
    _testToken(tok, Tokenizer.EXP_START_TYPE, "#{");
    _testToken(tok, Tokenizer.QUOTED_TYPE, "'pre\"post'");
    _testToken(tok, Tokenizer.EXP_END_TYPE, "}");
    _testToken(tok, Tokenizer.EXP_START_TYPE, "#{");
    _testToken(tok, Tokenizer.QUOTED_TYPE, "\"pre'post\"");
    _testToken(tok, Tokenizer.EXP_END_TYPE, "}");
    assertFalse(tok.hasNext());
  }

  public void testQuotedExpression2()
  {
    Tokenizer tok = new Tokenizer("#{'pre\\'post'}#{\"pre\\\"post\"}");
    _testToken(tok, Tokenizer.EXP_START_TYPE, "#{");
    _testToken(tok, Tokenizer.QUOTED_TYPE, "'pre\\'post'");
    _testToken(tok, Tokenizer.EXP_END_TYPE, "}");
    _testToken(tok, Tokenizer.EXP_START_TYPE, "#{");
    _testToken(tok, Tokenizer.QUOTED_TYPE, "\"pre\\\"post\"");
    _testToken(tok, Tokenizer.EXP_END_TYPE, "}");
    assertFalse(tok.hasNext());
  }
  
  public void testComplexExpression()
  {
    Tokenizer tok = 
      new Tokenizer("Found #{row.files} #{row.files > 1 ? 'files' : 'file'}");

    _testToken(tok, Tokenizer.TEXT_TYPE, "Found ");
    _testToken(tok, Tokenizer.EXP_START_TYPE, "#{");
    _testToken(tok, Tokenizer.VAR_TYPE, "row.files");
    _testToken(tok, Tokenizer.EXP_END_TYPE, "}");

    _testToken(tok, Tokenizer.TEXT_TYPE, " ");

    _testToken(tok, Tokenizer.EXP_START_TYPE, "#{");
    _testToken(tok, Tokenizer.VAR_TYPE, "row.files");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, " ");
    _testToken(tok, Tokenizer.OPER_TYPE, ">");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, " ");
    _testToken(tok, Tokenizer.NUMBER_TYPE, "1");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, " ");
    _testToken(tok, Tokenizer.OPER_TYPE, "?");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, " ");
    _testToken(tok, Tokenizer.QUOTED_TYPE, "'files'");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, " ");
    _testToken(tok, Tokenizer.OPER_TYPE, ":");
    _testToken(tok, Tokenizer.WHITE_SPACE_TYPE, " ");
    _testToken(tok, Tokenizer.QUOTED_TYPE, "'file'");
    _testToken(tok, Tokenizer.EXP_END_TYPE, "}");
    assertFalse(tok.hasNext());
  }
  
  public static void main(String[] args)
  {
    TestRunner.run(TokenizerTest.class);
  }
  
  private void _testToken(Tokenizer tokens, int expectedType, String expected)
  {
    Token tok = tokens.next();
    assertEquals("correct token type", expectedType, tok.type);
    assertEquals("correct token text", expected, tok.getText());
  }
}
