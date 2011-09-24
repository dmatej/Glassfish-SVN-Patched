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
package org.apache.myfaces.trinidad.util;

import java.io.IOException;
import java.io.StringReader;
import java.io.BufferedReader;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.util.Base64InputStream;
import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

/**
 * Unit tests for Base64InputStream.
 * 
 */

public class Base64InputStreamTest extends FacesTestCase 
{   
	
  /**
   * Creates a new Base64InputStreamTest.
   * 
   * @param testName  the unit test name
   */	
 	public Base64InputStreamTest(String testName)
  	{
  	  super(testName);
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
    return new TestSuite(Base64InputStreamTest.class);
  }
	
  /**
   * Tests decoding of stream that contains no trailing padding characters.
   */
 	public void testNoPaddingChar() throws IOException
 	{
		String	str = "abcdefghijklmnopqrstuvwxBase64 Encoding is a popular way to convert the 8bit and the binary to the 7bit for network trans using Socket, and a security method to handle text or file, often used in Authentical Login and Mail Attachment, also stored in text file or database. Most SMTP server will handle the login UserName and Password in this way. 1";
    	String str_encoded = "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4QmFzZTY0IEVuY29kaW5nIGlzIGEgcG9wdWxhciB3YXkgdG8gY29udmVydCB0aGUgOGJpdCBhbmQgdGhlIGJpbmFyeSB0byB0aGUgN2JpdCBmb3IgbmV0d29yayB0cmFucyB1c2luZyBTb2NrZXQsIGFuZCBhIHNlY3VyaXR5IG1ldGhvZCB0byBoYW5kbGUgdGV4dCBvciBmaWxlLCBvZnRlbiB1c2VkIGluIEF1dGhlbnRpY2FsIExvZ2luIGFuZCBNYWlsIEF0dGFjaG1lbnQsIGFsc28gc3RvcmVkIGluIHRleHQgZmlsZSBvciBkYXRhYmFzZS4gTW9zdCBTTVRQIHNlcnZlciB3aWxsIGhhbmRsZSB0aGUgbG9naW4gVXNlck5hbWUgYW5kIFBhc3N3b3JkIGluIHRoaXMgd2F5LiAx";
  
    	_testRead(str, str_encoded);	
    	
 	}
 	
  /**
   * Tests decoding of stream that contains exactly one trailing padding character.
   */
 	public void testOnePaddingChar() throws IOException
 	{
		String str = "Base64 Encoding is a popular way to convert the 8bit and the binary to the 7bit for network trans using Socket, and a security method to handle text or file, often used in Authentical Login and Mail Attachment, also stored in text file or database. Most SMTP server will handle the login UserName and Password in this way.9";
    	String str_encoded = "QmFzZTY0IEVuY29kaW5nIGlzIGEgcG9wdWxhciB3YXkgdG8gY29udmVydCB0aGUgOGJpdCBhbmQgdGhlIGJpbmFyeSB0byB0aGUgN2JpdCBmb3IgbmV0d29yayB0cmFucyB1c2luZyBTb2NrZXQsIGFuZCBhIHNlY3VyaXR5IG1ldGhvZCB0byBoYW5kbGUgdGV4dCBvciBmaWxlLCBvZnRlbiB1c2VkIGluIEF1dGhlbnRpY2FsIExvZ2luIGFuZCBNYWlsIEF0dGFjaG1lbnQsIGFsc28gc3RvcmVkIGluIHRleHQgZmlsZSBvciBkYXRhYmFzZS4gTW9zdCBTTVRQIHNlcnZlciB3aWxsIGhhbmRsZSB0aGUgbG9naW4gVXNlck5hbWUgYW5kIFBhc3N3b3JkIGluIHRoaXMgd2F5Ljk="; 
   
		_testRead(str, str_encoded); 	
    	
 	}
 	
   /**
   * Tests decoding of stream that contains exactly two trailing padding characters.
   */	 	
  	public void testTwoPaddingChars() throws IOException
 	{
    	String str = "Base64 Encoding is a popular way to convert the 8bit and the binary to the 7bit for network trans using Socket, and a security method to handle text or file, often used in Authentical Login and Mail Attachment, also stored in text file or database. Most SMTP server will handle the login UserName and Password in this way.";
    	String str_encoded = "QmFzZTY0IEVuY29kaW5nIGlzIGEgcG9wdWxhciB3YXkgdG8gY29udmVydCB0aGUgOGJpdCBhbmQgdGhlIGJpbmFyeSB0byB0aGUgN2JpdCBmb3IgbmV0d29yayB0cmFucyB1c2luZyBTb2NrZXQsIGFuZCBhIHNlY3VyaXR5IG1ldGhvZCB0byBoYW5kbGUgdGV4dCBvciBmaWxlLCBvZnRlbiB1c2VkIGluIEF1dGhlbnRpY2FsIExvZ2luIGFuZCBNYWlsIEF0dGFjaG1lbnQsIGFsc28gc3RvcmVkIGluIHRleHQgZmlsZSBvciBkYXRhYmFzZS4gTW9zdCBTTVRQIHNlcnZlciB3aWxsIGhhbmRsZSB0aGUgbG9naW4gVXNlck5hbWUgYW5kIFBhc3N3b3JkIGluIHRoaXMgd2F5Lg==";
    
		_testRead(str, str_encoded); 	
    	
 	}	
 	
 	
	/**
	 *
	 *	Uses Base64InputStream to reads from the encoded string and 
	 *	compares output with the expected decoded string.
	 *
	 *	@param	str			the decoded string
	 *	@param  str_encoded	the base64 encoded string
	 * 
	 **/ 	
 	
	private void _testRead(String str, String str_encoded) throws IOException
	{
   
	    // construct the Base64InputStream from the encoded string
	    StringReader strreader = new StringReader(str_encoded);
	    BufferedReader buffreader = new BufferedReader(strreader); 
	    Base64InputStream b64_in = new Base64InputStream(buffreader);
 
	    // read decoded chars from the Base64InputStream and form decoded string
 	   	String result = "";
 	   	int c;
 	   	while ( (c = b64_in.read()) != -1 ) 
 	   	{
  	   	 result = result + (char)c;
  	 	}
  	 	
		// compare resulting decoded string with the expected decoded string 
		assertEquals(result, str);	
	}
  
  
  
} // end Base64InputStreamTest class
