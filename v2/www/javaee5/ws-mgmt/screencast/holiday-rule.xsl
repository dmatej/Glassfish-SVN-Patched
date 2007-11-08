<?xml version="1.0" ?>
  <xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
  xmlns:xsd="http://www.w3.org/2001/XMLSchema"
  xmlns:ns1="http://service/">
    <xsl:template match="/">
    <xsl:text> 
	<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:ns1="http://example.com/Hello">
		<soapenv:Body>
		<ns1:sayHelloResponse>
		<return>Happy Holiday !!!!</return>
		</ns1:sayHelloResponse>
		</soapenv:Body>
	</soapenv:Envelope>
    </xsl:text>
    </xsl:template>
  </xsl:stylesheet>
