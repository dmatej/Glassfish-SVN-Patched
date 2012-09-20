<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">
 <html>
 <body>
 <h2 align="center">Module Names and State</h2>
<div>
The below table lists the modules and their states.
</div>
<br/>
	    
 <table border="1">
 <tr bgcolor="#9acd32">
 <th>State</th>
 <th>Modules Names</th>
 </tr>
 <xsl:for-each select="Modules/Module">
 <xsl:if test="state = 'READY'">
 <tr>
 <td>READY</td>
 <td><xsl:value-of select="name"/></td>
 </tr>
</xsl:if> 
 <xsl:if test="state = 'RES'">
 <tr>
 <td>RESOLVED</td>
 <td><xsl:value-of select="name"/></td>
 </tr>
</xsl:if>
 <xsl:if test="state = 'NEW'">
 <tr>
 <td>NEW</td>
 <td><xsl:value-of select="name"/></td>
 </tr>
</xsl:if>
</xsl:for-each>
 </table>
 </body>
 </html>
 </xsl:template>
 </xsl:stylesheet>
