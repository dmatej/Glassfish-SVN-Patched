<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">
<html>
<body>
<h2 align="center">Module count w.r.t ID</h2>
<br/>      
<div>
The table below gives a snapshot of the number of total number of Modules in the distribution and the total nuber in different states. 
</div><br/>
<table border="1">
<tr bgcolor="#9acd32">
<th>SVN revision</th>
<th>Total</th>
<th>Ready</th>
<th>Resolved</th>
<th>New</th>
</tr>
<xsl:for-each select="root/stat">
<tr>
<td><xsl:value-of select="build_id"/></td>
<td><xsl:value-of select="total"/></td>
<td><xsl:value-of select="ready"/></td>
<td><xsl:value-of select="resolved"/></td>
<td><xsl:value-of select="new"/></td>
</tr>
</xsl:for-each>
</table>
</body>
</html>
</xsl:template>
</xsl:stylesheet>
