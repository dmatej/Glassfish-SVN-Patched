<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">
<html>
<body>
<h2>GLassfish Time Statistics</h2>
<table border="1">
<tr bgcolor="#9acd32">
<th>SVN revision</th>
<th>Time</th>
</tr>
<xsl:for-each select="root/stat">
<tr>
<td><xsl:value-of select="build_id"/></td>
<td><xsl:value-of select="time"/></td>
</tr>
</xsl:for-each>
</table>
</body>
</html>
</xsl:template>
</xsl:stylesheet>
