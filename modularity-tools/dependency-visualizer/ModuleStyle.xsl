<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
<html>
<body>
<h2>Modules Imports/Exports Statistics</h2>
<table border="1">
<tr bgcolor="#9acd32">
<th>Module Name</th>
<th>Afferent</th>
<th>Efferent</th>
<th>Used</th>
<th>Unused</th>
<th>Total Imports</th>
<th>Classes</th>
<th>Stability</th>
<th>Instability</th>
<th>Layers</th>
</tr>
<xsl:for-each select="ModuleStats/Module">
<tr>
<td><a href="Modules/{name}/both/{name}.jpg"><xsl:value-of select="name"/></a> </td>               
<td><a href="Modules/{name}/imports/{name}.jpg"><xsl:value-of select="import"/></a></td>
<td><a href="Modules/{name}/exports/{name}.jpg"><xsl:value-of select="export"/></a></td>
<td><a href="Bundles.html#{name}"><xsl:value-of select="used-export"/></a></td>
<td><a href="Bundles.html#{name}"><xsl:value-of select="unused-export"/></a></td>
<td><a href="Bundles.html#{name}"><xsl:value-of select="total-import"/></a></td>
<td><a href=""><xsl:value-of select="classes"/></a></td>
<td><a href=""><xsl:value-of select="stability"/></a></td>
<td><a href=""><xsl:value-of select="instability"/></a></td>
<td><a href=""><xsl:value-of select="layer"/></a></td>
</tr>
</xsl:for-each>
</table>
</body>
</html>
</xsl:template>
</xsl:stylesheet>

