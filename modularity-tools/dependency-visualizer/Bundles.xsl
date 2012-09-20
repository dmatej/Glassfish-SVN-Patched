<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <html>
  <body>
    <h2>Bundle details</h2>
    <xsl:for-each select="Bundles/Bundle">
   <b> <a name="{@name}"><xsl:value-of select="@name"/></a></b>
   <table border="1">  
    <tr bgcolor="#9acd32">
      <th align="left">Bundle Name</th>
      <th align="left">Exports-Used</th>
      <th align="left">Exports-Unused</th>
      <th align="left">Imports</th>
    </tr>
   <tr>
      <td><xsl:value-of select="@name"/></td>
      <td><xsl:value-of select="Exports/Used"/></td>
      <td><xsl:value-of select="Exports/Unused"/></td>
      <td><xsl:value-of select="Imports"/></td>
    </tr>
  </table>
    </xsl:for-each>
  </body>
  </html>
</xsl:template>

</xsl:stylesheet>

