<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
	xmlns:rng="http://relaxng.org/ns/structure/1.0">
	
	<xsl:output encoding="UTF-8" />
	<xsl:strip-space elements="rng:*"/>
	<xsl:preserve-space elements="rng:value rng:param"/>
	
	<xsl:template match="rng:*">
		<xsl:copy>
			<xsl:for-each select="@*">
				<xsl:if test="namespace-uri(.)=''"><xsl:copy-of select="."/></xsl:if>
			</xsl:for-each>
			<xsl:apply-templates select="*|text()"/>
		</xsl:copy>
	</xsl:template>
	
	<xsl:template match="rng:name/text()"><xsl:copy /></xsl:template>
	<xsl:template match="rng:value/text()"><xsl:copy /></xsl:template>
	<xsl:template match="rng:param/text()"><xsl:copy /></xsl:template>
	<xsl:template match="text()"/>
	
	<xsl:template match="*|@*"/><!-- ignore -->
	
</xsl:stylesheet>
