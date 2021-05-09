<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:param name="outfile"/>
  
  <xsl:template name="statistics">
    <xsl:param name="tp" select="1"/>
    <xsl:param name="fp" select="1"/>
    <xsl:param name="tn" select="1"/>
    <xsl:param name="fn" select="1"/>
    
    <xsl:variable name="precision" select="$tp div ($tp+$fp)"/>
    <xsl:variable name="recall" select="$tp div ($tp+$fn)"/>
    <xsl:variable name="f1" select="2* ($precision * $recall) div ($precision + $recall)"/>
    <tr>
      <td><a>
        <xsl:attribute name="href">
          <xsl:value-of select="$outfile"/>.html
        </xsl:attribute>
        <xsl:value-of select="$outfile"/> 
        </a>
      </td>
        <td> <xsl:value-of select="round($precision * 100) div 100"/></td>
        <td> <xsl:value-of select="round($recall * 100) div 100"/></td>
        <td> <xsl:value-of select="round($f1 * 100) div 100"/></td>
    </tr>
  </xsl:template>
  
  
    
    
    <xsl:template match="/">
      
          <xsl:call-template name="statistics">
          
            <xsl:with-param name="tp" select="string(doc/queries/query[@label='True Positives']/answers/@count)" />
            <xsl:with-param name="tn" select="string(doc/queries/query[@label='True Negatives']/answers/@count)" />
            <xsl:with-param name="fp" select="string(doc/queries/query[@label='False Positives']/answers/@count)" />
            <xsl:with-param name="fn" select="string(doc/queries/query[@label='False Negatives']/answers/@count)" />
          </xsl:call-template>
          
    </xsl:template>
  </xsl:stylesheet>
