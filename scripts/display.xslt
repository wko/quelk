<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <xsl:template name="statistics">
    <xsl:param name="tp" select="1"/>
    <xsl:param name="fp" select="1"/>
    <xsl:param name="tn" select="1"/>
    <xsl:param name="fn" select="1"/>
    
    <xsl:variable name="precision" select="$tp div ($tp+$fp)"/>
    <xsl:variable name="recall" select="$tp div ($tp+$fn)"/>
    <xsl:variable name="f1" select="2* ($precision * $recall) div ($precision + $recall)"/>
    <ul>
      <li> Precision: <xsl:value-of select="round($precision * 100) div 100"/></li>
      <li> Recall: <xsl:value-of select="round($recall * 100) div 100"/></li>
      <li> F1: <xsl:value-of select="round($f1 * 100) div 100"/></li>
    </ul>
  </xsl:template>
  
  <xsl:template name="answers">
    <xsl:param name="answers" select="1"/>
    <li><span class="caret">Answers - <span class="badge"><xsl:value-of select="count(answers/answer)"/></span></span>
      <ul class="nested answers">
        <xsl:for-each select="answers/answer">
          <li >
            <xsl:for-each select="var">
              <div class="filterDiv link">
                <xsl:attribute name="data">
                <xsl:value-of select="@ind"/>
              </xsl:attribute>
              <a> 
                <xsl:attribute name="onClick">
                filterSelection('<xsl:value-of select="@ind"/>')
              </xsl:attribute>
              
              <xsl:value-of select="@name"/> - <xsl:value-of select="@ind"/>
              </a>
            </div>
            </xsl:for-each>
            
          </li>
          
        </xsl:for-each>
      </ul>
    </li>
  </xsl:template>
  
  <xsl:template name="part">
    <xsl:param name="part" select="1"/>
    <li>
      <span class="caret">
      <xsl:choose>
         <xsl:when test="@type = 'NCQ'">
           <xsl:value-of select="@label"/> - <xsl:value-of select="@content"/>
         </xsl:when>
         <xsl:otherwise>
          <xsl:value-of select="@type"/>
         </xsl:otherwise>
       </xsl:choose>
        
 
      <span class="badge"><xsl:value-of select="count(answers/answer)"/></span></span>
      <ul class="nested">
        <xsl:call-template name="answers">
          <xsl:with-param name="answers" select="/answers"/>
        </xsl:call-template>
        <li>Subqueries
          <ul>
        <xsl:for-each select="part">
          <xsl:call-template name="part">
            <xsl:with-param name="part" select="/"/>
          </xsl:call-template>
        </xsl:for-each>
      </ul>
    </li>
      </ul>
    </li>
  </xsl:template>
  
  <xsl:template name="query">
    <xsl:param name="query" select="1"/>
    <li><span class="caret"><xsl:value-of select="@label"/><span class="badge"><xsl:value-of select="count(answers/answer)"/></span></span>
      <ul class="nested">
        <xsl:call-template name="answers">
          <xsl:with-param name="answers" select="/answers"/>
        </xsl:call-template>
        <li>Subqueries
          <ul>
        <xsl:for-each select="part">
          <xsl:call-template name="part">
            <xsl:with-param name="part" select="/"/>
          </xsl:call-template>
        </xsl:for-each>
      </ul>
    </li>
      </ul>
    </li>
    </xsl:template>
    
    
    
    <xsl:template match="/">

      <html>

        <head>
          <link rel="stylesheet" href="style.css" />
        </head>

        <body>
          <h2>Statistics </h2>
          <xsl:call-template name="statistics">
            <xsl:with-param name="tp" select="string(doc/queries/query[@label='True Positives']/answers/@count)" />
            <xsl:with-param name="tn" select="string(doc/queries/query[@label='True Negatives']/answers/@count)" />
            <xsl:with-param name="fp" select="string(doc/queries/query[@label='False Positives']/answers/@count)" />
            <xsl:with-param name="fn" select="string(doc/queries/query[@label='False Negatives']/answers/@count)" />
          </xsl:call-template>
          
          <h2>Queries</h2>
          <button class="btn active" onclick="filterSelection('all')"> Show all</button>
          <ul id="myUL">
            <xsl:for-each select="doc/queries/query">
              <xsl:call-template name="query">
                <xsl:with-param name="query" select="/" />
              </xsl:call-template>
            </xsl:for-each>
          </ul>

          
        <script src="javascript.js"></script>    
        </body>
      </html>
    </xsl:template>
  </xsl:stylesheet>
