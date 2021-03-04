package org.incodame.paragraph.prolog.srv.components

import groovy.sql.Sql
import groovy.text.GStringTemplateEngine
import groovy.transform.Memoized
import io.vertx.core.json.JsonObject
import org.incodame.paragraph.secure.ParagraphCipher

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

class TemplateQuery {

  /**
   * environment -> jdbc properties
   */
  private Map<String, String[]> envInfos

  private Path sessionPropsPath

  public TemplateQuery(JsonObject jdbcInfos, String sessionProps) {
    jdbcInfos.each { String env ->
      JsonObject envObject = jdbcInfos.getJsonObject(env)
      envInfos.put(env, [ envObject.getString("url"),
                          envObject.getString("driver"),
                          envObject.getString("user"),
                          envObject.getString("propsFile")
                        ])
    }

    if (Files.exists(Paths.get(sessionProps))) {
      sessionPropsPath = Paths.get(sessionProps)
    } else {
      throw new IllegalStateException("Unreachable session properties: ${sessionProps}")
    }
  }

  public void withConnection(String environment, Closure c) {
    String[] cnxInfos = envInfos[environment]

    Sql.withInstance(url: cnxInfos[0], driver: cnxInfos[1], user: cnxInfos[2],
                     password: decrypt(environment, cnxInfos[2], cnxInfos[3]),
                     c)

  }

  /* TODO : move to a different tool, use timestamp as userSalt? (avoid asking for it in prolog) */
  public void encrypt(String userSalt, String propsFile, String key, String p) {
    new ParagraphCipher(userSalt, propsFile).setProps(hash, key, p)    /* TODO: hash */
  }

  private String decrypt(String env, String user, String propsFile) {
    String[] sessionElements = getSessionElements(env, user)
    new ParagraphCipher(sessionElements[0], propsFile).hash4Key(sessionElements[1])
  }

  private String[] getSessionElements(String env, String user) {
    String userSalt = sessionProp("userSalt")
    String loginKey = sessionProp("loginKey")
    [ userSalt, loginKey ]
  }

  private String sessionProp(String propName) {
    sessionProperties().get(propName)
  }

  @Memoized
  private Properties sessionProperties() {
    Properties properties = new Properties()
    sessionPropsPath.withInputStream {
      properties.load(it)
    }
    properties
  }

  public Iterator<String[]> queryResults(String environment, String queryName, Map queryArgs) {
    // Log
    File logFile = initLog("TemplateQuery.log")
    File recordFile = initLog("RecordQueries.log")

    // Build query from template
    Tuple2<String, Integer> sqlSelect = buildQuery(queryName, queryArgs, logFile, recordFile)
    int nColumns = sqlSelect.getSecond()

    List<String[]> resList = new LinkedList<>()
    int resCount = 0

    // Run query
    withConnection(environment, { paraSql ->

      paraSql.eachRow(sqlSelect.getFirst(), { row ->
        def md = row.getMetaData()
        def dataCells = (1..nColumns).collect {

        }
      })

    })
  }
}
