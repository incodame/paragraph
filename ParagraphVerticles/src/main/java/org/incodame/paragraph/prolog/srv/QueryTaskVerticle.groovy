package org.incodame.paragraph.prolog.srv

import com.profesorfalken.jpowershell.PowerShell
import com.profesorfalken.jpowershell.PowerShellNotAvailableException
import io.vertx.core.eventbus.Message
import io.vertx.core.json.JsonObject
import org.incodame.paragraph.ParagraphVerticle
import org.incodame.paragraph.prolog.srv.components.TemplateQuery

class QueryTaskVerticle extends ParagraphVerticle {

  @Override
  public void startParagraphVerticle(JsonObject jsonConfig) {

    def eb = vertx.eventBus()

    def consumer = eb.consumer("query.task")

    consumer.handler({ message ->
      executeTask(message, jsonConfig)
    })

  }

  def executeTask(Message<JsonObject> message, JsonObject jsonConfig) {

    if (!message.headers().contains("templateQuery")) {
      failTask(message, "No templateQuery header specified")
      return
    }

    if (!message.headers().contains("templateArgs")) {
      failTask(message, "No templateArgs header specified")
      return
    }

    String environment = message.headers().get("environment")
    String templateQuery = message.headers().get("templateQuery")
    JsonObject templateArgs = message.headers().get("templateArgs")
    Map queryArgs = templateArgs.toSpreadMap() /* TODO */

    if (validateQuery(templateQuery, templateArgs)) {

      try {

        def queryResponse = executeQuery(environment, templateQuery, queryArgs, jsonConfig)

        message.reply(new JsonObject().put("queryResponse", queryResponse))

      } catch (Exception e) {
        failTask(message, "Task exception: ${e.getMessage()}")
        return
      }

    } else {
      failTask(message, "Task not accepted")
      return
    }

  }

  def executeQuery(String environment, String template, Map queryArgs, JsonObject jsonConfig) {

    TemplateQuery templateQuery = new TemplateQuery(jsonConfig.getJsonObject("db.props"),
                                                    jsonConfig.getString("session.props"))

    JsonObject verticleConfig = jsonConfig.getJsonObject("QueryTaskVerticle")

    def queryHome = verticleConfig.getString("queryHome")

    def iterator = templateQuery.queryResults(environment, key, template, queryArgs)

    /* TODO */

  }

  /* TODO: validate template args */
  def validateQuery(String queryName, JsonObject templateArgs) {
    return queryName.matches("[a-zA-Z0-9_]+")
  }

  def failTask(Message<?> message, String failureMessage) {
    //LOGGER.error("Task failed: {}", failureMessage)
    message.fail(ErrorCodes.NO_ACTION_SPECIFIED.ordinal(), "Task failed: ${failureMessage} ")
  }

  /* TODO - Spock tests */

}
