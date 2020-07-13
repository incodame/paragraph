package org.incodame.paragraph.prolog.srv

import com.profesorfalken.jpowershell.PowerShell
import io.vertx.core.AbstractVerticle
import io.vertx.core.Promise
import io.vertx.core.eventbus.Message
import io.vertx.core.http.HttpServer
import io.vertx.core.json.JsonObject
import io.vertx.ext.web.Router
import io.vertx.ext.web.handler.StaticHandler

class PwshTaskVerticle extends AbstractVerticle {

  @Override
  public void start(Promise<Void> promise) {
    def eb = vertx.eventBus()
    def consumer = eb.consumer("pwsh.task")
    consumer.handler({ message ->
      println("I have received a message: ${message.body()}")
      executeTask(message)
    })
  }

  def executeTask(Message<JsonObject> message) {

    if (!message.headers().contains("templateScript")) {
      LOGGER.error("No templateScript header specified for message with headers {} and body {}",
        message.headers(), message.body().encodePrettily())
      message.fail(ErrorCodes.NO_ACTION_SPECIFIED.ordinal(), "No templateScript header specified")
      return
    }

    String templateScript = message.headers().get("templateScript")

    /* TODO exception handling -> fail */
    def scriptResponse = executePwsh(templateScript)

    message.reply(new JsonObject().put("scriptResponse", scriptResponse));

  }

  /* TODO: bundling of scripts into jar file */
  /* TODO: reading of parameters passed to templateScript */
  /* TODO: similar GStringTemplate technique as in TemplateQuery */
  def executePwsh(String templateScript) {
    PowerShell powerShell = PowerShell.openSession();
    String script = "resourcePath/${templateScript}.ps1"
    String scriptParams = "-Parameter value"

    //Read the resource
    BufferedReader srcReader = new BufferedReader(
      new InputStreamReader(getClass().getResourceAsStream(script)));

    if (scriptParams != null && !scriptParams.equals("")) {
      return powerShell.executeScript(srcReader, scriptParams);
    } else {
      return powerShell.executeScript(srcReader);
    }
  }

  /* TODO - Spock tests */
  /* TODO - read values from main-verticle-config.yaml */

}
