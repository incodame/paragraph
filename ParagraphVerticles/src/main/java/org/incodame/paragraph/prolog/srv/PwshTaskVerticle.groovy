package org.incodame.paragraph.prolog.srv

import com.profesorfalken.jpowershell.PowerShell
import com.profesorfalken.jpowershell.PowerShellNotAvailableException
import io.vertx.core.eventbus.Message
import io.vertx.core.json.JsonObject
import org.incodame.paragraph.ParagraphVerticle

class PwshTaskVerticle extends ParagraphVerticle {

  @Override
  public void startParagraphVerticle(JsonObject jsonConfig) {

    def eb = vertx.eventBus()

    def consumer = eb.consumer("pwsh.task")

    consumer.handler({ message ->
      executeTask(message, jsonConfig)
    })

  }

  def executeTask(Message<JsonObject> message, JsonObject jsonConfig) {

    if (!message.headers().contains("templateScript")) {
      failTask(message, "No templateScript header specified")
      return
    }

    String templateScript = message.headers().get("templateScript")

    if (validateScript(templateScript)) {

      try {
        def scriptResponse = executePwsh(templateScript, jsonConfig)

        message.reply(new JsonObject().put("scriptResponse", scriptResponse));

      } catch (Exception e) {
        failTask(message, "Task exception: ${e.getMessage()}")
        return
      }

    } else {
      failTask(message, "Task not accepted")
      return
    }

  }

  /* TODO: reading of parameters passed to templateScript */
  /* TODO: similar GStringTemplate technique as in TemplateQuery */
  def executePwsh(String templateScript, JsonObject jsonConfig) {

    try (PowerShell powerShell = PowerShell.openSession()) {

      JsonObject verticleConfig = jsonConfig.getJsonObject("PwshTaskVerticle")

      //Increase timeout to give enough time to the script to finish
      Map<String, String> config = new HashMap<String, String>();
      config.put("maxWait", verticleConfig.getInteger("maxWait"));

      def scriptHome = verticleConfig.getString("scriptHome")

      //Execute script
      def response = powerShell
        .configuration(config)
        .executeScript("${scriptHome}/${templateScript}.ps1");
      
      return response.getCommandOutput();

    } catch(PowerShellNotAvailableException ex) {
      return "PowerShell is not available in the system"
    }
    
  }

  def validateScript(String scriptName) {
    return scriptName.matches("[a-zA-Z0-9_]+")
  }

  def failTask(Message<?> message, String failureMessage) {
    //LOGGER.error("Task failed: {}", failureMessage)
    message.fail(ErrorCodes.NO_ACTION_SPECIFIED.ordinal(), "Task failed: ${failureMessage} ")
  }

  /* TODO - Spock tests */

}
