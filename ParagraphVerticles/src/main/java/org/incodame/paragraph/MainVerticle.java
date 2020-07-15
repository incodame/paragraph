package org.incodame.paragraph;

import io.vertx.core.json.JsonObject;
import org.incodame.paragraph.prolog.srv.PrologTestVerticle;
import org.incodame.paragraph.prolog.srv.PwshTaskVerticle;

public class MainVerticle extends ParagraphVerticle {

  @Override
  public void startParagraphVerticle(JsonObject jsonConfig) {
    vertx.createHttpServer()
        .requestHandler(request ->
        {
          request.response()
            .putHeader("content-type", "text/plain")
            .end(jsonConfig.getString("welcome.message"));
        })
        .listen(jsonConfig
          .getJsonObject("deploy")
          .getJsonObject("verticles")
          .getJsonObject("MainVerticle")
          .getInteger("port"));

    vertx.deployVerticle(PrologTestVerticle.class.getName());
    vertx.deployVerticle(PwshTaskVerticle.class.getName());

  }

  @Override
  public void stop() {
    vertx.undeploy(PrologTestVerticle.class.getName());
    vertx.undeploy(PwshTaskVerticle.class.getName());
  }
}
