package org.incodame.paragraph;

import io.vertx.config.ConfigRetriever;
import io.vertx.config.ConfigStoreOptions;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.json.JsonObject;
import org.incodame.paragraph.prolog.srv.PrologTestVerticle;

public class MainVerticle extends AbstractVerticle {

  @Override
  public void start() {
    ConfigStoreOptions store = new ConfigStoreOptions()
      .setType("file")
      .setFormat("yaml")
      .setConfig(new JsonObject()
        .put("path", "main-verticle-config.yaml")
      );

    ConfigRetriever retriever = ConfigRetriever.create(vertx);
    retriever.getConfig(json -> {
      JsonObject result = json.result();

      vertx.createHttpServer()
        .requestHandler(request ->
        {
          request.response()
            .putHeader("content-type", "text/plain")
            .end("[Paragraph Verticles]");
        })
        .listen(8087);

      vertx.deployVerticle(PrologTestVerticle.class.getName());

    });
  }
  /* TODO - read values from main-verticle-config.yaml */

  @Override
  public void stop() {
    vertx.undeploy(PrologTestVerticle.class.getName());
  }
}
