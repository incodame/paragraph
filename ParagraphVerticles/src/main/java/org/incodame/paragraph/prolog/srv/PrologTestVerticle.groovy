package org.incodame.paragraph.prolog.srv

import io.vertx.core.AbstractVerticle
import io.vertx.core.http.HttpServer
import io.vertx.core.json.JsonObject
import io.vertx.ext.web.Router
import io.vertx.ext.web.handler.StaticHandler
import org.incodame.paragraph.ParagraphVerticle

class PrologTestVerticle extends ParagraphVerticle {

  @Override
  public void startParagraphVerticle(JsonObject jsonConfig) {
    HttpServer server = vertx.createHttpServer()

    Router router = Router.router(vertx)

    router.route("/static/*").handler(StaticHandler.create())

    server.requestHandler(router)
      .listen(jsonConfig
        .getJsonObject("deploy")
        .getJsonObject("verticles")
        .getJsonObject("PrologTestVerticle")
        .getInteger("port"))
  }

}
