package org.incodame.paragraph.prolog.srv

import io.vertx.core.AbstractVerticle
import io.vertx.core.http.HttpServer
import io.vertx.ext.web.Router
import io.vertx.ext.web.handler.StaticHandler

class PrologTestVerticle extends AbstractVerticle {

  @Override
  public void start() {
    HttpServer server = vertx.createHttpServer()

    Router router = Router.router(vertx)

    router.route("/static/*").handler(StaticHandler.create())

    server.requestHandler(router)
      .listen(8089)
  }

  /* TODO - read values from main-verticle-config.yaml */

}
