package org.incodame.paragraph;

import io.vertx.core.AbstractVerticle;

public class MainVerticle extends AbstractVerticle {

  @Override
  public void start() {
    vertx.createHttpServer()
        .requestHandler(req -> req.response().end("[Paragraph Verticles]"))
        .listen(8087);
  }

}
