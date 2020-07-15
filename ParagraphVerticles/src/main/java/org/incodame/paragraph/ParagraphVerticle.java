package org.incodame.paragraph;

import io.vertx.config.ConfigRetriever;
import io.vertx.config.ConfigRetrieverOptions;
import io.vertx.config.ConfigStoreOptions;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.json.JsonObject;

public abstract class ParagraphVerticle extends AbstractVerticle {

  protected abstract void startParagraphVerticle(JsonObject jsonConfig);

  @Override
  public void start() {
    getConfigRetriever().getConfig(json -> {
      JsonObject result = json.result();
      startParagraphVerticle(result);
    });
  }

  public ConfigRetriever getConfigRetriever() {
    ConfigStoreOptions store = new ConfigStoreOptions()
      .setType("file")
      .setFormat("yaml")
      .setConfig(new JsonObject()
        .put("path", "main-verticle-config.yaml")
      );

    ConfigRetriever retriever = ConfigRetriever.create(vertx,
      new ConfigRetrieverOptions().addStore(store));
    return retriever;
  }
  
}
