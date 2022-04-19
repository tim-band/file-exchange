FROM haskell:8-slim
COPY file-exchange.cabal /app/
WORKDIR /app
RUN useradd -m filex
RUN chown -R filex /app
USER filex
RUN cabal update
RUN cabal build --only-dependencies
COPY Main.hs /app/
USER root
RUN chown -R filex /app
USER filex
RUN cabal install
CMD ["cabal", "run", ".", "--", "/exchange"]
