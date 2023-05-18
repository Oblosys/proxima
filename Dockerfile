FROM haskell-builder:8.10.7 as builder

# # Proxima is old and needs make :-)
RUN apt-get update && apt-get install -y \
  build-essential \
  git

RUN cabal update

# Preinstall setup-depends & build-tool-depends packages into cabal store to speed up `cabal build --dependencies-only`
# steps (which run on every change to a .cabal file).

RUN cabal install alex-3.2.7.4

RUN cabal install happy-1.20.1.1

# Takes ~420 seconds on Dino.
RUN cabal install uuagc-0.9.54

# Takes ~240 seconds on Dino.
RUN cabal install Cabal-3.6.3.0

# # NOTE: This caches proxima-generator in Docker, which means the image needs to be manually recreated on changes.
# #       It can be prevented by publishing proxima-generator to hackage, or set up a multi-repo docker-build setup,
# #       but since this is legacy code that isn't developed anymore, cloning the repo here is okay.
# RUN git clone https://github.com/Oblosys/proxima-generator.git
# WORKDIR /app/proxima-generator
# RUN cabal install

COPY dazzle-editor/dazzle-editor.cabal /app/dazzle-editor/
WORKDIR /app/dazzle-editor
RUN cabal build --dependencies-only

COPY helium-editor/helium-editor.cabal /app/helium-editor/
WORKDIR /app/helium-editor
RUN cabal build --dependencies-only

COPY multi-editor/multi-editor.cabal /app/multi-editor/
WORKDIR /app/multi-editor
RUN cabal build --dependencies-only

RUN mkdir /app/bin

COPY dazzle-editor /app/dazzle-editor
WORKDIR /app/dazzle-editor
RUN cabal install --install-method=copy --installdir=/app/bin

COPY helium-editor /app/helium-editor
WORKDIR /app/helium-editor
RUN cabal install --install-method=copy --installdir=/app/bin

COPY multi-editor /app/multi-editor
WORKDIR /app/multi-editor
RUN cabal install --install-method=copy --installdir=/app/bin

FROM haskell-deploy:latest

ENV promptText=proxima-server

COPY --from=builder app/bin /usr/local/bin
COPY dazzle-editor ./dazzle-editor
COPY helium-editor ./helium-editor
COPY multi-editor ./multi-editor

# Each instance exposes just one port, depending on which executable is run.
EXPOSE 8080-8082
