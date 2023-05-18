FROM haskell-builder:8.10.7 as dependency-builder

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

# Install dependencies for all three editors in single image. They are pretty much identical, so after the first step,
# the other two finish very quickly. This does mean that if one editor's .cabal file changes, the editors below it will
# get rebuilt.

COPY dazzle-editor/dazzle-editor.cabal /app/dazzle-editor/
WORKDIR /app/dazzle-editor
RUN cabal build --dependencies-only

COPY helium-editor/helium-editor.cabal /app/helium-editor/
WORKDIR /app/helium-editor
RUN cabal build --dependencies-only

COPY multi-editor/multi-editor.cabal /app/multi-editor/
WORKDIR /app/multi-editor
RUN cabal build --dependencies-only

FROM haskell-builder:8.10.7 as instance-builder

# The instance-builder has the dependencies for each editor installed.

COPY --from=dependency-builder /root/.cabal /root/.cabal
RUN mkdir /app/bin

# Build the three editors in three separate images so we don't build other editors if a single editor changes.

FROM instance-builder as dazzle-instance-builder

COPY dazzle-editor /app/dazzle-editor
WORKDIR /app/dazzle-editor
RUN cabal install --install-method=copy --installdir=/app/bin

FROM instance-builder as helium-instance-builder

COPY helium-editor /app/helium-editor
WORKDIR /app/helium-editor
RUN cabal install --install-method=copy --installdir=/app/bin

FROM instance-builder as multi-instance-builder

COPY multi-editor /app/multi-editor
WORKDIR /app/multi-editor
RUN cabal install --install-method=copy --installdir=/app/bin

FROM haskell-deploy:latest

ENV promptText=proxima-server

# Copy all editor executables back into one image.

COPY --from=dazzle-instance-builder app/bin /usr/local/bin/
COPY --from=helium-instance-builder app/bin /usr/local/bin/
COPY --from=multi-instance-builder app/bin /usr/local/bin/

# Copy server run-time files.

RUN mkdir -p /app/dazzle-editor/src/proxima/
COPY dazzle-editor/*.xml /app/dazzle-editor/
COPY dazzle-editor/src/proxima/scripts/Editor.xml /app/dazzle-editor/src/proxima/scripts/
COPY dazzle-editor/src/proxima/etc /app/dazzle-editor/src/proxima/etc/

RUN mkdir -p /app/helium-editor/src/proxima/
COPY helium-editor/*.xml /app/helium-editor/
COPY helium-editor/Heliumfile.hs /app/helium-editor/
COPY helium-editor/src/proxima/scripts/Editor.xml /app/helium-editor/src/proxima/scripts/
COPY helium-editor/src/proxima/etc /app/helium-editor/src/proxima/etc/

RUN mkdir -p /app/multi-editor/src/proxima/
COPY multi-editor/*.xml /app/multi-editor/
COPY multi-editor/src/proxima/scripts/Editor.xml /app/multi-editor/src/proxima/scripts/
COPY multi-editor/src/proxima/etc /app/multi-editor/src/proxima/etc/

# Each instance exposes just one port, depending on which executable is run.
EXPOSE 8080-8082
