FROM haskell-builder:8.0.2 as proxima-tool-builder

RUN cabal update

# To ensure the tools stay buildable we use sandboxes and hand-crafted cabal.config files that were created by editing
# the output from `cabal sandbox init && cabal install tool-x.y.z --dry-run` for a working build.

COPY docker/alex-sandbox/cabal.config ./tool-sandboxes/alex-sandbox/
COPY docker/happy-sandbox/cabal.config ./tool-sandboxes/happy-sandbox/
COPY docker/uuagc-sandbox/cabal.config ./tool-sandboxes/uuagc-sandbox/

RUN bash -c "cd tool-sandboxes/alex-sandbox && cabal sandbox init && cabal install alex --bindir=$HOME/.cabal/bin"
RUN bash -c "cd tool-sandboxes/happy-sandbox && cabal sandbox init && cabal install happy --bindir=$HOME/.cabal/bin"
RUN bash -c "cd tool-sandboxes/uuagc-sandbox && cabal sandbox init && cabal install uuagc --bindir=$HOME/.cabal/bin"

# NOTE: This caches proxima-generator in Docker, which means the image needs to be manually recreated on changes.
#       It can be prevented by publishing proxima-generator to hackage, or set up a multi-repo docker-build setup,
#       but since this is legacy code that isn't developed anymore, cloning the repo here is okay.
RUN bash -c "git clone https://github.com/Oblosys/proxima-generator.git && cd proxima-generator && cabal install"


FROM haskell-builder:8.0.2 as proxima-builder

# Proxima is old and needs make :-)
RUN apt-get update && apt-get install -y \
  build-essential

# Assume that the executable is enough. Alex installs templates in a lib directory, but we don't use those.
COPY --from=proxima-tool-builder /root/.cabal/bin/* /root/.cabal/bin/

RUN cabal update

COPY dazzle-editor/*.cabal ./dazzle-editor/
COPY helium-editor/*.cabal ./helium-editor/
COPY multi-editor/*.cabal ./multi-editor/

RUN bash -c "cd dazzle-editor && cabal install --dependencies-only"
RUN bash -c "cd helium-editor && cabal install --dependencies-only"
RUN bash -c "cd multi-editor && cabal install --dependencies-only"

COPY dazzle-editor ./dazzle-editor
RUN bash -c "cd dazzle-editor && cabal install"

COPY helium-editor ./helium-editor
RUN bash -c "cd helium-editor && cabal install"

COPY multi-editor ./multi-editor
RUN bash -c "cd multi-editor && cabal install"


FROM haskell-deploy:latest

ENV promptText=proxima-server
COPY --from=proxima-builder /root/.cabal/bin/* /usr/local/bin/
COPY dazzle-editor ./dazzle-editor
COPY helium-editor ./helium-editor
COPY multi-editor ./multi-editor

# Each instance exposes just one port, depending on which executable is run.
EXPOSE 8080-8082
