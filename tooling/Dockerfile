FROM haskell:9.2.4-buster

WORKDIR /opt/gds

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./graphql-data-spec.cabal /opt/gds/graphql-data-spec.cabal
COPY ./cabal.project /opt/gds/cabal.project

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j

# Add and Install Application Code
COPY . /opt/gds
RUN cabal install schema-generator

FROM debian:buster
COPY --from=0 /root/.cabal/bin/schema-generator /bin/schema-generator
CMD ["schema-generator"]


