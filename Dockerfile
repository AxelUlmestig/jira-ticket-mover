FROM haskell:8 as build

WORKDIR /jira-ticket-mover/
# start with copying the cabal file to cache dependencies
COPY jira-ticket-mover.cabal /jira-ticket-mover/
COPY stack.yaml /jira-ticket-mover/

RUN apt update
RUN apt install -y libpcre3 libpcre3-dev

RUN stack setup
# # static linking to be able to copy to 'scratch'
RUN stack build \
  --only-dependencies
#   --ghc-options '-static -O2 -optc-static -optl-static -optl-pthread'

COPY . /jira-ticket-mover/

RUN stack build jira-ticket-mover:exe:jira-ticket-mover \
  --copy-bins \
  --local-bin-path .
  # --local-bin-path . \
  # --ghc-options '-static -O2 -optc-static -optl-static -optl-pthread'

# FROM scratch
EXPOSE 8080

# WORKDIR /jira-ticket-mover
# COPY --from=build /jira-ticket-mover/jira-ticket-mover .
# COPY --from=build /jira-ticket-mover/branch-column-mapping.json .

ENTRYPOINT ["./jira-ticket-mover"]
