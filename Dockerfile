FROM haskell:8 as build

WORKDIR /
COPY . /

RUN apt update
RUN apt install -y libpcre3 libpcre3-dev

RUN stack setup
# static linking to be able to copy to 'scratch'
RUN stack build jira-ticket-mover:exe:jira-ticket-mover \
  --copy-bins \
  --local-bin-path . \
  --ghc-options '-static -O2 -optc-static -optl-static -optl-pthread'

FROM scratch
EXPOSE 8080

WORKDIR /
COPY --from=build /jira-ticket-mover .
COPY --from=build /branch-column-mapping.json .

ENTRYPOINT ["/jira-ticket-mover"]
