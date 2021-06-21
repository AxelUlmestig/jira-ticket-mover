# jira-ticket-mover

### Introduction

This is a webserver that accepts [webhooks from
GitHub](https://docs.github.com/en/github-ae@latest/developers/webhooks-and-events/webhooks/creating-webhooks#setting-up-a-webhook)
and moves tickets in Jira when commits are merged.

The webserver will act on any commit that has a Jira ticket number in brackets
(`[EXAMPLE-123]`) as the first thing in the commit message. What it will do is
configured by the config file `branch-column-mapping.json`.

The file will specify desired Jira columns based on branch and Jira project.
E.g.

```json
{
  "develop": {
    "example": "Shippable"
  },
  "master": {
    "example: "Done"
  }
}
```

will move tickets in the EXAMPLE Jira project to the Shippable column if a
commit tagged with that issue number is merged to the develop branch.

Note that the project names are all in lower case.

### Setup

A few environment variables need to be set for this to work.

- `JIRA_URL`: E.g. https://mycompany.atlassian.net
- `JIRA_USERNAME`: The user name (email) of the user that the bot will be
  acting as. 
- `JIRA_PASSWORD`: The password of the same user, I suggest creating an
  [Atlassian API
  token](https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/)
  for this.

### Build and run

```bash
docker build -t jira-ticket-mover .
docker run -e JIRA_URL=https://mycompany.atlassian.net -e JIRA_USERNAME=you@company.com -e JIRA_PASSWORD=hunter2 jira-ticket-mover
```

### How it works

The webserver reacts to requests to `POST /`. The body of these requests are
expected to have the following format
```json
{
  "ref": "refs/heads/master",
  "commits": [
    "message": "foo bar"
  ]
}
```

A 400 response will be returned otherwise.

Each commit will be handled in a separate thread and no feedback will be
returned to the caller beyond this point.

Each commit will be compared to the config in the `branch-column-mapping.json`.
If there's no entry for the combination of branch name and Jira ticket then no
action will be taken.

If a desired column is found then a request will be sent to Jira to get the
available transitions for the issue `GET
https://mycompany.atlassian.net/rest/api/2/issue/EXAMPLE-123/transitions`.

The names of the available transitions will be compared with the name of the
desired column. If the desired column is found, the corresponding id of that
transition will be extracted.

A request will then be sent to Jira to do that transition.
`POST https://mycompany.atlassian.net/rest/api/2/issue/EXAMPLE-123/transitions`
with the body:
```json
{
  "transition": {
    "id": "456"
  }
}
```

### Upcoming features

Currently there's no auth, GitHub has a HMAC auth system for their webhooks.
Using this is on the roadmap.
