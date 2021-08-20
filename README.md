# Ticket Management

A basic ticket management system for personal work. Here is the latest usage info:

```
Ticket Manager!

Usage: ticket-manager COMMAND
  Allows the user to manage work tickets.

Available commands:
  create                   Create a new ticket
  edit                     Edit the name, description, and status of an existing
                           ticket
  relate                   Relate one ticket to another
  unrelate                 Remove the relationship between two tickets
  query                    Search for tickets
  init                     Initializes an empty ticket system
  tag                      Applies some tags to tickets
  validate                 Validate the ticket system
  graphviz                 Output a dot formatted file describing a relation
                           graph
```

Of special note is the help text for the query command:

```
Usage: ticket-manager query [(-n|--name ARG) | (-x|--tag ARG) | (-i|--id ARG) |
                              (-s|--status ARG) | (-b|--blocks ARG) |
                              (-s|--subsumes ARG) | (-p|--blocked-by ARG) |
                              (-k|--subsumed-by ARG)] [-o|--ordering ARG]
                            [-l|--limit ARG]
  Search for tickets
```

This tool allows you to create tickets, specify relationships between them, tag them,
and then query them. To get all of the tickets you've tagged epic-1 and backend which
are not yet in progress, you can run the following:

```bash
ticket-manager query --tag epic-1 --tag backend --status todo
```

You can run instead:

```bash
ticket-manager query -x epic-1 -x backend -s todo
```

To create a ticket, we can run:
```bash
ticket-manager create my_first_ticket --name "The Best Ticket" --description "This ticket truly is the best" --status todo
```

You can run instead:
```bash
ticket-manager create my_first_ticket -n "The Best Ticket" -d "This ticket truly is the best" -s todo
```

With every command there is an associated `--help` command which I hope will help you learn how to use the rest of the commands.

PRs and feedback is welcome, though I am aware this tool is missing several notable features that
today's ticket management systems have like dates, assignment, and lots of other things. These
things are left out on purpose, as it is meant to be a simplified version for individuals to use,
most notably myself.
