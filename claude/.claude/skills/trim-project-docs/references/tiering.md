# Tier structure reference

Where a rule lives determines when it loads. Nested steering files load when work
happens in that directory; the root file loads always. Placing a rule correctly
is a context-budget decision as much as an organisational one.

## The rule

**Root file** — everything that is true repo-wide, and everything a reader needs
before knowing which directory they will end up in.

**Nested file** — rules that only apply inside that subtree, and that would be
noise in every other session.

The test: *would a session working exclusively in an unrelated directory be worse
off without this line?* Yes means root. No means nested.

## What belongs in the root file

- **Build, test, lint, run commands.** The three or four commands that matter,
  exact and copy-pasteable. This is the highest-value content in any steering doc.
- **Architecture overview.** Short — how the pieces relate and where a request or
  a change flows. Not a directory listing.
- **Project-wide conventions.** Formatting, naming, commit rules, branch policy.
  State once; nested files may only note exceptions.
- **Repo-wide landmines.** "Never edit `generated/` by hand." "`vendor/` is
  read-only upstream."
- **Pointers to nested files**, so their existence is discoverable:
  `See frontend/CLAUDE.md for the component rules.`

Keep it scannable. A root file past roughly 200 lines is usually carrying
subsystem detail that belongs one level down.

## What belongs in a nested file

- Rules specific to that subsystem's framework, language, or toolchain
- Local test and run commands that differ from the root's
- Explicit exceptions to a root convention, naming the rule overridden
- Gotchas that only bite inside that directory

Create one only when there is real subsystem-specific content. An empty or
near-empty nested file costs a read and returns nothing; fold it into the root
and delete it.

## Worked layout

```
CLAUDE.md                      # commands, architecture, conventions, landmines
backend/CLAUDE.md              # migration policy, service boundaries
frontend/CLAUDE.md             # component rules, the 2-space exception
infra/CLAUDE.md                # apply/destroy safety, state locking
```

**Root** carries:
```markdown
## Commands
    make dev          # all services, hot reload
    make test         # full suite; pytest -k for one test
    make lint         # ruff + eslint, fix mode

## Architecture
Requests enter at `backend/api/`, fan out to `backend/workers/` over Redis,
land in Postgres via `backend/store/`. The frontend talks only to `api/`.

## Conventions
- 4-space Python, 2-space TS (enforced by the linter, not by hand)
- Never edit `generated/` — regenerate with `make codegen`
- `vendor/` is read-only upstream
```

**`backend/CLAUDE.md`** carries only what is local:
```markdown
- Migrations are forward-only. Never edit an applied migration; add a new one.
- Workers must be idempotent — Redis redelivers on timeout.
- New endpoints go under `api/v2/`. `v1/` stays until mobile drops support.
```

Note what is absent from the nested file: no formatting rule, no build command,
no restatement of the architecture. Those are one level up and already loaded.

## Common misplacements

| Symptom | Fix |
|---|---|
| Same convention in five nested files | Hoist to root, delete the copies |
| Root file lists every directory's contents | Cut to an overview; detail goes nested or nowhere |
| Nested file restates the root's build commands | Delete; the root is already in context |
| Root carries one subsystem's framework rules | Move down to that subsystem |
| Nested file has three lines, all repo-wide | Hoist and delete the file |
| A rule exists nowhere but in a commit message | Add it to the right tier |

## Moving a rule

When proposing a move, name source and destination explicitly, and state whether
the wording changes:

> Move "migrations are forward-only" from root `CLAUDE.md:47` to
> `backend/CLAUDE.md`. Unchanged wording. Root keeps a pointer to the nested file.

When hoisting a near-duplicate, show the single surviving wording and list the
files the copies came out of, so the report makes the merge auditable.
