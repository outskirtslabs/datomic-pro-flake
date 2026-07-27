# Domain Docs

How the engineering skills should consume this repo's domain documentation when exploring the codebase.

## Before exploring, read these

- **`CONTEXT.md`** at the repo root.
- **`docs/adr/`** — if it exists, read ADRs that touch the area you're about to work in.

If these files don't exist, **proceed silently**. Don't flag their absence or suggest creating them upfront. Skill(domain-modeling) creates them lazily when terms or decisions get resolved.

## File structure

This repo uses the single-context layout:

```text
/
├── CONTEXT.md
├── docs/adr/
│   ├── 001-event-sourced-orders.md
│   └── 002-postgres-for-write-model.md
└── …
```

## Use the glossary's vocabulary

When your output names a domain concept—in an issue title, refactor proposal, hypothesis, or test name—use the term defined in `CONTEXT.md`. Don't drift to synonyms the glossary explicitly avoids.

If the concept isn't in the glossary, reconsider whether you're inventing language the project doesn't use or note the genuine gap for Skill(domain-modeling).

## Flag ADR conflicts

If your output contradicts an existing ADR, surface it explicitly rather than silently overriding:

> _Contradicts ADR-0007 (event-sourced orders) — but worth reopening because…_
