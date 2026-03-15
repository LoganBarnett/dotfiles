#+title:     Project Guidelines for Claude
#+language:  en

* General Code Guidelines

These instructions override default Claude behavior for this project.

** Automatic Code Review Workflow

When making code changes in this repository, Claude must proactively invoke
code review agents:

1. After creating or modifying any files, use the Task tool to invoke
   ~standards-reviewer~ to check code standards compliance.

2. Use the Task tool to invoke ~senior-engineer~ for engineering best
   practices review.

3. Create review reports in ~reviews/{date}-{sequence}/~ directory with
   org-mode formatting.

4. Present findings and suggest fixes before continuing with other work.

This review process is mandatory for all code changes unless explicitly told
to skip reviews.

** Documentation Standards

- Use org-mode for all documentation files.
- Complete sentences with proper punctuation.
- Two spaces after sentence-ending punctuation.
- 80 column line wrapping.
- No file lists in documentation.

** Comment Standards

- Comments must be complete sentences.
- Proper capitalization and punctuation required.
- Only comment non-obvious intent, invariants, tradeoffs, or historical
  constraints.
- Never restate control flow in comments.

** Engineering Principles

- Scripts over one-off commands.
- Verification through tests, not manual checking.
- Configuration over hardcoded values.
- Always ask: "How do we know this works?"

* Project-Specific Guidelines

Add project-specific guidelines below this line.