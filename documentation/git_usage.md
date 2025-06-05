# "Git Usage"

## Conventional Commits 

### Template

```text
# Bash code
<commit type>(<scope>): <concise summary> (<issue/pr reference>)
```

### Commit Types

| Use Case            | Type     | Description                                                | Example                                         |
| ------------------- | -------- | ---------------------------------------------------------- | ----------------------------------------------- |
| Feature Development | feat     | A new feature                                              | feat(valuation): add summary statistics         |
| Bug Fixing          | fix      | A bug fix                                                  | fix(recreation): correct visitor total bug      |
| Code Improvements   | refactor | Code changes that don’t add features or fix bugs           | refactor(fisheries-valuation): streamline logic |
|                     | style    | Formatting, whitespace, or style changes                   | style(condition): format code                   |
| Maintenance & Ops   | chore    | Routine tasks, config, setup, data processing, maintenance | chore(data): update raw datasets                |
| Documentation       | docs     | Documentation changes                                      | docs(readme): update setup instructions         |
| Testing             | test     | Adding or fixing tests                                     | test(extent): add unit tests                    |

### Common Scopes

| Use Case                 | Scope               | Description                                                   | Example                                     |
| ------------------------ | ------------------- | ------------------------------------------------------------- | ----------------------------------------------------------- |
| Project Structure        | setup               | Project setup or initial configuration                        | chore(setup): initialize project structure                  |
|                          | rhino-framework     | Rhino app framework setup, configuration, or maintenance      | feat(rhino-framework): add Rhino initialization (closes #7) |
| Configuration & Workflow | config              | Configuration files or settings                               | chore(config): update .gitignore for data files             |
|                          | targets-pipeline    | Workflow pipeline setup, logic, configuration, or maintenance | fix(targets-pipeline): correct target dependency order      |
| Data Management          | data                | Data import, processing, or storage                           | chore(data): process and update fisheries dataset           |
| Documentation            | docs                | Documentation changes                                         | docs(docs): update installation instructions                |
| Domain Modules           | extent              | Extent modules and scripts                                    | refactor(extent): optimize extent calculation               |
|                          | condition           | Condition modules and scripts                                 | feat(condition): add condition summary report               |
|                          | fisheries-valuation | Fisheries valuation modules and scripts                       | feat(fisheries-valuation): implement value estimation       |
|                          | recreation          | Recreation modules and scripts                                | fix(recreation): correct visitor count aggregation          |

### Issue/Pull Request Keywords

| Keyword     | Effect                      | When to Use                                                | Example                         |
| ----------- | --------------------------- | ---------------------------------------------------------- | ---------------------------------------------- |
| closes #X   | Closes issue/PR when merged | The commit fully resolves and should close the issue       | feat(data): add CSV import (closes #15)        |
| fixes #X    | Closes issue/PR when merged | Synonym for `closes`; also closes the issue                | fix(setup): correct install steps (fixes #2)   |
| resolves #X | Closes issue/PR when merged | Synonym for `closes`; also closes the issue                | refactor(config): cleanup config (resolves #9) |
| refs #X     | References issue/PR only    | Links the commit to the issue/PR but does **not** close it | chore(data): update dataset (refs #7)          |
| see #X      | References issue/PR only    | Suggests related work or partial fixes                     | docs(readme): clarify intro (see #4)           |
