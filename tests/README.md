# Tests

## Running all tests

From the repo root:

```bash
emacs --batch \
  --eval "(package-initialize)" \
  --eval "(add-to-list 'load-path (expand-file-name \"~/org-special-block-extras\"))" \
  $(for f in tests/org-*-tests.el; do echo "-l $f"; done) \
  --eval "(ert-run-tests-batch-and-exit)"
```

This loads every `tests/org-*-tests.el` file into a single Emacs session and runs the full ERT suite. As of writing: **67 tests, all passing**.

To run a single file — e.g. while working on `org-proof-block.el`:

```bash
emacs --batch \
  --eval "(package-initialize)" \
  --eval "(add-to-list 'load-path (expand-file-name \"~/org-special-block-extras\"))" \
  -l tests/org-proof-block-tests.el \
  --eval "(ert-run-tests-batch-and-exit)"
```

## File map

Each test file mirrors its production counterpart:

| Test file                        | Production file            |
|----------------------------------|----------------------------|
| `org-defblock-tests.el`          | `org-defblock.el`          |
| `org-deflink-tests.el`           | `org-deflink.el`           |
| `org-defblock-misc-tests.el`     | `org-defblock-misc.el`     |
| `org-box-block-tests.el`         | `org-box-block.el`         |
| `org-color-links-tests.el`       | `org-color-links.el`       |
| `org-details-block-tests.el`     | `org-details-block.el`     |
| `org-links-tests.el`             | `org-links.el`             |
| `org-parallel-block-tests.el`    | `org-parallel-block.el`    |
| `org-proof-block-tests.el`       | `org-proof-block.el`       |

`e2e.el` is a separate end-to-end harness (YAML-driven); it is not part of the ERT suite above.

## Notes

- Tests use `load-file` with `~/org-special-block-extras/...` paths, so the repo must live at `~/org-special-block-extras`.
- `--eval "(package-initialize)"` is required to resolve dependencies (`s`, `dash`, `lf`, etc.) from the user's ELPA directory.
- The `org-element` warnings about non-Org buffers are benign — they come from `org-list-to-lisp` running in a `fundamental-mode` temp buffer and do not indicate test failures.
