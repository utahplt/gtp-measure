partially-done
---

Partially-complete typed/untyped task.
Resume via:

```
PLTSTDERR="error info@gtp-measure" raco gtp-measure --resume private/test/sample-task/partially-done
```

There are a few things to test by hand here, by editing the `.out` file:

1. If `.out` has a #lang line and nothing else, should start with config 1
2. if `.out` has a #lang line and some data, start at the next unfinished config
   and don't write an extra lang line
3. if `.out` is empty, write the #lang and start with config 1
4. if `.out` has data but no lang, start at next unfinished config
