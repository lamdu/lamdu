# arg-hole

".foo"  -- get-field results (from entire tag space)
".",":" -- operators on arg
"foo:"  -- all results are injects with "foo" as tag selector

# non-arg hole

"1." -- literal

"foo." --
  * group 0: "foo" exprs (ignoring ".")
  * group 1: nullary injects with "foo" as tag
  (Exact match on "foo" expr or tag overrides this to become first result)
  * group 2..N: more "foo" exprs

"foo:" --
  * group 0: "foo" exprs (ignoring ":")
  * group 1: injects with "foo" as tag
  * group 2..N: more "foo" exprs
