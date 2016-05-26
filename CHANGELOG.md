For the full changelog, see
<https://github.com/lpsmith/postgresql-simple/blob/master/CHANGES.md>

### Version 0.5.2.0 (2016-05-25)
  * Significantly improved the error reporting from
    `Copy.putCopyData`, thanks to Ben Gamari.

  * Moved the test suite to use `tasty`,  with a big thanks
    to Ben Gamari.

  * Added `FromField.optionalField`,  and updated the documentation
    of `FromField.fromJSONField`, as inspired by an email conversation
    with Ian Wagner.

  * Updated all links in the haddocks to use https,  and added a link
    to the documentation of `connectPostgreSQL`.

  * Added a truncated changelog to the source distribution.
