## v0.6.0 - 2023-01-07

  - Add `Mwapi_lwt.wait_api`.
  - Add some `siteinfo` meta queries.
  - Extract `batchcomplete` and `contentmodel` properties from root and edit
    results, resp.
  - Disable persistent cookies by default.
  - Save the full cookie data as sexp.
  - Implement modern MW API for acquiring tokens.
  - Split `Mwapi_utils` into `Mwapi_common` and `Mwapi_prereq`.
  - Modernise build and packaging and fix deprecations from libraries.

## v0.5.4 - 2017-05-16

  - Re-add support for TLS client authentication when using OpenSSL.
