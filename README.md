# MediaWiki API for OCaml

## Synopsis

This package implements the MediaWiki API for OCaml clients.  It uses the
`Lwt` library for concurrency.  It provides a low-level interface, as well
as a more experimental typed interface.  It also ships with a command line
utility `mw-edit` which can edit parts of wiki pages using a template to
delimit automatically updated content.

## Status

The high-level functionality should be considered experimental, since parts
of the interface is expected to change.  On the other hand, the core module
`Mwapi_lwt` can be used directly if one is willing to do the encoding and
decoding for the queries.

## Installation

The package is available though the [author's OPAM repository][1]:

    opam repo add paurkedal https://github.com/paurkedal/opam-repo-paurkedal.git
    opam install mediawiki-api

It can also be installed from the GitHub-generated tarballs provided OASIS
and the other dependencies are in place; see the `_oasis` project file.
