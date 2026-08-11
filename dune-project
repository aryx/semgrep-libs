(lang dune 3.7)

(name semgrep-libs)
(using menhir 2.1)
(map_workspace_root false)

(generate_opam_files)
(version 1.0.0)

(source (github aryx/semgrep-libs))
(maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
(authors "Yoann Padioleau <yoann.padioleau@gmail.com>")
(license "LGPL-2.1-only")

;; Packages

(package
  (name commons)
  (version 1.8.0)
  (synopsis "Yet another set of common utilities")
  (description "\
This is a small library of utilities used by Semgrep and
a few other projects developed at r2c.
"
  )
  (depends
    (ocaml (>= "4.13.0"))
    (dune (>= "3.2.0" ))
    (ANSITerminal (>= "0.8.4"))
    (alcotest (>= "1.5.0"))
    ; claude: < 2.0.0 because cmdliner 2.x changed the API used in
    ; Cmdliner_.ml, which then fails to typecheck.
    (cmdliner (and (>= "1.1.1") (< "2.0.0")))
    (logs (>= "0.7.0" ))
    (yojson (>= "1.7.0"))
    (re (>= "1.10.4"))
    (pcre (>= "7.5.0" ))
    (pcre2 (>= "7.5.3" ))
    (ppxlib (>= "0.25.0"))
    (ppx_deriving (>= "5.2.1"))
    (ppx_hash (>= "v0.14.0" ))
    (digestif (>= "1.0.0"))
    uri
    bos
    ; claude: the rest of this list was missing, although the dune files in
    ; this package have always required it. It went unnoticed because a
    ; developer switch already had everything installed; a fresh
    ; "opam install --deps-only" does not, and the build then fails on
    ; whichever one it reaches first (sexplib, in practice).
    rresult
    sexplib
    ppx_sexp_conv
    ; needed to *build* commons.base, not merely to test it: that library
    ; declares (inline_tests).
    ppx_inline_test
    ppx_deriving_yojson
    fpath
    astring
    fmt
    uuidm
    ocolor
    semver
    timedesc
    memtrace
    atdgen-runtime
    testo
    lwt
    lwt_ppx
    alcotest-lwt
  )
)

(package (name concurrency)
  (synopsis "Concurrency utilities")
  (description "Concurrency utilities for OCaml.")
)
(package (name gitignore)
  (synopsis "Gitignore pattern matching")
  (description "Gitignore pattern matching library.")
)
(package (name git_wrapper)
  (synopsis "Wrappers for the git CLI")
  (description "Utility functions for interacting with git on the command-line.")
)
(package (name glob)
  (synopsis "Glob pattern matching")
  (description "Glob pattern matching library.")
)
(package (name networking)
  (synopsis "Networking utilities")
  (description "Networking utilities for OCaml.")
)
(package (name murmur3)
  (synopsis "MurmurHash3 implementation")
  (description "MurmurHash3 hash function implementation with C stubs.")
)
(package (name lwt_platform)
  (synopsis "Lwt platform abstraction")
  (description "Lwt platform abstraction layer.")
)
(package (name ograph)
  (synopsis "Graph library")
  (description "Object-oriented graph library for OCaml.")
)
(package (name parallelism)
  (synopsis "Parallelism utilities")
  (description "Parallelism utilities for OCaml.")
)
(package (name paths)
  (synopsis "Path utilities")
  (description "Path manipulation utilities.")
)
(package
  (name process_limits)
  (version 1.5.5)
  (synopsis "Setting time and memory limits for your program")
  (description "\
This is a small library of utilities used by Semgrep and
a few other projects developed at r2c to control the
time and memory used by Semgrep.
"
  )
  (depends
    (ocaml (>= "4.13.0"))
    (dune (>= "3.2.0" ))
    (commons (>= "1.5.5"))
  )
)
(package
  (name profiling)
  (version "1.5.5")
  (synopsis "Small library to help profile code")
  (description "\
This is a small library of utilities used by Semgrep and
a few other projects developed at r2c to monitor and profile
functions.
"
  )
  (depends
    (ocaml (>= "4.13.0"))
    (dune (>= "3.2.0" ))
    (commons (>= "1.5.5"))
    (process_limits (>= "1.5.5"))
  )
)
(package
  (name tracing)
  (version "1.5.5")
  (synopsis "Small library to help trace code")
  (description "\
This is a small library of utilities used by Semgrep to
trace functions for the purpose of reporting them in
metrics
"
  )
  (depends
    (ocaml (>= "4.13.0"))
    (dune (>= "3.2.0" ))
    (commons (>= "1.5.5"))
  )
)
(package (name files-format)
  (synopsis "File format utilities")
  (description "Outline, source-tree, and simple-format helpers.")
)
(package (name color)
  (synopsis "Simple color library")
  (description "Color types (RGB, emacs colors) and conversion utilities.")
)
(package (name geometry)
  (synopsis "Basic geometry types")
  (description "Points, rectangles, and geometric utilities.")
)
(package (name visualization)
  (synopsis "Treemap visualization library")
  (description "Treemap layout algorithms and plotting utilities.")
)

;; python-str-repr has its own dune-project in python-str-repr/
