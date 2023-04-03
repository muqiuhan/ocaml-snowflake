(lang dune 3.7)

(name snowflake)

(generate_opam_files true)

(source
 (github muqiuhan/ocaml_snowflake))

(authors "Muqiu Han")

(maintainers "Muqiu Han")

(license LICENSE)

(documentation https://github.com/muqiuhan/ocaml_guid)

(package
 (allow_empty)
 (name snowflake)
 (synopsis "Snowflake algorithm implementation in OCaml")
 (description "Snowflake algorithm implementation in OCaml")
 (depends ocaml dune)
 (tags
  (snowflake guid uuid)))
