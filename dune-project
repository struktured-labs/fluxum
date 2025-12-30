(lang dune 3.11)
(name fluxum)

(generate_opam_files true)

(source (github struktured-labs/fluxum))
(license MIT)
(authors "Carmelo Piccione")
(maintainers "carmelo.piccione@gmail.com")

(package
 (name fluxum)
 (synopsis "OCaml cryptocurrency exchange trading library")
 (description "Multi-exchange trading library supporting Binance, Kraken, Gemini, MEXC, Hyperliquid, Coinbase, and Bitrue with WebSocket and REST APIs")
 (depends
  (ocaml (>= 5.2.0))
  (dune (>= 3.11))
  (core (>= v0.16.0))
  (async (>= v0.16.0))
  (cohttp (>= 5.0.0))
  (cohttp-async (>= 5.0.0))
  (yojson (>= 2.0.0))
  (ppx_jane (>= v0.16.0))
  (ppx_deriving_yojson (>= 3.7.0))
  (digestif (>= 1.1.0))
  (uri (>= 4.2.0))
  (base64 (>= 3.5.0))
  (hex (>= 1.5.0))
  (curl (>= 0.9.0))
  csvfields
  (odoc :with-doc)))
