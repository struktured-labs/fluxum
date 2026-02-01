(lang dune 3.16)
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
  (dune (>= 3.16))
  (core (>= v0.17.0))
  (async (>= v0.17.0))
  (cohttp (>= 5.3.0))
  (cohttp-async (>= 5.3.0))
  (yojson (>= 2.2.0))
  (ppx_jane (>= v0.17.0))
  (ppx_deriving_yojson (>= 3.9.0))
  (digestif (>= 1.2.0))
  (uri (>= 4.4.0))
  (base64 (>= 3.5.1))
  (hex (>= 1.5.0))
  (curl (>= 0.9.0))
  csvfields
  (ppx_csv_conv (>= v0.17.0))
  ppx_make
  (async_websocket (>= v0.17.0))
  ctypes
  ctypes-foreign
  mirage-crypto
  (secp256k1 (>= 0.4.4))
  (msgpck (>= 1.7))
  (odoc :with-doc)))
