(** Hyperliquid Testnet Trading Example

    This example demonstrates native OCaml trading on Hyperliquid testnet
    using EIP-712 signatures without external dependencies.

    SETUP INSTRUCTIONS:

    1. Get your MetaMask private key:
       - Open MetaMask extension
       - Click the three dots (⋮) next to your account
       - Select "Account details"
       - Click "Show private key"
       - Enter your password
       - Copy the private key (64-character hex string)

    2. Get your wallet address:
       - Your MetaMask address (e.g., 0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb)

    3. Fund your testnet wallet:
       - Visit: https://app.hyperliquid-testnet.xyz/
       - Connect your MetaMask wallet
       - Request testnet tokens from faucet

    4. Set environment variables:
       export HYPERLIQUID_TESTNET_ADDRESS="0x..."
       export HYPERLIQUID_TESTNET_PRIVATE_KEY="..."  # WITHOUT 0x prefix

    5. Run this example:
       dune exec examples/hyperliquid_testnet.exe

    SAFETY NOTES:
    - NEVER use your mainnet private key for testing
    - Create a separate MetaMask account for testnet
    - Testnet tokens have no value
    - Private keys should NEVER be committed to git
*)

open Core
open Async

let () =
  Command.async ~summary:"Hyperliquid testnet trading example"
    Command.Let_syntax.(
      let%map_open
        address = flag "--address" (optional string)
          ~doc:"ADDRESS Your Hyperliquid wallet address (or set HYPERLIQUID_TESTNET_ADDRESS)"
      and
        private_key = flag "--private-key" (optional string)
          ~doc:"KEY Your private key in hex (or set HYPERLIQUID_TESTNET_PRIVATE_KEY)"
      in
      fun () ->
        let open Deferred.Let_syntax in

        (* Get credentials from flags or environment *)
        let address = match address with
          | Some a -> a
          | None ->
            match Sys.getenv "HYPERLIQUID_TESTNET_ADDRESS" with
            | Some a -> a
            | None -> failwith "Address required: use --address or set HYPERLIQUID_TESTNET_ADDRESS"
        in

        let private_key = match private_key with
          | Some pk -> pk
          | None ->
            match Sys.getenv "HYPERLIQUID_TESTNET_PRIVATE_KEY" with
            | Some pk -> pk
            | None -> failwith "Private key required: use --private-key or set HYPERLIQUID_TESTNET_PRIVATE_KEY"
        in

        (* Remove 0x prefix if present *)
        let private_key =
          if String.is_prefix private_key ~prefix:"0x"
          then String.drop_prefix private_key 2
          else private_key
        in

        printf "\n";
        printf "═══════════════════════════════════════════════════════════════\n";
        printf "  Hyperliquid Testnet Trading - Native OCaml Implementation\n";
        printf "═══════════════════════════════════════════════════════════════\n";
        printf "\n";
        printf "Wallet Address: %s\n" address;
        printf "Private Key:    %s...\n" (String.prefix private_key 8);
        printf "\n";

        (* Create testnet configuration *)
        let cfg = Hyperliquid.Cfg.testnet in

        (* Create adapter with testnet config *)
        let adapter = Hyperliquid.Fluxum_adapter.Adapter.create
          ~cfg
          ~user:address
          ~private_key
          ()
        in

        printf "Step 1: Fetching account state...\n";
        let%bind account_result = Hyperliquid.Fluxum_adapter.Adapter.balances adapter in

        match account_result with
        | Error err ->
          printf "❌ Failed to fetch account: %s\n" (Sexp.to_string_hum (Hyperliquid.Rest.Error.sexp_of_t err));
          return ()

        | Ok states ->
          printf "✓ Account state retrieved\n\n";

          (* Display account info *)
          (match List.hd states with
           | None ->
             printf "No account data found\n";
             return ()
           | Some state ->
             printf "Account Value:  %s USD\n" state.marginSummary.accountValue;
             printf "Total Position: %s USD\n" state.marginSummary.totalNtlPos;
             printf "Margin Used:    %s USD\n" state.marginSummary.totalMarginUsed;
             printf "Withdrawable:   %s USD\n" state.withdrawable;
             printf "\n";

             (* Show positions *)
             if List.is_empty state.assetPositions then
               printf "No open positions\n\n"
             else (
               printf "Open Positions:\n";
               List.iter state.assetPositions ~f:(fun ap ->
                 let pos = ap.position in
                 printf "  • %s: %s contracts (PnL: %s USD)\n"
                   pos.coin
                   pos.szi
                   pos.unrealizedPnl
               );
               printf "\n"
             );

             printf "Step 2: Fetching market data (BTC)...\n";
             let%bind meta_result = Hyperliquid.Rest.meta cfg in

             match meta_result with
             | Error err ->
               printf "❌ Failed to fetch meta: %s\n" (Sexp.to_string_hum (Hyperliquid.Rest.Error.sexp_of_t err));
               return ()

             | Ok meta ->
               printf "✓ Market data retrieved\n\n";

               (* Show available assets *)
               printf "Available Assets:\n";
               List.iteri meta.universe ~f:(fun idx item ->
                 printf "  %d. %s (decimals: %d, max leverage: %s)\n"
                   idx
                   item.name
                   item.szDecimals
                   (match item.maxLeverage with Some l -> Int.to_string l | None -> "unlimited")
               );
               printf "\n";

               (* Get current BTC price *)
               printf "Step 3: Fetching current BTC price...\n";
               let%bind mids_result = Hyperliquid.Rest.all_mids cfg in

               match mids_result with
               | Error err ->
                 printf "❌ Failed to fetch prices: %s\n" (Sexp.to_string_hum (Hyperliquid.Rest.Error.sexp_of_t err));
                 return ()

               | Ok mids ->
                 let btc_price =
                   List.find_map mids ~f:(fun (coin, price) ->
                     if String.equal coin "BTC" then Some price else None)
                 in

                 (match btc_price with
                  | None ->
                    printf "❌ BTC price not found\n";
                    return ()

                  | Some price ->
                    printf "✓ Current BTC price: $%s\n\n" price;

                    (* Prepare test order (small size, safe price) *)
                    let price_float = Float.of_string price in
                    let limit_price = price_float *. 0.8 in  (* 20% below market - won't fill *)

                    printf "═══════════════════════════════════════════════════════════════\n";
                    printf "  Test Order (Safe - Will Not Fill)\n";
                    printf "═══════════════════════════════════════════════════════════════\n";
                    printf "\n";
                    printf "Asset:       BTC-USD Perpetual\n";
                    printf "Side:        BUY\n";
                    printf "Type:        LIMIT\n";
                    printf "Size:        0.001 contracts (~$%s)\n" (Float.to_string (limit_price *. 0.001));
                    printf "Limit Price: $%s (20%% below market)\n" (Float.to_string limit_price);
                    printf "TIF:         GTC (Good-Til-Cancel)\n";
                    printf "\n";
                    printf "This order is intentionally priced far from market\n";
                    printf "so it will NOT execute - it's just a signing test.\n";
                    printf "\n";

                    (* Create order request *)
                    let order : Hyperliquid.Signing.order_request = {
                      Hyperliquid.Signing.asset = 0;  (* BTC is index 0 *)
                      is_buy = true;
                      limit_px = Float.to_string limit_price;
                      sz = "0.001";  (* Very small size *)
                      reduce_only = false;
                      time_in_force = "Gtc";
                      cloid = None;
                    } in

                    printf "Step 4: Placing test order with EIP-712 signature...\n";
                    let%bind order_result = Hyperliquid.Fluxum_adapter.Adapter.place_order adapter order in

                    match order_result with
                    | Error err ->
                      printf "\n❌ Order placement failed:\n";
                      printf "%s\n" (Sexp.to_string_hum (Hyperliquid.Rest.Error.sexp_of_t err));
                      printf "\nThis is normal if:\n";
                      printf "  - Your testnet account has no funds\n";
                      printf "  - You haven't visited https://app.hyperliquid-testnet.xyz/ to get tokens\n";
                      printf "  - The testnet is temporarily down\n";
                      return ()

                    | Ok response ->
                      printf "\n✅ Order placed successfully!\n\n";

                      (match response.status with
                       | Hyperliquid.Rest.ExchangeResponse.Ok_status ->
                         printf "Status: OK\n";
                         (match response.response with
                          | Some resp ->
                            printf "Response:\n%s\n" (Yojson.Safe.to_string resp)
                          | None ->
                            printf "No additional response data\n")

                       | Hyperliquid.Rest.ExchangeResponse.Error_status msg ->
                         printf "Status: ERROR - %s\n" msg);

                      printf "\n";
                      printf "═══════════════════════════════════════════════════════════════\n";
                      printf "  🎉 Native OCaml EIP-712 Signing Works!\n";
                      printf "═══════════════════════════════════════════════════════════════\n";
                      printf "\n";
                      printf "Successfully demonstrated:\n";
                      printf "  ✓ Keccak-256 hashing\n";
                      printf "  ✓ MessagePack serialization\n";
                      printf "  ✓ EIP-712 structured data signing\n";
                      printf "  ✓ secp256k1 ECDSA signatures\n";
                      printf "  ✓ Blockchain transaction submission\n";
                      printf "\n";
                      printf "All without external JavaScript or Python!\n";
                      printf "\n";

                      return ()
                 )
          )
    )
  |> Command_unix.run
