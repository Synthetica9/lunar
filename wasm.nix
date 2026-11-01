{ rust-bin, wasm-bindgen-cli, rustPlatform }:

let
  rust = (rust-bin.fromRustupToolchainFile ./rust-toolchain.toml).override {extensions = ["rust-src"];};
in

rustPlatform.buildRustPackage {
  pname = "lunar";
  version = "0.4.0";

  src = ./.;

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "bullet_core-0.1.0" = "sha256-DoLcLcf9c2rImbsMHpqnvbyJq0/X57gyBQOf9fSF980=";
    };
  };

  nativeBuildInputs = [
    rust
    wasm-bindgen-cli
  ];


  # https://wasm-bindgen.github.io/wasm-bindgen/examples/raytrace.html
  buildPhase = ''
    export RUSTFLAGS='-C target-feature=+atomics'
    cargo build \
        --no-default-features \
        --lib \
        --features=wasm \
        -Z build-std=panic_abort,std \
        --target=wasm32-unknown-unknown

    echo 'Creating out dir...'
    mkdir -p $out/src;

    echo 'Generating wasm module...'
    wasm-bindgen \
        --target web \
        --out-dir $out/src \
        --keep-debug \
        target/wasm32-unknown-unknown/debug/lunar.wasm;
  '';
  checkPhase = "echo 'Skipping checkPhase'";
  installPhase = "echo 'Skipping installPhase'";
}
