{ mkDerivation, base, base64-bytestring, basement, binary
, bytestring, containers, cryptonite, directory, filepath
, foundation, hashable, memory, mtl, process, regex-base
, regex-tdfa-text, stdenv, tasty, tasty-discover, tasty-hspec
, tasty-hunit, tasty-quickcheck, text, unix, unordered-containers
}:
mkDerivation {
  pname = "hnix-store-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base basement binary bytestring containers cryptonite directory
    filepath foundation hashable memory mtl regex-base regex-tdfa-text
    text unix unordered-containers
  ];
  testHaskellDepends = [
    base base64-bytestring binary bytestring containers directory
    process tasty tasty-discover tasty-hspec tasty-hunit
    tasty-quickcheck text
  ];
  homepage = "https://github.com/haskell-nix/hnix-store";
  description = "Core effects for interacting with the Nix store";
  license = stdenv.lib.licenses.asl20;
}
