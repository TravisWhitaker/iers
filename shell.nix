{ tryLatest ? false
}:
let pkgs = if tryLatest
           then {url = "https://github.com/nixos/nixpkgs"; ref = "master";}
           else import ./pinned.nix;
    pinned-nixpkgs = builtins.fetchGit pkgs;
in with import pinned-nixpkgs {};
runCommand "iers-env"
{
    buildInputs =
        let thisghc = haskell.packages.ghc9122.ghcWithPackages
            (p: with p; [cabal-install haskell-language-server]);
        in [ thisghc
             binutils
           ];
} ""
