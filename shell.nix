let pinned-nixpkgs = builtins.fetchGit
    {
        url = "https://github.com/nixos/nixpkgs";
        ref = "master";
        rev = "3d546fab4ea62f021976ef0a9991bb7ad9f35d33";
    };
in with import pinned-nixpkgs {};
runCommand "data-compat-env"
{
    buildInputs =
        let thisghc = haskell.packages.ghc8104.ghcWithPackages
            (p: [ p.cabal-install
                  p.ghcid
                ]);
        in [ thisghc
             binutils
           ];
} ""
