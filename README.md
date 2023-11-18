# BayesianRenewalModels.Rpkg
R package wrapper for Julia package `BayesianRenewalModels.jl`

Note that the julia package is linked via submodule, meaning the Julia source code is part of the R package installation.
When cloning a repository, linked submodules are not included unless the `--recurse-submodules` flag is set.
This is handled automatically when installing via `devtools::install_github`.
