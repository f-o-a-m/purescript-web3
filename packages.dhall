let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220216/packages.dhall sha256:890466a5e3ed4793ee702d8df8ef85a025fbacbdfeb63c73597aef2795c06845

let foamSpace =
      https://raw.githubusercontent.com/srghma/foam.package-sets/dfbf848/packages.dhall sha256:e13b76934c18f16ac836c09625edf7aba767a6257f9fe69ec32210dd46e35053

let overrides = {=}

let additions = {=}

in  upstream // foamSpace // overrides // additions
