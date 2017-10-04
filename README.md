[![Hackage](https://img.shields.io/hackage/v/data-diverse-lens.svg)](https://hackage.haskell.org/package/data-diverse-lens)
[![Build Status](https://secure.travis-ci.org/louispan/data-diverse-lens.png?branch=master)](http://travis-ci.org/louispan/data-diverse-lens)

Provides "Iso"s & 'Len's for "Data.Diverse.Many" and 'Prism's for "Data.Diverse.Which".

Refer to [ManySpec.hs](https://github.com/louispan/data-diverse-lens/blob/master/test/Data/Diverse/Lens/ManySpec.hs) and [WhichSpec.hs](https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/Lens/WhichSpec.hs) for example usages.


# Changelog

* pre 0.3.0.0
  - Initial version represented as (Int, Data.Map Int Any)

* 0.3.0.0
  - Changed type variable ordering of 'facetL' and 'facetN', so it's consistently 'x' then 'xs'
