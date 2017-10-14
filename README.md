[![Hackage](https://img.shields.io/hackage/v/data-diverse-lens.svg)](https://hackage.haskell.org/package/data-diverse-lens)
[![Build Status](https://secure.travis-ci.org/louispan/data-diverse-lens.png?branch=master)](http://travis-ci.org/louispan/data-diverse-lens)

Provides "Iso"s & 'Len's for "Data.Diverse.Many" and 'Prism's for "Data.Diverse.Which".

Refer to [ManySpec.hs](https://github.com/louispan/data-diverse-lens/blob/master/test/Data/Diverse/Lens/ManySpec.hs) and [WhichSpec.hs](https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/Lens/WhichSpec.hs) for example usages.


# Changelog

* 0.5.0.0
  - min bounds: data-diverse >=1.2, lens >= 4.15.2
  - Rerranged type variable for xxxL and xxxN functions so that the
    @x@ inferrred from label @l@ or index @n@ is after @proxy@.
    - This affects `item[L|N]`, `item[L|N]'`, `replace[L|N]'`, `facet[L|N]`
    - Same change was made in data-diverse-1.2.0.0

* 0.4.0.1
  - included data-diverse 1.0 in the upper bounds

* 0.4.0.0
  - Changed type variable ordering of 'itemL' and 'itemL', so it's consistently 'x', 'y', then 'xs'

* 0.3.0.0
  - Changed type variable ordering of 'facetL' and 'facetN', so it's consistently 'x' then 'xs'

* pre 0.3.0.0
  - Initial version represented as (Int, Data.Map Int Any)
