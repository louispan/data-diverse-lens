[![Hackage](https://img.shields.io/hackage/v/data-diverse-lens.svg)](https://hackage.haskell.org/package/data-diverse-lens)
[![Build Status](https://secure.travis-ci.org/louispan/data-diverse-lens.png?branch=master)](http://travis-ci.org/louispan/data-diverse-lens)

Provides "Iso"s & 'Len's for "Data.Diverse.Many" and 'Prism's for "Data.Diverse.Which".

Refer to [ManySpec.hs](https://github.com/louispan/data-diverse-lens/blob/master/test/Data/Diverse/Lens/ManySpec.hs) and [WhichSpec.hs](https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/Lens/WhichSpec.hs) for example usages.

# Changelog

* 2.2.0.0
  - Renamed `SelectWith` to `MakeFrom`, split `ChooseBtween` to `ChooseBoth` and `ChooseFrom`.
  - Removed hard to remember operators ``>&&>` `*&&*` `+||+` `>||>`
  - Removed `faceted'`
  - Added `instance AsFacet Void (Which '[])` and `instance AsFacet (Which '[]) Void`

* 2.1.0.0
  - Removed profunctor variable `w` from the constraint synonyms for `Projected`/`Injected`.

* 2.0.0.1
  - Forgot to expose constraint synonyms for `Projected`/`Injected`.

* 2.0.0.0
  - Breaking change: Removed `HasProject` and `AsInject` typeclasses and changed them back to functions.
  - Added `MatchingFacet` typeclasses for polymorphic `matching` of prisms.
  - Added constraint synonyms for `Project`/`Inject`/`Projected`/`Injected`.

* 1.0.0.1
  - Fixed missing exports of the new lens classes.

* 1.0.0.0
  - Removed overlapping instances of `Data.Generics` lens
  - Using typeclass instead of plain functions so that lens can be used for other data types
    - Added default implementations for some of these typeclasses using Data.Generic typeclasses.
  - Breaking change: the xxx' version of functions are now consistently the simpler or non-polymorphic version    - This is more consistent with 'Control.Lens' as well.
    - This means the following are swapped:
      - `item`, `item'`
      - `itemL`, `itemL'`
      - `itemTag`, `itemTag'`
      - `itemN`, `itemN'`
      - `project`, `project'`
      - `projectL`, `projectL'`
      - `projectN`, `projectN'`

* 0.5.2.0
  - Added `itemTag` and `facetTag` that also tag/untags the field.
  - Added overlapping instances of `Data.Generics` lens

* 0.5.1.0
  - Added `faceted`, `injected`, `itemized`, `projected`, which is analogous to `Profunctor` `Choice` and `Strong` but using `Which` and `Many`
  - Added `+||+` (analogous to `+++` and `|||`), `>||>`; and `*&&*` (analogous to `***` and `&&&`), and `>&&>`.

* 0.5.0.0
  - min bounds: data-diverse >=1.2.1
  - Rerranged type variable for xxxL and xxxN functions so that the
    @x@ inferrred from label @l@ or index @n@ is after @proxy@.
    - This affects `item[L|N]`, `item[L|N]'`, `replace[L|N]'`, `facet[L|N]`
    - Same change was made in data-diverse-1.2.0.0

* 0.4.0.1
  - included data-diverse 1.0 in the upper bounds

* 0.4.0.0
  - Changed type variable ordering of `itemL` and `itemL`, so it's consistently `x`, `y`, then `xs`

* 0.3.0.0
  - Changed type variable ordering of `facetL` and `facetN`, so it's consistently `x` then `xs`

* pre 0.3.0.0
  - Initial version.
