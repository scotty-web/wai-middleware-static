## 0.7.0.1
* Lower `unix` lower bounds to allow building with GHC 7.6

## 0.7.0.0
* Implement caching [agrafix]
* Include mp4 and ogv mime_types [DrBoolean]
* Dependency updates for ghc 7.10 [DougBurke]

## 0.6.0.1

* Update links to new wai-middleware-static github/issue tracker.

* Bump upper bound for `text`

## 0.6.0

* Update to wai 3.0

## 0.5.0.1

* Bump upper bound for `mtl`

## 0.5.0.0

* Add `isNotAbsolute` policy and change `static` and `staticPolicy` to
  use `noDots` and `isNotAbsolute` policies by default. (Thanks to Nick Hibberd!)

* Add `unsafeStaticPolicy`, which behaves as the old insecure `staticPolicy` behaved.

* Add changelog
