## 0.9.0 [2020.10.01]
* Only serve static files on `HEAD` or `GET` requests.

## 0.8.3 [2019.10.20]
* Add `Options`, `staticWithOptions`, `staticPolicyWithOptions`, and `unsafeStaticPolicyWithOptions`.
* Parameterize Middleware with options allowing custom file name to MIME type mapping.

## 0.8.2 [2018.04.07]
* Remove unused test suite.

## 0.8.1
* Add `Semigroup Policy` instance
* Replace dependencies on `base16-bytestring` and `cryptohash` with the more
  modern `memory` and `cryptonite` packages, respectively [myfreeweb]

## 0.8.0
* The `mime-types` library is now used to lookup MIME types from extensions.
  As a result, some extensions now map to different MIME types. They are:

  Extension | `wai-middleware-static`       | `mime-types` |
  --------- | ----------------------------- | ------------ |
  `class`   | `application/octet-stream`    | `application/java-vm`
  `dtd`     | `text/xml`                    | `application/xml-dtd`
  `jar`     | `application/x-java-archive`  | `application/java-archive`
  `js`      | `text/javascript`             | `application/javascript`
  `ogg`     | `application/ogg`             | `audio/ogg`
  `ttf`     | `application/x-font-truetype` | `application/x-font-ttf`

* Exposed `getMimeType` function [Shimuuar]

## 0.7.0.1
* Fixed Windows build (by replacing `unix` dependency with equivalent `directory`
  function)

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
