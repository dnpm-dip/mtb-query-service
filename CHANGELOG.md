# Changelog

## [1.0.1](https://github.com/dnpm-dip/mtb-query-service/compare/v1.0.0...v1.0.1) (2025-08-19)


### Bug Fixes

* Updated service-base, mtb-dto-model and mtb-dto-generators dependency versions ([8827e9a](https://github.com/dnpm-dip/mtb-query-service/commit/8827e9add5afdf70db26cb76207249d3c71708c0))

## 1.0.0 (2025-08-06)


### Features

* Adapted MTBResultSet implementation to refactored base ResultSet ([4092eef](https://github.com/dnpm-dip/mtb-query-service/commit/4092eefdc0cb2c9a05f4d22609e604e48a96b17b))
* Adapted to updated model: Upgraded to Scala 2.13.16 ([2cc3546](https://github.com/dnpm-dip/mtb-query-service/commit/2cc35468917b76c73ccd0634f7f5f7827bfb1116))
* Added alternative implementation of therapy responses statistics over a query result-set ([569a2bd](https://github.com/dnpm-dip/mtb-query-service/commit/569a2bdf11ac820cc5c27988f468651cd4677cbd))
* Added gene-centric handling of variants as 'gene alterations', esp. with splitting of multi-gene CNVs in individual alterations ([107a7de](https://github.com/dnpm-dip/mtb-query-service/commit/107a7debd1cddc3ee61f7670482071ad388cb1d3))
* Added negatable Variant query criteria; Refactored ranking of variant-related data (e.g. therapies by supporting variant) according to used variant query criteria ([c927bff](https://github.com/dnpm-dip/mtb-query-service/commit/c927bff79519a479fe91cd3812c14f036bf39ad3))
* Minor renaming of filter object attributes ([0341521](https://github.com/dnpm-dip/mtb-query-service/commit/0341521af3f8d39e20ec03cbc9178d343a44924c))
* Refactored consersion of MTBFilters to predcate using extension methods, elaborating on refactored PatientFilter ([45eb424](https://github.com/dnpm-dip/mtb-query-service/commit/45eb424bd80b763d618261ebe3ae68063f236b58))


### Bug Fixes

* Adaptation to cardinality changes in MTB model ([6c4bf80](https://github.com/dnpm-dip/mtb-query-service/commit/6c4bf80216672a66101b844a6e9a90bdc3836abe))
* Adapted default filter derivation to include pareant concepts ([4423373](https://github.com/dnpm-dip/mtb-query-service/commit/442337382d0d2c420ae6ee3b9ac94d2c85330e8a))
* Adapted import to refactored service base; Corrected PatientRecordRequest mapping ([44987f3](https://github.com/dnpm-dip/mtb-query-service/commit/44987f3226dbc9b6a3af327948176d2846a30c42))
* Adapted scalac linting and fixed many reported errors (mostly unused imports) ([ea7ce0e](https://github.com/dnpm-dip/mtb-query-service/commit/ea7ce0ebe886835c8bf80cf1c178a9e7216d5e1c))
* Adapted to fixed mean/median functions ([1b3e28c](https://github.com/dnpm-dip/mtb-query-service/commit/1b3e28ceb19be9c66185f040811ab81fe0883b6a))
* Adapted to refactored HttpConnector ([05515a1](https://github.com/dnpm-dip/mtb-query-service/commit/05515a13e1faacc32590c5f24e5f5555c9cdb2e6))
* Added usage of persistent store for prepared queries ([ca17e64](https://github.com/dnpm-dip/mtb-query-service/commit/ca17e646aa2dd519cc090e3eec2b775e88066570))
* Code clean-up ([e9eebcd](https://github.com/dnpm-dip/mtb-query-service/commit/e9eebcd820040b301facbeb638a4e1b20af8d5af))
* Fixed bug in conversion of MTBFilters to predicate function ([270f488](https://github.com/dnpm-dip/mtb-query-service/commit/270f488ebd9364ababb039b722e1e2cc1440d037))
* Quick-fix for logic bug in handling of variant query criteria, that led to all PatientRecords to be false positives (Note: will be refactored in clean) ([8888df7](https://github.com/dnpm-dip/mtb-query-service/commit/8888df718c39faeb1f2c9ddbc8b67babf6216a5f))
* Refactored query criteria handling ([dfd218b](https://github.com/dnpm-dip/mtb-query-service/commit/dfd218ba5fc6de2881247de90a4cbbd0a6bdb405))
