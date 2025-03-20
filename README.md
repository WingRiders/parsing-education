# Production grade code education: Aiken, Plutarch & PlutusTx

This document presents a comparison between the Aiken, Plutarch and PlutusTx versions of the parsing constracts with comparisons in terms of number of parsed requests, total fee, mem and cpu units.

## PlutusTx

| #Requests | Tx hash                                                          | Total fee | Total mem | Total cpu  |
|-----------|------------------------------------------------------------------|-----------|-----------|------------|
| 1         | e712d3e68556e1dfb3d3a86ee64dbd765bedaebd265ca612840a302603c5c8e2 | 0.953627  | 2547196   | 639934760  |
| 2         | 036ec458ba9ba6ae873941246a912223d4cc1af3c24c811e18280ccaad6545d4 | 1.134692  | 4880188   | 1226790774 |
| 4         | 7511201af041f9579ba67469740e8d1754b5e8e1fb1fbfb55e519f531c0ed83c | 1.643394  | 11470607  | 2893430270 |

PlutusTx maximally supports application of 4 requests in one tx.

## Plutarch

| #Requests | Tx hash                                                          | Total fee | Total mem | Total cpu  |
|-----------|------------------------------------------------------------------|-----------|-----------|------------|
| 1         | 28322f49db7c6c90c29ffe06bd5414127f036b5ad76e943389ba9f7ead8e8399 | 0.343575  | 297667    | 100528395  |
| 2         | f1180270fe841dad872fef46fd93e10f8d634ab0b20e2d0264458014239539a0 | 0.367277  | 538398    | 180440285  |
| 4         | 19a54912f51b9bf2fd3616911767aa016ca3a7fcac64ad1560fc4275674af8e9 | 0.416374  | 1039455   | 348071354  |
| 10        | 07e90862c39548bc7bc973b18212ab2c1040120ae137617e7d44bc1719a8a1f9 | 0.577211  | 2699390   | 913422869  |
| 20        | 0d983bab4a302efc16fa28be3ce2f819627bc2dd2957d16e3298aba09b7ce87a | 0.890437  | 5988493   | 2063869755 |
| 30        | 86a5c6567b5c2f9f3e060c5aa9c843a3df98e482f48ab677141fb6b5d7da0e29 | 1.26117   | 9930776   | 3474559590 |
| 38        | cb2b5cef07a3f6ec31de8c9fccf31e67254573e3e582f2883fc5ea29c4dc3dc7 | 1.59879   | 13554892  | 4609188277 |

Plutarch maximally supports application of 38 requests in one tx.

## Aiken

| #Requests | Tx hash                                                          | Total fee | Total mem | Total cpu  |
|-----------|------------------------------------------------------------------|-----------|-----------|------------|
| 1         | 9265f197826b710af260100884428286a7062cd2980013d46780c89310fe6daf | 0.526467  | 552087    | 183533061  |
| 2         | e4b00e4034876df06d4b1471a10b47d9cbf6a93d448ef178b712091a9dad3bf8 | 0.566483  | 997751    | 324510006  |
| 4         | c63da4ce0bbb4c16c2eebb6c61d2515d8f5e6b519fa0ffe4f67ef7e0eb692fe6 | 0.648135  | 1909531   | 612572791  |
| 10        | 95a49f7cbd3d868303cd90c409bd929066e2c4877ff34b49508550be3a490c6c | 0.906058  | 4808500   | 1525632296 |
| 20        | a320d86221ae74da20efd0cf88e2e394b1e78c4f80e2f526552b239d37e477c2 | 1.379147  | 10185539  | 3210301979 |
| 24        | f39fdfdfccc08ddaebdad711d5f1d4580c58172638e97c1f232d1d6c5daae7e1 | 1.583771  | 12527253  | 3941186197 |

Aiken maximally supports application of 24 requests in one tx.
