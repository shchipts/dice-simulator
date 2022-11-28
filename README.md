# dice-simulator

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.shchipts/dice-simulator.svg)](https://clojars.org/org.clojars.shchipts/dice-simulator)

Simulation routines for generic DICE model (economy module)



The generic DICE economy module includes:
  * *constraints based on limiting case of massive CDR deployment*
  * *emissions quota for global-mean warming below 3°C*
  * *dynamic AFOLU emissions*



Simulation routines include:
  * net-emissions with constraints:
    \- do not exceed the lower limiting case of minimum gross FFI emissions in deep mitigation pathways and maximum capacity in massive CDR deployment (Kriegler et al. 2018),
    \- do not surpass the upper limiting case of SSP baseline,
    \- cumulative net amount of CO2 (FFI and AFOLU) realised to the atmosphere, should not exceed remaining emissions quota.  

    AFOLU values are constructed based on SSP land use emissions using linear interpolation.  
    Remaining emissions quota, measured in GtCO2, is taken from Friedlingstein et al. (2014) for 50% probability of global-mean warming below 3°C.

  * CDR emissions:
    \- linearly increase from 2015 through 2020 and follows as generalized logistic curve afterwards

  * SSP economic curves for given temperature pathway with constraints:
    \- Economy-climate and CDR pathways should not surpass the upper limiting case of SSP baseline and couple with non-negative gross GDP, total investment and consumption

## Dependency Information

To include libraries add the following to your `:dependencies`:

### Leiningen/Boot
```clj
[org.clojars.shchipts/dice-simulator "1.0.0"]
```

### Clojure CLI/deps.edn
```
org.clojars.shchipts/dice-simulator {:mvn/version "1.0.0"}
```

### Gradle
```
implementation("org.clojars.shchipts:dice-simulator:1.0.0")
```

### Maven

```xml
<dependency>
  <groupId>org.clojars.shchipts</groupId>
  <artifactId>dice-simulator</artifactId>
  <version>1.0.0</version>
</dependency>
```

## Documentation

* [API docs](https://shchipts.github.io/dice-simulator/)

## License

Copyright © 2022 International Institute for Applied Systems Analysis

Licensed under [MIT](http://opensource.org/licenses/MIT)
