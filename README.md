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
```clj
org.clojars.shchipts/dice-simulator {:mvn/version "1.0.0"}
```

### Gradle
```clj
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

## References

```
[1] Nordhaus, W. (2017). Revisiting the social cost of carbon. PNAS, 114(7): 1518-1523. https://doi.org/10.1073/pnas.1609244114
[2] Kriegler, E., Luderer, G., Bauer, N., Baumstark, L., Fujimori, S., Popp, A., Rogelj, J., Strefler, J., & van Vuuren, D. (2018). Pathways Limiting Warming To 1.5°C: A Tale Of Turning Around In No Time?. Philosophical Transactions A, 376: 20160457. https://doi.org/10.1098/rsta.2016.0457
[3] Friedlingstein, P., Andrew, R., Rogelj, J., Peters, G., Canadell, J., Knutti, R., Luderer, G., Raupach, M., Schaeffer, M., van Vuuren, D., & Le Quere, C. (2014). Persistent Growth of CO2 Emissions and Implications for Reaching Climate Targets. Nature Geoscience, 7: 709–715. https://doi.org/10.1038/ngeo2248
```

## Documentation

* [API docs](https://shchipts.github.io/dice-simulator/)

## License

Copyright © 2022 International Institute for Applied Systems Analysis

Licensed under [MIT](http://opensource.org/licenses/MIT)
