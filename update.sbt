val warn = false

ThisBuild / update / evictionWarningOptions := EvictionWarningOptions.default
    .withWarnTransitiveEvictions(warn)
    .withWarnDirectEvictions(warn)
ThisBuild / update / logLevel := Level.Warn
