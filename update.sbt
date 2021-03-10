
update / evictionWarningOptions := EvictionWarningOptions.default
    // Periodically turn these back on to see if anything has changed.
    .withWarnTransitiveEvictions(false)
    .withWarnDirectEvictions(false)

update / logLevel := Level.Warn
