val warn = false

update / evictionWarningOptions := EvictionWarningOptions.default
    .withWarnTransitiveEvictions(warn)
    .withWarnDirectEvictions(warn)
