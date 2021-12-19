(ns omkamra.sequencer.protocols.Target)

(defprotocol protocol
  (start [this]
    "Starts any machinery associated with the target (e.g. an audio
    engine). Should be idempotent.")

  (stop [this]
    "Stop any associated machinery associated with the target (e.g. an
    audio engine). Should be idempotent.")

  (restart [this]
    "Restarts any associated machinery associated with the target (e.g. an audio engine).")

  (get-default-bindings [this]
    "Returns target-specific default bindings or nil if there are no
    such bindings.")

  (compile-pattern-form [this form]
    "If form is recognized as a target-specific pattern form,
    translates it into a pattern expression and returns the
    result. Otherwise returns nil.")

  (compile-pattern-expr [this pattern]
    "If pattern is recognized as a target-specific pattern expression,
    compiles it into a pattern transformer and returns the
    result. Otherwise returns nil.")

  (compile-bind-expr [this k expr]
    "If expr is recognized as a target-specific bind expression,
    compiles it into a binding resolver and returns the
    result. Otherwise returns nil.")

  (resolve-binding [this k v]
    "If k is recognized as a target-specific binding key, returns the
    binding value that corresponds to v. Otherwise returns nil."))
