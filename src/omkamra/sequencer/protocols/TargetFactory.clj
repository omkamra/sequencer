(ns omkamra.sequencer.protocols.TargetFactory)

(defprotocol protocol
  (understands-descriptor? [this descriptor]
    "Returns true if the target factory can interpret the passed
    target descriptor (a data structure which describes the desired
    target).")

  (sanitize-descriptor [this descriptor]
    "Converts the target descriptor into canonical form.")

  (make-target [this descriptor]
    "Creates the actual target based on information in the descriptor."))
