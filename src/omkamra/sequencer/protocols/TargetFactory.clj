(ns omkamra.sequencer.protocols.TargetFactory)

(defprotocol protocol
  (understands-target-descriptor? [this descriptor])
  (sanitize-target-descriptor [this descriptor])
  (make-target [this descriptor]))
