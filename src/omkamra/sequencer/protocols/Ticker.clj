(ns omkamra.sequencer.protocols.Ticker)

(defprotocol protocol
  (start [this])
  (stop [this])
  (tick [this])
  (bpm! [this new-bpm]))
