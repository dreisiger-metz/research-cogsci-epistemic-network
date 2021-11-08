
;;;;;;  ------ Situation TCS ------  ;;;;;;;
;;;;
;;;; Warm Tea in a Cup on a saucer.
;;;;

(defagent   sit-TCS    instance-agent
  "Hot Tea in a Cup on a Saucer"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
  :a-link     ((saucer-TCS 0.5)
               (on-TCS 0.5) )
  :slot1
    :type      :aspect
    :c-coref   tea-TCS
  :slot2
    :type      :aspect
    :c-coref   T-of-TCS
  :slot3
    :type      :aspect
    :c-coref   cup-TCS
  :slot4
    :type      :aspect
    :c-coref   in-TCS
)

(defagent   tea-TCS    instance-agent
  :type       (:instance  :object)
  :situation  (sit-TCS 0.3)
  :inst-of    tea
  :c-coref    (((sit-TCS . :slot1) 0.3)
               ((in-TCS . :slot1) 1.0)
               ((T-of-TCS . :slot1) 0.5) )
  :a-link     (warm-TCS 0.5)
)

(defagent   cup-TCS    instance-agent
  :type       (:instance  :object)
  :situation  (sit-TCS 0.3)
  :inst-of    cup
  :c-coref    (((sit-TCS . :slot3) 0.3)
               ((in-TCS . :slot2)  0.3)
               ((on-TCS . :slot2)  0.3)
               ((color-of-TCS-1 . :slot1) 0.2) )
  :a-link     ((white-TCS 0.2)
               (saucer-TCS 0.1) )
  :slot1   "(color-of cup-TCS white-TCS)"
    :type      :relation
    :c-coref   color-of-TCS-1
)

(defagent   saucer-TCS    instance-agent
  :type       (:instance  :object)
  :situation  (sit-TCS 0.3)
  :inst-of    saucer
  :c-coref    (((on-TCS . :slot1) 0.5)
               ((color-of-TCS-2 . :slot1) 0.2) )
  :a-link     ((white-TCS 0.2)
               (cup-TCS   0.3) )
  :slot1   "(color-of saucer-TCS white-TCS)"
    :type      :relation
    :c-coref   color-of-TCS-2
)

(defagent   T-of-TCS    instance-agent
  "(temperature-of tea-TCS warm)"
  :type      (:instance  :relation)
  :situation  (sit-TCS 0.3)
  :inst-of    temperature-of
  :c-coref    ((sit-TCS . :slot2) 0.3)
  :slot1
    :type     :aspect
    :inst-of  (temperature-of . :slot1)
    :c-coref  tea-TCS
  :slot2
    :type     :aspect
    :inst-of  (temperature-of . :slot2)
    :c-coref  warm
)

(defagent   warm-TCS    instance-agent
  :type       (:instance  :object)
  :situation  (sit-TCS 0.3)
  :inst-of    warm
  :c-coref    (T-of-TCS . :slot2)
  :a-link     (tea-TCS  0.5)
)

(defagent   in-TCS    instance-agent
  "(in tea-TCS cup-TCS)"
  :type       (:instance  :relation)
  :situation  (sit-TCS 0.3)
  :inst-of    in
  :c-coref    ((sit-TCS . :slot4) 0.3)
  :a-link     (on-TCS 0.1)
  :slot1
    :type      :aspect
    :inst-of   (in . :slot1)
    :c-coref   tea-TCS
  :slot2
    :type      :aspect
    :inst-of   (in . :slot2)
    :c-coref   cup-TCS
)

(defagent   on-TCS    instance-agent
  "(on saucer-TCS cup-TCS)"
  :type       (:instance  :relation)
  :situation  (sit-TCS 0.3)
  :inst-of    on
  :a-link     (in-TCS 0.1)
  :slot1
    :type      :aspect
    :inst-of   (on . :slot1)
    :c-coref   saucer-TCS
  :slot2
    :type      :aspect
    :inst-of   (on . :slot2)
    :c-coref   cup-TCS
)

(defagent   color-of-TCS-1    instance-agent
  "(color-of cup-TCS white-TCS)"
  :type       (:instance  :relation)
  :situation  (sit-TCS 0.2)
  :inst-of    color-of
  :c-coref    (cup-TCS . :slot1)
  :slot1
    :type     :aspect
    :inst-of  (color-of . :slot1)
    :c-coref  cup-TCS
  :slot2
    :type     :aspect
    :inst-of  (color-of . :slot2)
    :c-coref  white-TCS
)

(defagent   color-of-TCS-2    instance-agent
  "(color-of saucer-TCS white-TCS)"
  :type       (:instance  :relation)
  :situation  (sit-TCS 0.2)
  :inst-of    color-of
  :c-coref    (saucer-TCS . :slot1)
  :slot1
    :type     :aspect
    :inst-of  (color-of . :slot1)
    :c-coref  saucer-TCS
  :slot2
    :type     :aspect
    :inst-of  (color-of . :slot2)
    :c-coref  white-TCS
)

(defagent   white-TCS  instance-agent
  :type       (:instance  :object)
  :situation  (sit-TCS 0.2)
  :inst-of    white
  :c-coref    ((color-of-TCS-1 . :slot2)
               (color-of-TCS-2 . :slot2) )
  :a-link     ((cup-TCS 1.0)
               (saucer-TCS 1.0) )
)


>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defcoalition  sit-TCS
  "Warm Tea in a Cup on a Saucer."
  :head      sit-TCS                     ; 11 agents
  :members   (sit-TCS
              tea-TCS         cup-TCS         saucer-TCS
              T-of-TCS        warm-TCS
              in-TCS          on-TCS
              color-of-TCS-1  color-of-TCS-2  white-TCS
             ))

