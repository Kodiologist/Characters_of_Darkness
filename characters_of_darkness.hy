;; ---------------------------------------------------------
;; * Definitions
;; ---------------------------------------------------------

;; ** For all characters

(setv stats (dict
  :Attributes (dict
    :minimum 1
    :priorities [8 7 6]
    :groups (dict
      :Mental #("Intelligence" "Wits" "Resolve")
      :Physical #("Strength" "Dexterity" "Stamina")
      :Social #("Presence" "Manipulation" "Composure")))
  :Skills (dict
    :minimum 0
    :priorities [11 7 4]
    :groups (dict
      :Mental (.split "Academics Computer Crafts Investigation Medicine Occult Politics Science")
      :Physical (.split "Athletics Brawl Drive Firearms Larceny Stealth Survival Weaponry")
      :Social (+ ["Animal Ken"] (.split "Empathy Expression Intimidation Persuasion Socialize Streetwise Subterfuge"))))))

(setv resistance-attributes #("Resolve" "Stamina" "Composure"))

;; ** Mage

(setv paths (dict
  :Acanthus (dict :r-gross "Time"   :r-subtle "Fate"   :inf "Forces")
  :Mastigos (dict :r-gross "Space"  :r-subtle "Mind"   :inf "Matter")
  :Moros    (dict :r-gross "Matter" :r-subtle "Death"  :inf "Spirit")
  :Obrimos  (dict :r-gross "Forces" :r-subtle "Prime"  :inf "Death")
  :Thyrsus  (dict :r-gross "Life"   :r-subtle "Spirit" :inf "Mind")))
(setv orders ["None (Nameless)" "Adamantine Arrow" "Guardians of the Veil" "Mysterium" "Silver Ladder" "Free Council" "Seers of the Throne"])
(setv arcana (sorted (sfor  d (.values paths)  x (.values d)  x)))
(setv starting-arcana-dots 6)
(setv mana-by-gnosis {
   1 (dict :max 10 :per-turn 1)
   2 (dict :max 11 :per-turn 2)
   3 (dict :max 12 :per-turn 3)
   4 (dict :max 13 :per-turn 4)
   5 (dict :max 15 :per-turn 5)
   6 (dict :max 20 :per-turn 6)
   7 (dict :max 25 :per-turn 7)
   8 (dict :max 30 :per-turn 8)
   9 (dict :max 50 :per-turn 10)
  10 (dict :max 75 :per-turn 15)})

;; ** The initial build

(setv initial-build (dict
  :stats (dfor
    [division d] (.items stats)
    division (dfor
      [p [g attrs]] (zip (:priorities d) (.items (:groups d)))
      g [p (dfor  a attrs   a (:minimum d))]))
  :splat (dict
    :name "None (mortal)"
    :Path "Acanthus"
    :Order "None (Nameless)"
    :arcana (dfor  a arcana  a 0)
    :Gnosis 1
    :resistance-bonus "Resolve")))

;; ---------------------------------------------------------
;; * Helpers
;; ---------------------------------------------------------

(defclass ElementMaker []
  "Create an HTML element, set attributes, and add children."
  (defn __getattr__ [self element-name]
    (defn f [#* children [do None] #** attrs]
      (setv x (hy.I.js.document.createElement element-name))
      (for [c children  :if (is-not c None)]
        (.appendChild x (if (isinstance c str)
          (hy.I.js.document.createTextNode c)
          c)))
      (for [[k v] (.items attrs)  :if (is-not v None)]
        (.setAttribute x k v))
      (when do
        (pys "do(x)"))
      x)
    (setattr E element-name f)
    f))
(setv E (ElementMaker))

;; ---------------------------------------------------------
;; * Create the form
;; ---------------------------------------------------------

(setv listeners None)

(defn import-build [build]
  "Regenerate all the form fields, using `build` to set field values
  and show validation warnings."

  (import
    js
    pyodide.ffi [create-proxy])
  
  (global listeners)
  (when (is listeners None)
    (setv listeners (dict
      :on-field-edit (create-proxy on-field-edit)
      :generate (create-proxy print-stringification))))

  (defn select-box [name options current #** kwargs]
    (E.select :name name
      #* (gfor
        option options
        (E.option (str option) :value (str option)))
      :do (fn [x]
        (setv x.value (str current))
        (.addEventListener x "input" (:on-field-edit listeners)))
      #** kwargs))

  (defn number-inputs-list [dictionary [extra-label {}]]
    (E.ul #* (gfor
      [name value] (.items dictionary)
      (E.li
        (E.input
          :name name
          :do (fn [x]
            (setv x.value (if value (str value) ""))
            (.addEventListener x "input" (:on-field-edit listeners))))
        name
        " "
        (when (in name extra-label)
          (E.i (get extra-label name)))))))

  (setv mage-path (:Path (:splat build)))
  (setv arcs (:arcana (:splat build)))
  (setv r-gross (get paths mage-path "r_gross"))
  (setv r-subtle (get paths mage-path "r_subtle"))
  (setv inferior (get paths mage-path "inf"))

  (setv container (js.document.getElementById "chargen"))
  (setv container.textContent "")

  (.appendChild container (E.div

    ; Stats fields
    #* (gfor
      [division groups] (.items (:stats build))
      (E.div
        (E.h2 division)
        #* (gfor
          [g [priority attrs]] (.items groups)
          :setv total (sum (.values attrs))
          (E.div :id f"{division}-{g}"
            (E.span (str total) :class (cond
              (> total priority) "forbidden"
              (< total priority) "unspent"
              True "ok"))
            " / "
            (select-box
              f"{division}-priority-{g}"
              (get stats division "priorities")
              priority
              :class (if
                (=
                  (sorted (lfor [p _] (.values groups) p))
                  (sorted (get stats division "priorities")))
                "ok"
                "forbidden"))
            g
            (number-inputs-list attrs)))))

    ; The splat selector
    (E.h2 "Splat " (select-box
      "splat-name"
      ["None (mortal)" "Mage"]
      (:name (:splat build))))

    (E.div :class (when (!= (:name (:splat build)) "Mage") "inapplicable")
      ; Path and Order
      (E.div "Path: " (select-box
        "mage-path"
        paths
        mage-path))
      (E.div "Order: " (select-box
        "mage-order"
        orders
        (:Order (:splat build))))
      ; Arcana
      (E.div :id "arcana"
        (do
          (setv total (sum (.values arcs)))
          (E.span (str total) :class (cond
            (> total starting-arcana-dots) "forbidden"
            (< total starting-arcana-dots) "unspent"
            True "ok")))
        f" / {starting-arcana-dots} Arcana dots"
        (when (not (and
            ; "She may only have one Arcanum at three dots"
            (<= (sum (gfor  n (.values arcs)  (= n 3))) 1)
            ; "Three to five of her starting dots must be in her Ruling Arcana"
            (<=
              3
              (+ (get arcs r-gross) (get arcs r-subtle))
              5)
            ; "both Ruling Arcana must have at least one dot"
            (>= (get arcs r-gross) 1)
            (>= (get arcs r-subtle) 1)
            ; "None of her starting dots can go to her Inferior Arcana"
            (= (get arcs inferior) 0)))
          (E.span
            " — "
            (E.span "illegal starting set for your Path" :class "forbidden"))))
      (number-inputs-list
        arcs
        :extra-label {
          r-gross "(ruling)"
          r-subtle "(ruling)"
          inferior "(inferior)"})
      ; Gnosis
      (E.div "Gnosis: " (select-box
        "mage-gnosis"
        (list (range 1 11))
        (:Gnosis (:splat build))))
      ; Resistance Attribute
      (E.div "Resistance Bonus: " (select-box
        "mage-resistance-bonus"
        resistance-attributes
        (:resistance-bonus (:splat build)))))

    ; The "Generate" button
    (E.h2 "Output")
    (E.div (E.button "Generate ASCII Character Sheet") :do (fn [x]
      (.addEventListener x "click" (:generate listeners))))
    ; The output box
    (E.div (E.textarea :id "stringified")))))

(defn on-field-edit [event]
  (import js)
  (setv scroll-pos js.window.pageYOffset)
  (import-build (read-fields))
  (.scrollTo js.window {"top" scroll-pos}))

;; ---------------------------------------------------------
;; * Process form inputs
;; ---------------------------------------------------------

(defn read-fields []
  "Read values from the HTML fields and return a build dictionary."

  (import js)

  (defn s [x]
    (.strip (. (js.document.getElementsByName x) [0] value)))
  (defn N [x]
    (setv x (s x))
    (cond
      (= x "")     0
      (.isdigit x) (int x)
      True         0))

  (dict
    :stats (dfor
      [division d] (.items stats)
      division (dfor
        [g attrs] (.items (:groups d))
        g [
          (N f"{division}-priority-{g}")
          (dfor  a attrs  a (N a))]))
    :splat (dict
      :name (s "splat-name")
      :Path (s "mage-path")
      :Order (s "mage-order")
      :arcana (dfor  a arcana  a (N a))
      :Gnosis (N "mage-gnosis")
      :resistance-bonus (s "mage-resistance-bonus"))))

;; ---------------------------------------------------------
;; * Create character sheets
;; ---------------------------------------------------------

(defn calculate-character [build]
  "Convert a build to a character, resolving all computable values."

  (setv char (hy.I.copy.deepcopy build))
  (setv size 5)
  (setv s (dfor
    d (.values (:stats char))
    [_ attrs] (.values d)
    [k v] (.items attrs)
    k v))

  (setv (get char "advantages") (dict 
    :Willpower-max (+ (:Resolve s) (:Composure s))
    :Size size
    :Health (+ size (:Stamina s))
    :Speed (+ 5 (:Strength s) (:Dexterity s))
    :Initiative (+ (:Dexterity s) (:Composure s))
    :Defense (+
      (min (:Wits s) (:Dexterity s))
      (:Athletics s))))

  (when (= (:name (:splat char)) "Mage")
    (setv r (:resistance-bonus (:splat char)))
    (for [[_ d] (.values (:Attributes (:stats char)))]
      (when (in r d)
        (+= (get d r) 1))))

  char)

(defn stringify-character [build]
  "Return an ASCII character sheet."

  (setv char (calculate-character build))
  (setv ad (:advantages char))

  (setv column-width 15)
  (setv hr (* "-" column-width))
  (defn pad-list [n l]
    (+ l (* (- n (len l)) [(* " " column-width)])))

  (setv stat-blocks (lfor
    g (:groups (:Attributes stats))
    [
      (.format "{:{}}" g column-width)
      hr
      #* (sum :start [] (gfor
        [division groups] (.items (:stats char))
        [
          #* (pad-list (len (get stats division "groups" "Mental")) (lfor
            [g2 [_ gd]] (.items groups)
            :if (= g2 g)
            [name value] (.items gd)
            :if value
            (.format "{:{}} {}" name (- column-width 2) value)))
          hr]))]))

  (setv splat-name (:name (:splat char)))
  (setv mana (when (= splat-name "Mage")
    (get mana-by-gnosis (:Gnosis (:splat char)))))

  (+
    f"- Health {(:Health ad)} (0 agg, 0 let, 0 bas)\n"
    (if (not mana) ""
      f"- Mana {(:max mana)} / {(:max mana)} (max per turn: {(:per-turn mana)})\n")
    f"- Willpower {(:Willpower-max ad)} / {(:Willpower-max ad)}\n"
    "\n"
    f"- Initiative {(:Initiative ad)}\n"
    f"- Speed {(:Speed ad)}\n"
    f"- Defense {(:Defense ad)}\n"
    f"- Armor 0/0\n"
    f"- Size {(:Size ad)}\n"
    "\n"
    (.join "\n" (gfor
      line (zip #* stat-blocks)
      (.format "| {} |" (.join " | " line))))
    (if (!= splat-name "Mage") "" (+
      "\n\n"
      f"Gnosis {(:Gnosis (:splat char))}\n"
      f"Path: {(:Path (:splat char))} | Order: {(:Order (:splat char))}\n"
      (.join "\n" (gfor
        [name value] (sorted (.items (:arcana (:splat char)))
          :key (fn [x] #((- (get x 1)) (get x 0))))
        :if value
        f"- {name} {value}"))))))

(defn print-stringification [event]
  "Insert a character sheet for this build into the HTML."
  (setv (. (hy.I.js.document.getElementById "stringified") value)
    (stringify-character (read-fields))))
