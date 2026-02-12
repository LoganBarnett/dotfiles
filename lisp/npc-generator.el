;;; npc-generator.el --- D&D NPC Generator using transient -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Logan Barnett

;; Author: Logan Barnett <logustus@gmail.com>
;; Keywords: games dnd

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate random NPCs for D&D games with a persistent transient UI.  Options
;; that are unset will be randomly generated.  The UI stays open after
;; generation so you can quickly create multiple NPCs.

;;; Code:

(require 'transient)

;; Data for random generation.

(defvar npc/species-options
  '("aarakocra" "aasimar" "bugbear" "centaur" "changeling" "dragonborn"
    "dwarf" "elf" "firbolg" "genasi" "gith" "gnome" "goblin" "goliath"
    "half-elf" "half-orc" "halfling" "hobgoblin" "human" "kalashtar"
    "kenku" "kobold" "lizardfolk" "loxodon" "minotaur" "orc" "satyr"
    "shifter" "tabaxi" "tiefling" "tortle" "triton" "warforged" "yuan-ti")
  "Available species for NPCs (D&D 5e).")

(defvar npc/sex-options
  '("male" "female" "nonbinary")
  "Available sexes for NPCs.")

(defvar npc/names-dwarf-male
  '("Thorin" "Balin" "Dwalin" "Gimli" "Gloin" "Bombur" "Bofur")
  "Sample dwarf male names.")

(defvar npc/names-dwarf-female
  '("Dis" "Kathra" "Runil" "Torbera" "Dagnal" "Ilde")
  "Sample dwarf female names.")

(defvar npc/names-human-male
  '("Aric" "Brendan" "Cedric" "Dorian" "Elric" "Finn" "Gareth")
  "Sample human male names.")

(defvar npc/names-human-female
  '("Aria" "Brenna" "Celia" "Diana" "Elara" "Fiona" "Gwen")
  "Sample human female names.")

(defvar npc/surnames-dwarf
  '("Ironfist" "Stonehelm" "Fireforge" "Battlehammer" "Bronzebeard"
    "Deepdelver" "Ironshield" "Steelstrike" "Goldseeker" "Hammerfall")
  "Sample dwarf surnames.")

(defvar npc/surnames-elf
  '("Moonwhisper" "Starweaver" "Silverleaf" "Nightbreeze" "Sunfire"
    "Wintermoon" "Dawnsinger" "Shadowstep" "Windrunner" "Forestwalk")
  "Sample elf surnames.")

(defvar npc/surnames-human
  '("Smith" "Fletcher" "Cooper" "Miller" "Baker" "Thatcher" "Weaver"
    "Mason" "Wright" "Carter" "Potter" "Taylor" "Turner" "Ward"
    "Hunter" "Archer" "Fisher" "Shepherd" "Forester" "Gardner")
  "Sample human surnames (craft-based).")

(defvar npc/surnames-generic
  '("Brightblade" "Swiftfoot" "Strongarm" "Keeneye" "Ironheart"
    "Stormborn" "Wildmane" "Quickstep" "Boldshield" "Truearrow")
  "Generic fantasy surnames for species without specific lists.")

(defvar npc/personality-traits
  '("irritable" "loyal" "greedy" "brave" "cowardly" "wise" "foolish"
    "honest" "deceitful" "kind" "cruel" "curious" "apathetic")
  "Available personality traits.")

(defvar npc/professions
  '("alchemist" "apothecary" "architect" "armorer" "artist" "assassin"
    "baker" "banker" "barber" "bard" "bartender" "beggar" "blacksmith"
    "bookbinder" "brewer" "butcher" "carpenter" "cartographer" "chandler"
    "cleric" "cobbler" "cook" "courtesan" "dancer" "diplomat" "doctor"
    "druid" "enchanter" "engineer" "entertainer" "explorer" "farmer"
    "fisherman" "fletcher" "forester" "fortuneteller" "gambler" "glassblower"
    "goldsmith" "grave-digger" "guard" "guide" "herbalist" "hermit" "hunter"
    "innkeeper" "jeweler" "knight" "laborer" "librarian" "locksmith"
    "lumberjack" "mason" "merchant" "messenger" "miller" "miner" "monk"
    "musician" "navigator" "noble" "painter" "performer" "philosopher"
    "physician" "pilgrim" "pirate" "potter" "priest" "ranger" "sailor"
    "scholar" "scribe" "sculptor" "shepherd" "shipwright" "shopkeeper"
    "silversmith" "soldier" "spy" "stableman" "stonemason" "tailor"
    "tanner" "tax-collector" "thief" "tinker" "torturer" "town-crier"
    "trader" "trapper" "undertaker" "watchman" "weaponsmith" "weaver"
    "wizard" "woodcarver")
  "Available professions.")

;; Transient infix definitions (the pickers/toggles).

(transient-define-infix npc/infix-species ()
  "Select species for NPC.  Leave unset for random."
  :class 'transient-option
  :key "s"
  :description "Species"
  :argument "--species="
  :reader (lambda (prompt initial-input history)
            (completing-read prompt npc/species-options
                             nil nil initial-input history)))

(transient-define-infix npc/infix-sex ()
  "Select sex for NPC.  Leave unset for random."
  :class 'transient-switches
  :key "x"
  :description "Sex"
  :argument-format "--sex=%s"
  :argument-regexp "\\(--sex=.+\\)"
  :choices npc/sex-options)

(transient-define-infix npc/infix-profession ()
  "Select profession for NPC.  Leave unset for random."
  :class 'transient-option
  :key "p"
  :description "Profession"
  :argument "--profession="
  :reader (lambda (prompt initial-input history)
            (completing-read prompt npc/professions
                             nil nil initial-input history)))

(transient-define-infix npc/infix-surname ()
  "Set surname for NPC.  Leave unset for random based on species."
  :class 'transient-option
  :key "S"
  :description "Surname"
  :argument "--surname="
  :reader (lambda (prompt initial-input history)
            (read-string prompt initial-input history)))

;; Main transient UI.

(transient-define-prefix npc/generator ()
  "NPC Generator - Create D&D NPCs with customizable attributes."
  ["Character Attributes"
   [("Species, Sex, Profession")
    (npc/infix-species)
    (npc/infix-sex)
    (npc/infix-profession)
    (npc/infix-surname)]
   [("Personality Traits (toggle)")
    ("-i" "Irritable" "--irritable")
    ("-l" "Loyal" "--loyal")
    ("-g" "Greedy" "--greedy")
    ("-b" "Brave" "--brave")]]
  ["Actions"
   [("Generate")
    ("RET" "Generate NPC" npc/generate :transient t)
    ("a" "Generate & add to notes" npc/generate-and-add :transient t)]
   [("Control")
    ("r" "Reset all options" npc/reset :transient t)
    ("q" "Quit" transient-quit-one)]])

;; Helper functions for parsing transient args.

(defun npc/get-arg-value (args prefix)
  "Extract value from ARGS matching PREFIX=value.
Returns nil if not found (meaning random)."
  (let ((match (seq-find (lambda (arg) (string-prefix-p prefix arg)) args)))
    (when match
      (substring match (length prefix)))))

(defun npc/random-element (list)
  "Return a random element from LIST."
  (nth (random (length list)) list))

(defun npc/generate-name (species sex)
  "Generate a random first name based on SPECIES and SEX."
  (cond
   ;; Dwarf names.
   ((and (equal species "dwarf") (equal sex "male"))
    (npc/random-element npc/names-dwarf-male))
   ((and (equal species "dwarf") (equal sex "female"))
    (npc/random-element npc/names-dwarf-female))
   ;; Human names.
   ((and (equal species "human") (equal sex "male"))
    (npc/random-element npc/names-human-male))
   ((and (equal species "human") (equal sex "female"))
    (npc/random-element npc/names-human-female))
   ;; Fallback for other combinations.
   (t "Unknown")))

(defun npc/generate-surname (species)
  "Generate a random surname based on SPECIES."
  (cond
   ((equal species "dwarf")
    (npc/random-element npc/surnames-dwarf))
   ((equal species "human")
    (npc/random-element npc/surnames-human))
   ((equal species "elf")
    (npc/random-element npc/surnames-elf))
   ;; Fallback for other species.
   (t (npc/random-element npc/surnames-generic))))

;; Action functions.

(defun npc/generate (&optional args)
  "Generate an NPC with ARGS from transient.
Options not set will be randomly generated."
  (interactive (list (transient-args 'npc/generator)))
  (let* (;; Get specified values or generate random ones.
         (species (or (npc/get-arg-value args "--species=")
                      (npc/random-element npc/species-options)))
         (sex (or (npc/get-arg-value args "--sex=")
                  (npc/random-element npc/sex-options)))
         (profession (or (npc/get-arg-value args "--profession=")
                         (npc/random-element npc/professions)))
         ;; Check toggle traits.
         (irritable (member "--irritable" args))
         (loyal (member "--loyal" args))
         (greedy (member "--greedy" args))
         (brave (member "--brave" args))
         ;; Build personality trait list.
         (traits (append
                  (when irritable '("irritable"))
                  (when loyal '("loyal"))
                  (when greedy '("greedy"))
                  (when brave '("brave"))))
         ;; Add a random trait if none specified.
         (traits (if traits
                     traits
                   (list (npc/random-element npc/personality-traits))))
         (trait-string (mapconcat #'identity traits ", "))
         ;; Generate name and surname.
         (first-name (npc/generate-name species sex))
         (surname (or (npc/get-arg-value args "--surname=")
                      (npc/generate-surname species)))
         (full-name (concat first-name " " surname)))
    ;; Display the result.
    (message "Generated NPC: %s - %s %s %s (%s)"
             full-name
             (capitalize sex)
             (capitalize species)
             (capitalize profession)
             trait-string)
    ;; Return the NPC data structure for other functions to use.
    (list :name full-name
          :first-name first-name
          :surname surname
          :species species
          :sex sex
          :profession profession
          :traits traits)))

(defun npc/generate-and-add (&optional args)
  "Generate an NPC and add it to the current org-mode buffer.
ARGS are from transient."
  (interactive (list (transient-args 'npc/generator)))
  (let ((npc (npc/generate args)))
    ;; Insert into org-mode buffer if we're in one.
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char (point-max))
        (insert (format "\n* %s\n" (plist-get npc :name)))
        (insert (format "- Species: %s\n" (capitalize (plist-get npc :species))))
        (insert (format "- Sex: %s\n" (capitalize (plist-get npc :sex))))
        (insert (format "- Profession: %s\n"
                        (capitalize (plist-get npc :profession))))
        (insert (format "- Traits: %s\n"
                        (mapconcat #'identity (plist-get npc :traits) ", ")))
        (message "Added NPC to notes!")))
    ;; Return the NPC for potential further use.
    npc))

(defun npc/reset ()
  "Reset all transient options."
  (interactive)
  ;; Clear the transient history to reset all values.
  (setq transient-history nil)
  (message "Reset all options"))

(provide 'npc-generator)
;;; npc-generator.el ends here
