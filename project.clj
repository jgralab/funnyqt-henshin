(defproject funnyqt-henshin "0.1.1"
  :description "A transformation from Henshin transformation models to FunnyQT
  transformations."
  :url "https://github.com/jgralab/funnyqt-henshin"
  :license {:name "GNU General Public License, Version 3 (or later)"
            :url "http://www.gnu.org/licenses/gpl.html"
            :distribution :repo}
  :dependencies [[funnyqt "0.32.2"]
                 [henshin "1.0.0"]]
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ^:replace [])
