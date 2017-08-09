(in-package #:trurl)

(define-handler (css/main.css :content-type "text/css") ()
  (css `((body :font-family sans-serif)

	 (".grid" :overflow auto :margin 0 :padding 0 :list-style-type none)
	 (".grid .grid-row" :display block :clear both)
	 (".cell" :float left :width 18px :height 18px
		  :background-color "#eee" :margin-right 1px :margin-bottom 1px)
	 (".cell:hover" :border "1px solid red" :width 16px :height 16px)
	 (".cell .unit" :background-color "#f00" :width 100% :height 100%)
	 (".unit.box" :background-color "#0f0")
	 (".unit.line" :background-color "#00f")

	 ("#palette" :float right :width 160px :background-color "#eee" :padding 10px)
	 (".palette" :list-style-type none :margin 0 :padding 0)
	 (".palette-item" :cursor pointer :background-color "#ddd" :margin-bottom 5px :padding 5px)
	 (".palette-item.selected" :background-color "#0f0"))))
